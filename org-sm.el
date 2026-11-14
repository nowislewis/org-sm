;;; org-sm.el --- SuperMemo-style incremental reading for org-mode -*- lexical-binding: t -*-

;; Author: Lewis Liu
;; Package-Requires: ((emacs "29.1") (org "9.6") (fsrs "6.0"))
;; Keywords: org, spaced-repetition, incremental-reading

;;; Commentary:
;;
;; Minimal incremental reading for org-mode.  Two card types:
;;   topic  - repeated reading, A-Factor scheduling (org priority [#A]/[#B]/[#C]
;;            sets the growth rate), no grading.
;;   cloze  - memory testing with {{cloze}} markers, FSRS scheduling.
;;
;; Design rule: capture only stores raw material; refining and splitting happen
;; later, on an existing card (`org-sm-card-workbench', plus the optional AI
;; actions in org-sm-gptel.el).
;;
;; Commands:
;;   org-sm-item-mark      - mark heading as topic or cloze
;;   org-sm-item-extract   - extract region as child topic or cloze
;;   org-sm-item-dismiss   - dismiss item from SRS (tag :dismissed:, keep data)
;;   org-sm-item-undismiss - restore a dismissed item
;;   org-sm-review-start   - start review session
;;   org-sm-review-confirm - confirm topic read / advance cloze state
;;   org-sm-review-abort   - abort review session
;;   org-sm-tree           - foldable aggregated tree; filter + review in place
;;   org-sm-set-readpoint  - drop an inline read-point anchor; review jumps here
;;   org-sm-goto-source    - grep the vault for this card's source
;;   org-sm-capture                  - capture region/clipboard, then confirm with C-c C-c
;;   org-sm-capture-clipboard-notify - capture clipboard immediately and notify
;;   org-sm-card-workbench           - refine and/or split the card at point
;;
;; Minor modes:
;;   org-sm-mode             - buffer-local; {{cloze}} font-lock (use via :hook)
;;   global-org-sm-read-mode - global; binds M-z to `org-sm-capture'
;;
;; Point `org-sm-directory' at the tree holding your cards, optionally
;; `org-sm-capture-file' / `org-sm-capture-olp' at a different inbox.  See
;; README.org for setup and suggested keys.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-id)
(require 'org-macs)
(require 'fsrs)
(require 'org-sm-card)
(require 'org-sm-queue)
(require 'org-sm-source)

(declare-function org-sm-gptel-explain "org-sm-gptel")
(declare-function org-sm-neural-start "org-sm-neural")
(autoload 'org-sm-neural-start "org-sm-neural" nil t)
(autoload 'org-sm-sectionize-card "org-sm-import" nil t)
(declare-function org-sm-gptel-suggest "org-sm-gptel")
(declare-function org-sm-gptel-card-ai "org-sm-gptel")
(declare-function notifications-notify "notifications")

;;;; ---- Customization -------------------------------------------------------

(defcustom org-sm-title-max-length 40
  "Max display-column width for auto-generated heading titles."
  :type 'integer
  :group 'org-sm)

(defcustom org-sm-topic-prefix "[T] "
  "Prefix string prepended to auto-generated topic heading titles."
  :type 'string
  :group 'org-sm)

(defcustom org-sm-cloze-prefix "[C] "
  "Prefix string prepended to auto-generated cloze heading titles."
  :type 'string
  :group 'org-sm)

(defcustom org-sm-stall-review-threshold 4
  "Review count at which a childless topic is treated as stalled.
Past this the card is not being read, it is being pushed: four passes with
nothing extracted and nothing dismissed is the shape of avoidance."
  :type 'integer
  :group 'org-sm)

(defcustom org-sm-leech-again-threshold 2
  "`again' count at which a cloze is treated as a leech.
One lapse is noise; two means the card is probably testing something the
reader never encoded, so it wants rewriting rather than more repetitions."
  :type 'integer
  :group 'org-sm)

;; Defined by `define-minor-mode' at the end of this file.
(defvar org-sm-mode)

;;;; ---- Schedule helper -----------------------------------------------------

(defun org-sm--schedule (time)
  "Set SCHEDULED of current heading to TIME.
Must be called before any `org-entry-put' / `org-set-property' on the same
heading, because those functions move point into the PROPERTIES drawer; after
that `org-schedule' (called internally) would climb to the wrong heading."
  (org-schedule nil (format-time-string "%F %a %H:%M" time)))

;;;; ---- Review session state ------------------------------------------------
;; Declared early so `org-sm-item-extract' (defined above the review section)
;; can reference `org-sm--cloze-state' without a free-variable warning.

(defvar org-sm--queue nil
  "Active review queue: list of markers consumed by `org-sm--advance'.")

(defvar org-sm--review-total 0
  "Number of cards selected at the start of the current review.")

(defvar org-sm-review-start-hook nil
  "Normal hook run when an `org-sm' review session starts.")

(defvar org-sm-after-review-functions nil
  "Abnormal hook receiving a review event plist after an action.
Keys include `:action', `:card', `:old-days', `:new-days' and
optional action-specific fields such as `:rating'.")

(defvar org-sm-prompt-function #'org-sm-default-prompt
  "Function receiving review state plist to present the active card prompt.
Keys: `:card', `:position', `:cloze-state', `:prev'.")

(defvar org-sm-feedback-function #'org-sm-default-feedback
  "Function receiving a review event and returning action feedback.")

(defvar org-sm-finish-function #'org-sm-default-finish
  "Function receiving PREV and ABORTED to present session-end feedback.")

(defvar-local org-sm--mode-line nil
  "Cached review position for the current card's buffer.")

(defvar-local org-sm--header-line nil
  "Review status string displayed at the start of `header-line-format'.")
(put 'org-sm--header-line 'risky-local-variable t)

(defconst org-sm--header-line-construct
  '(org-sm--header-line (" " org-sm--header-line "   "))
  "Header-line construct prepended to `header-line-format' during review.")

(defun org-sm-set-header-line (text)
  "Display TEXT at the start of `header-line-format' in the current buffer.
Coexists with `breadcrumb-local-mode' by prepending to the existing list."
  (setq org-sm--header-line text)
  (when text
    (unless (listp header-line-format)
      (setq-local header-line-format
                  (and header-line-format (list header-line-format))))
    (add-to-list 'header-line-format org-sm--header-line-construct))
  (force-mode-line-update))

(defvar-local org-sm--cloze-state nil
  "Cloze review state for the current buffer: `hidden' or `revealed'.")

(defvar org-sm--current-card-marker nil
  "Marker identifying the exact card currently presented for review.")

(defun org-sm--current-review-card-p ()
  "Return non-nil when point belongs to the card presented by the session."
  (and (markerp org-sm--current-card-marker)
       (eq (marker-buffer org-sm--current-card-marker) (current-buffer))
       (save-excursion
         (org-back-to-heading t)
         (= (point) (marker-position org-sm--current-card-marker)))))

(defvar org-sm--current-buffer nil
  "Buffer holding the card currently being reviewed, for state cleanup.")

;;;; ---- FSRS ----------------------------------------------------------------

(defvar org-sm--scheduler nil
  "FSRS scheduler instance, initialized on first call to `org-sm-review-start'.")

(defun org-sm--ensure-scheduler ()
  "Initialize `org-sm--scheduler' if not already done."
  (unless org-sm--scheduler
    (setq org-sm--scheduler
          (fsrs-make-scheduler :desired-retention 0.9
                               :learning-steps    '((1 :minute) (10 :minute))
                               :enable-fuzzing-p  t))))

(defun org-sm--cloze-read ()
  "Build an `fsrs-card' from the current heading's properties."
  (fsrs-make-card
   :state       (or (when-let* ((s (org-entry-get nil "SRS_STATE"))) (read s)) :learning)
   :step        (if-let* ((s (org-entry-get nil "SRS_STEP"))) (string-to-number s) 0)
   :stability   (when-let* ((s (org-entry-get nil "SRS_STABILITY")))  (string-to-number s))
   :difficulty  (when-let* ((d (org-entry-get nil "SRS_DIFFICULTY"))) (string-to-number d))
   :last-review (org-entry-get nil "SRS_LAST")
   :due         (or (when-let* ((t_ (org-get-scheduled-time nil)))
                      (format-time-string "%FT%TZ" t_ "UTC0"))
                    (fsrs-now))))

(defun org-sm--put-or-delete (prop value)
  "Set PROP to VALUE, or delete it when VALUE is nil."
  (if value (org-entry-put nil prop value) (org-delete-property prop)))

(defun org-sm--cloze-write (card)
  "Persist CARD to current heading's properties and SCHEDULED."
  ;; Schedule first — org-entry-put moves point into PROPERTIES drawer.
  (org-sm--schedule (parse-iso8601-time-string (fsrs-card-due card)))
  (org-entry-put nil "SRS_STATE" (prin1-to-string (fsrs-card-state card)))
  (org-sm--put-or-delete "SRS_STABILITY"
                         (when-let* ((s (fsrs-card-stability card))) (number-to-string s)))
  (org-sm--put-or-delete "SRS_DIFFICULTY"
                         (when-let* ((d (fsrs-card-difficulty card))) (number-to-string d)))
  (org-sm--put-or-delete "SRS_LAST" (fsrs-card-last-review card))
  (org-sm--put-or-delete "SRS_STEP"
                         (when-let* ((s (fsrs-card-step card))) (number-to-string s))))

;;;; ---- Review log ----------------------------------------------------------

(defun org-sm--log-review (type &optional rating extra)
  "Log a review entry for TYPE (symbol) with optional RATING and EXTRA string."
  (org-with-wide-buffer
   ;; Always log into a drawer, whatever the user's `org-log-into-drawer' is:
   ;; `org-end-of-meta-data' skips drawers but not bare log lines, so bare
   ;; entries would land inside `org-sm-body-bounds' and be treated as body
   ;; text -- which `org-sm-set-body' would then delete.
   (let* ((org-log-into-drawer t)
          (ts   (format-time-string (org-time-stamp-format 'long 'inactive)))
          (name (symbol-name type))
          (line (concat "- " name
                        (when rating (format "  %s" rating))
                        (when extra  (format "  %s" extra))
                        (format "  %s\n" ts))))
     (goto-char (org-log-beginning t))
     (insert line))))

;;;; ---- Topic scheduling ----------------------------------------------------

(defun org-sm-topic-afactor ()
  "Return A-Factor for current heading derived from org priority.
[#A] → 1.2 (slow growth, review frequently)
[#B] or none → 1.5 (`org-entry-get' reports a missing cookie as B)
[#C] → 1.8 (fast growth, review less)"
  (pcase (org-entry-get nil "PRIORITY")
    ("A" 1.2) ("B" 1.5) (_ 1.8)))

(defun org-sm-topic-interval (a)
  "Return the next interval in days for the current topic, given A-Factor A.
Read-only: reads org properties, writes nothing.  Measured from SCHEDULED, not
now, so early/late reviews don't distort spacing."
  (let* ((last (org-entry-get nil "SRS_LAST"))
         (ref  (or (org-get-scheduled-time nil) (current-time))))
    (max 1 (round (* a (if last
                           (max 1 (org-sm-elapsed-days (parse-iso8601-time-string last) ref))
                         1))))))

(defun org-sm--topic-write (ivl)
  "Apply IVL (days) to current topic: update SCHEDULED and SRS_LAST."
  (org-sm--schedule (time-add (current-time) (days-to-time ivl)))
  (org-entry-put nil "SRS_LAST" (format-time-string "%FT%TZ" (current-time) "UTC0")))

;;;; ---- Grading (buffer mutations, no UI) --------------------------------
;; Single source of truth for "what a review does": update scheduling, log it,
;; return a result plist.  Shared by the interactive command and other front-ends.

(defvar org-sm-after-card-change-functions nil
  "Hook receiving an event after a successful card mutation.
Unlike review events, this includes creation, editing and restoration.")

(defun org-sm--notify (hook event)
  "Run HOOK observers without making a completed write appear to fail."
  (run-hook-wrapped hook
                    (lambda (fn)
                      (condition-case err
                          (funcall fn event)
                        (error (message "org-sm observer %S: %s" fn
                                        (error-message-string err))))
                      nil)))

(defun org-sm--card-changed (action &optional card)
  "Notify observers of completed ACTION on CARD (or current metadata)."
  (org-sm--notify 'org-sm-after-card-change-functions
                  (list :action action :card (or card (org-sm-card-at-point)))))

(defun org-sm--commit-action (action card &rest props)
  "Publish a review event for ACTION on pre-action CARD and return it."
  (let ((event (append (list :action action :card card
                             :session (and (org-sm--current-review-card-p) t))
                       props)))
    (org-sm--notify 'org-sm-after-card-change-functions event)
    (org-sm--notify 'org-sm-after-review-functions event)
    event))

(defun org-sm-default-feedback (event)
  "Format default echo-area feedback for EVENT."
  (let ((action (plist-get event :action))
        (rating (plist-get event :rating))
        (old (plist-get event :old-days))
        (new (plist-get event :new-days))
        (label (plist-get event :interval-label)))
    (pcase action
      ('dismiss "dismissed")
      ((or 'reschedule 'postpone)
       (format "topic reschedule → %dd"
               (or (plist-get event :new-days) (plist-get event :postpone-days))))
      ('cloze (if (and new (> new 0))
                  (if (and old (> old 0))
                      (format "cloze %s · %dd → %dd" rating old new)
                    (format "cloze %s → %dd" rating new))
                (format "cloze %s → %s" rating (or label ""))))
      ('topic (if (and old (> old 0))
                  (format "topic · %dd → %dd" old new)
                (format "topic → %dd" new)))
      (_ (format "%s" action)))))

(defun org-sm-default-finish (&optional prev aborted)
  "Format completion or abort feedback, including optional PREV."
  (if aborted "org-sm: review aborted"
    (format "org-sm: done 󱁖%s" (if prev (format "  (last: %s)" prev) ""))))

(defun org-sm-grade-topic ()
  "Reschedule current topic by its A-Factor and log the repetition.
Return a review event plist; presentation is handled by the caller."
  (let* ((card (org-sm-card-at-point))
         (a (org-sm-topic-afactor))
         (ivl (org-sm-topic-interval a)))
    (org-sm--topic-write ivl)
    (org-sm--log-review 'topic nil (format "a=%.1f  %2dd" a ivl))
    (org-sm--commit-action 'topic card
                           :old-days (plist-get card :interval-days)
                           :new-days ivl :interval-days ivl :afactor a)))

(defun org-sm-grade-reschedule (days)
  "Manually reschedule current topic to positive DAYS (SuperMemo `Ctrl+J').
Updates the base interval to DAYS (`SRS_LAST' = now, `SCHEDULED' = now+DAYS)
without counting as a repetition."
  (unless (and (integerp days) (> days 0))
    (user-error "Days must be a positive integer"))
  (let ((card (org-sm-card-at-point)))
    (org-sm--topic-write days)
    (org-sm--log-review 'reschedule nil (format "%2dd" days))
    (org-sm--commit-action 'reschedule card :postpone-days days
                           :old-days (plist-get card :interval-days)
                           :new-days days :interval-days days)))

(defalias 'org-sm-grade-postpone #'org-sm-grade-reschedule)

;;;; ---- Cloze overlays ------------------------------------------------------

(defvar org-sm--cloze-regexp "{{\\([^}]+\\)}}"
  "Regexp matching {{cloze}} markup; group 1 is the answer.")

(defface org-sm-cloze-face '((t :inherit bold :box t))
  "Face for {{cloze}} markers during normal reading."
  :group 'org-sm)

(defface org-sm-extracted-face
  '((((background dark))  :background "#292e42" :extend nil)
    (((background light)) :background "#f3ead8" :extend nil))
  "SuperMemo-style highlight face for extracted spans in parent cards."
  :group 'org-sm)

(defconst org-sm--extracted-end-regexp
  "\\[\\[id:[^]\n]+\\]\\[<[TC]>\\]\\]"
  "Regexp matching the closing back-reference link of an extracted span.")

(defconst org-sm--extracted-regexp
  (concat "\\(⌈\\)\\([^⌈]*?\\)\\(" org-sm--extracted-end-regexp "\\)")
  "Regexp matching an extracted span `⌈...[[id:...][<T>/<C>]]'.")

(defun org-sm--match-extracted (limit)
  "Font-lock matcher for `org-sm--extracted-regexp' up to LIMIT."
  (when (re-search-forward org-sm--extracted-regexp limit t)
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-multiline t)
    t))

(defvar org-sm--font-lock-keywords
  (let ((re "\\({{\\)\\([^}]+\\)\\(}}\\)"))
    `((,re
       (1 '(face nil display "") prepend)   ; hide "{{"
       (2 'org-sm-cloze-face    prepend)    ; show answer with face
       (3 '(face nil display "") prepend))  ; hide "}}"
      ("⌈"
       (0 'shadow prepend))                 ; dim "⌈" delimiter
      (org-sm--match-extracted
       (2 'org-sm-extracted-face prepend)))) ; highlight extracted span
  "Font-lock keywords for {{cloze}} and extracted spans.")

(defun org-sm--cloze-overlays ()
  "Return all org-sm cloze overlays in the current entry."
  (seq-filter (lambda (ov) (eq (overlay-get ov 'category) 'org-sm-cloze))
              (overlays-in (org-entry-beginning-position) (org-entry-end-position))))

(defun org-sm-cloze-apply-overlays ()
  "Hide all {{cloze}} answers in current heading."
  (save-excursion
    (goto-char (org-entry-beginning-position))
    (while (re-search-forward org-sm--cloze-regexp (org-entry-end-position) t)
      (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov 'category      'org-sm-cloze)
        (overlay-put ov 'display       (propertize "[___]" 'face '(bold highlight)))
        (overlay-put ov 'org-sm-answer (match-string 1))
        (overlay-put ov 'evaporate     t)))))

(defun org-sm-cloze-reveal-overlays ()
  "Reveal all cloze answers in current heading."
  (dolist (ov (org-sm--cloze-overlays))
    (overlay-put ov 'display
                 (propertize (format "[%s]" (overlay-get ov 'org-sm-answer))
                             'face 'success))))

(defun org-sm-cloze-remove-overlays ()
  "Remove all cloze overlays from current heading."
  (mapc #'delete-overlay (org-sm--cloze-overlays)))

;;;; ---- Cloze grading (pure, no UI) -----------------------------------------

(defun org-sm-interval-label (secs)
  "Format SECS as a short human interval label like \"10m\", \"2h\", \"3d\", \"5mo\".
Single source of truth for interval labels across review menus and the web UI."
  (let ((days (/ secs 86400.0)))
    (cond ((< days (/ 1.0 24)) (format "%dm" (max 1 (round (/ secs 60)))))
          ((< days 1)          (format "%dh" (round (* days 24))))
          ((< days 30)         (format "%dd" (round days)))
          ((< days 365)        (format "%dmo" (round (/ days 30.0))))
          (t                   (format "%.1fy" (/ days 365.0))))))

(defconst org-sm-cloze-ratings '(:again :hard :good :easy)
  "Ordered list of FSRS ratings offered for cloze review.")

(defun org-sm-preview-cloze ()
  "Preview every rating for the current cloze without modifying anything.
Return an alist mapping each rating symbol to a plist
(:card CARD :interval-secs SECS), where CARD is the hypothetical FSRS card
and SECS the resulting interval.  Reads properties without modifying the card."
  (org-sm--ensure-scheduler)
  (let ((card (org-sm--cloze-read))
        (now  (fsrs-now)))
    (mapcar
     (lambda (rating)
       (let* ((c    (cl-nth-value 0 (fsrs-scheduler-review-card
                                     org-sm--scheduler card rating)))
              (secs (fsrs-timestamp-difference (fsrs-card-due c) now)))
         (cons rating (list :card c :interval-secs secs))))
     org-sm-cloze-ratings)))

(defun org-sm-grade-cloze (rating &optional preview)
  "Apply RATING (a symbol in `org-sm-cloze-ratings') to the current cloze.
Write the new FSRS card, log the review, and return a review event plist.
PREVIEW, if given, is the alist from `org-sm-preview-cloze', reused to
avoid recomputing."
  (let* ((card-info (org-sm-card-at-point))
         (entry (or (cdr (assq rating (or preview (org-sm-preview-cloze))))
                    (error "Unknown cloze rating: %S" rating)))
         (card  (plist-get entry :card))
         (secs  (plist-get entry :interval-secs))
         (days  (max 0 (round (/ secs 86400.0))))
         (label (org-sm-interval-label secs))
         (extra (format "%s  %s" rating label)))
    (org-sm--cloze-write card)
    (org-sm--log-review 'cloze nil extra)
    (org-sm--commit-action 'cloze card-info :rating rating
                           :old-days (plist-get card-info :interval-days)
                           :new-days days :interval-secs secs
                           :interval-label label :due (fsrs-card-due card))))

;;;; ---- Heading helpers -----------------------------------------------------

(defun org-sm-type ()
  "Return SRS_TYPE of current heading as a symbol, or nil."
  (when-let* ((v (org-entry-get nil "SRS_TYPE"))) (intern v)))

(defun org-sm--goto-enclosing-card ()
  "Move point to the nearest enclosing card heading (current or ancestor).
Return its start position, or nil if no ancestor is a card.

`org-up-heading-safe' leaves point where it is once there is no parent left,
so the climb must stop on its nil return.  Anything that keeps looping there
(jumping to `point-min', say) re-tests the very same heading and hangs Emacs
on a top-level non-card heading."
  (save-match-data
    ;; Text before the first heading has no enclosing card; report that as nil
    ;; rather than letting `org-back-to-heading' raise its own user-error,
    ;; which callers would surface instead of their own message.
    (when (or (org-at-heading-p) (ignore-errors (org-back-to-heading t) t))
      (let ((climbing t) pos)
        (while (and (not pos) climbing)
          (if (org-sm-type)
              (setq pos (point))
            (setq climbing (org-up-heading-safe))))
        pos))))

(defun org-sm-body-bounds ()
  "Return (start . end) of current heading's body (excluding meta-data).
The body stops at the first child heading: for a heading with no body of its
own `org-entry-end-position' reaches past the whole subtree, and callers like
`org-sm-set-body' would then overwrite the children."
  (save-excursion
    (org-back-to-heading t)
    ;; Measure the child boundary from the heading itself: with no body,
    ;; `org-end-of-meta-data' already moves past the first child.
    (let ((limit (save-excursion (outline-next-heading) (point)))
          (start (progn (org-end-of-meta-data t) (point))))
      (cons (min start limit) (min (org-entry-end-position) limit)))))

(defun org-sm-body-text ()
  "Return the current entry's trimmed body, excluding metadata and descendants."
  (let ((bounds (org-sm-body-bounds)))
    (string-trim (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(defun org-sm-reading-bounds ()
  "Return the part of the current card the reader is looking at.
The active region when there is one; otherwise the card body clipped to
the window, i.e. the text on screen.  For a long topic that is the next
screenful from the read point, not the whole article, which is what a
suggestion should be about and what keeps the request short."
  (let ((body (org-sm-body-bounds)))
    (if (use-region-p)
        (cons (max (car body) (region-beginning)) (min (cdr body) (region-end)))
      (cons (max (car body) (window-start))
            (min (cdr body) (window-end nil t))))))

(defun org-sm-paragraph-bounds ()
  "Return (START . END) of the paragraph at point, clipped to the card body.
This is what a reader means by \"this\" when stuck: narrower than the card,
wider than a sentence."
  (let ((body (org-sm-body-bounds)))
    (save-excursion
      (save-restriction
        (narrow-to-region (car body) (cdr body))
        (cons (progn (forward-paragraph) (backward-paragraph)
                     (skip-chars-forward " \t\n") (point))
              (progn (forward-paragraph) (skip-chars-backward " \t\n") (point)))))))

(defun org-sm-section-count ()
  "Return how many plain headings the card at point holds before a child card.
org-sm assumes every heading inside a card is itself a card: the body ends
at the first child heading, the workbench reads `**' as a new card, and
child counts drive stall detection.  A plain section heading breaks all
three, so a non-zero count means `org-sm-sectionize-card' is due."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t)))
          (n 0))
      (while (and (outline-next-heading) (< (point) end) (not (org-sm-type)))
        (setq n (1+ n)))
      n)))

(defalias 'org-sm-body-clean #'string-trim
  "Trim surrounding whitespace from RAW.")

(defun org-sm-set-title (title)
  "Replace the current card title, preserving Org metadata, and notify observers."
  (org-back-to-heading t)
  (org-edit-headline (string-trim title))
  (org-sm--card-changed 'edit))

(defun org-sm-set-body (new-body)
  "Replace the current heading's body text with NEW-BODY.
Point must be on the heading.  Metadata (planning line, drawer) is
preserved; only the body region from `org-sm-body-bounds' is replaced.
Used by `org-sm--capture-commit-body' for manual or AI-assisted workbench
edits."
  (let* ((bounds (org-sm-body-bounds))
         (start  (car bounds))
         (end    (cdr bounds)))
    (goto-char start)
    (delete-region start end)
    (insert (org-sm-body-clean new-body))
    ;; keep a trailing newline so the subtree stays well-formed
    (unless (bolp) (insert "\n"))
    (org-sm--card-changed 'edit)))

(defun org-sm--cloze-markers-p ()
  "Return non-nil if current heading body contains {{cloze}} markers."
  (let ((bounds (org-sm-body-bounds)))
    (string-match-p org-sm--cloze-regexp
                    (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(defun org-sm--truncate-title (str)
  "Truncate STR to `org-sm-title-max-length' columns, collapsing newlines."
  (let ((flat (replace-regexp-in-string "[ \t]*\n[ \t]*" " " (string-trim str))))
    (truncate-string-to-width flat org-sm-title-max-length nil nil "…")))

(defconst org-sm--all-srs-props
  '("SRS_LAST" "SRS_STATE" "SRS_STABILITY" "SRS_DIFFICULTY" "SRS_STEP")
  "All SRS scheduling properties cleared on re-mark.
SRS_TYPE is intentionally excluded: it is overwritten by `org-sm--init-item',
not deleted, so that re-marking always sets a valid type.")

(defun org-sm--init-item (type)
  "Write SRS_TYPE and type-specific properties for TYPE (symbol).
Caller must call `org-sm--schedule' before this function, as `org-entry-put'
moves point into the PROPERTIES drawer."
  (org-entry-put nil "SRS_TYPE" (symbol-name type))
  (dolist (p org-sm--all-srs-props) (org-delete-property p))
  (pcase type
    ('topic
     (org-entry-put nil "SRS_LAST" (format-time-string "%FT%TZ" (current-time) "UTC0")))
    ('cloze
     (org-entry-put nil "SRS_STATE" ":learning"))))

;;;; ---- Capture ---------------------------------------------

(defcustom org-sm-capture-file nil
  "Target org file for `org-sm-capture'.
When nil, cards go to \"sm-cards.org\" under `org-sm-directory' (created on
demand).  Cards land in one inbox; sort them into per-topic files yourself."
  :type '(choice (const nil) file)
  :group 'org-sm)

(defcustom org-sm-capture-olp '("sm-cards")
  "Outline path for the capture target (list of heading strings).
The card is inserted as a child of the deepest heading in the path, which
is created if missing.  Example: \\='(\"Topics\" \"Physics\")"
  :type '(repeat string)
  :group 'org-sm)

(defun org-sm-capture-target-file ()
  "Return the capture file, defaulting to sm-cards.org under `org-sm-directory'."
  (expand-file-name
   (or org-sm-capture-file
       (expand-file-name "sm-cards.org" (or org-sm-directory default-directory)))))

;;;###autoload
(defun org-sm-capture-change-target (&optional file)
  "Select the target `.org' FILE for `org-sm-capture'."
  (interactive
   (list (read-file-name
          "Capture target file: "
          (file-name-directory (org-sm-capture-target-file))
          (org-sm-capture-target-file)
          nil nil
          (lambda (f)
            (or (file-directory-p f)
                (string-suffix-p ".org" f t))))))
  (unless (and (stringp file) (not (string-empty-p file)))
    (user-error "No capture target file selected"))
  (setq org-sm-capture-file (expand-file-name file))
  (when (derived-mode-p 'org-sm-capture-mode) (org-sm--capture-show-hint))
  (message "org-sm: capture target → %s"
           (or (org-sm-book-label org-sm-capture-file)
               (file-name-nondirectory org-sm-capture-file))))

(defun org-sm--grab-content ()
  "Return the region or clipboard text to capture, as a plain string.
An active region is read directly and also put on the kill ring, so the
clipboard agrees with what was captured; otherwise read the clipboard.
PDF pages, which have no Emacs region, go through `org-sm-capture-page'."
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (kill-new text)
        (deactivate-mark)
        text)
    (substring-no-properties (or (ignore-errors (current-kill 0 t)) ""))))

(defun org-sm--clipboard-content ()
  "Return non-blank system clipboard text, or signal a `user-error'.
Prefer the GUI clipboard selection and fall back to the kill ring, so a
one-key capture works in both graphical and terminal Emacs.  Kept separate
from `org-sm--grab-content', whose region-copy behavior belongs only to the
interactive editable capture flow."
  (let ((content (string-trim
                  (or (ignore-errors
                        (gui-get-selection 'CLIPBOARD 'UTF8_STRING))
                      (ignore-errors (current-kill 0 t))
                      ""))))
    (unless (org-string-nw-p content)
      (user-error "Clipboard is empty"))
    content))

(defconst org-sm--capture-card-rx "^\\*\\* "
  "Regexp matching a card heading in an `org-sm-capture-mode' buffer.")

(defun org-sm-capture-preamble-bounds ()
  "Return (START . END) of the buffer's preamble.
The preamble is the text before the first card heading, or the whole buffer
if there is none.  The one place that locates that heading."
  (org-with-wide-buffer
   (goto-char (point-min))
   (cons (point-min)
         (if (re-search-forward org-sm--capture-card-rx nil t)
             (match-beginning 0)
           (point-max)))))

(defun org-sm--capture-split-buffer ()
  "Split the current capture buffer into (PREAMBLE . CARDS).
PREAMBLE is the trimmed preamble text (see `org-sm-capture-preamble-bounds');
CARDS is a list of (TITLE . BODY), one per card heading, nil when there are
none.  Each heading is a card the reader wrote by hand."
  (org-with-wide-buffer
   (let ((end (cdr (org-sm-capture-preamble-bounds)))
         cards)
     (goto-char end)
     (while (re-search-forward org-sm--capture-card-rx nil t)
       (let* ((title  (org-get-heading t t t t))
              (bounds (org-sm-body-bounds))
              (body   (string-trim (buffer-substring-no-properties
                                    (car bounds) (cdr bounds)))))
         (when (org-string-nw-p body)
           (push (cons (and (org-string-nw-p title) (string-trim title)) body)
                 cards))))
     (cons (string-trim (buffer-substring-no-properties (point-min) end))
           (nreverse cards)))))

(defvar-local org-sm-capture-parent-marker nil
  "Marker of the card this buffer edits, or nil when no card exists yet.
Set once by whoever opens the buffer (`org-sm--capture-buffer'): non-nil in
`org-sm-card-workbench', nil in plain `org-sm-capture'.  It is the only
signal distinguishing the two: `org-sm-capture-commit' refines in place when
it is set and creates new cards when it is not, and the AI actions refuse to
run without it.  Never reassigned.")

(defvar-local org-sm-capture-card-type nil
  "SRS_TYPE (`topic'/`cloze'/nil) of the source card for `org-sm-card-workbench'.
Selects the AI prompt for the workbench's leading body text.  nil in plain
capture buffers, where drafts have no type yet anyway.")

(defun org-sm--capture-live-marker ()
  "Return `org-sm-capture-parent-marker' if its buffer is still alive.
Read it in the capture buffer before switching buffers: it is buffer-local,
so reading it later would see the *target* buffer's own nil value."
  (and org-sm-capture-parent-marker
       (marker-buffer org-sm-capture-parent-marker)
       org-sm-capture-parent-marker))

(defun org-sm--capture-at-marker (marker fn)
  "Call FN with point on MARKER's heading, then save that buffer."
  (with-current-buffer (marker-buffer marker)
    (save-excursion (goto-char marker) (funcall fn))
    (when (buffer-file-name) (let ((save-silently t)) (save-buffer)))))

(defun org-sm-capture-set-preamble (body)
  "Replace the leading text of this capture buffer with BODY."
  (pcase-let ((`(,beg . ,end) (org-sm-capture-preamble-bounds)))
    (delete-region beg end)
    (goto-char beg)
    (insert (string-trim body) (if (= end (point-max)) "\n" "\n\n"))
    (goto-char (point-min))
    (org-fold-show-all)))

(defun org-sm--capture-commit-workbench (buf marker)
  "Write BUF back to MARKER's card, then extract its underlined spans.
Step 1 replaces the card's body with BUF's text.  Step 2 walks the
underlined spans (`org-sm-pencils') from end to start and runs
`org-sm-extract' on each, so every child card is highlighted in place
with `⌈...[[id:..][<T>]]' and knows where in the parent it came from --
the same result as extracting by hand."
  (let (body spans)
    (with-current-buffer buf
      (goto-char (point-min))
      (let ((beg (progn (skip-chars-forward " \t\n") (point)))
            (end (progn (goto-char (point-max))
                        (skip-chars-backward " \t\n") (point))))
        (when (< beg end)
          (setq body (buffer-substring-no-properties beg end)
                spans (delq nil
                            (mapcar (lambda (ov)
                                      (let ((s (max beg (overlay-start ov)))
                                            (e (min end (overlay-end ov))))
                                        (and (< s e)
                                             (list (buffer-substring-no-properties s e)
                                                   (- s beg) (- e beg)))))
                                    (nreverse (org-sm-pencils beg end))))))))
    (when body
      (org-sm--capture-at-marker
       marker
       (lambda ()
         (org-sm-set-body body)
         (dolist (span spans)
           (apply #'org-sm-extract 'topic span))))
      (set-buffer buf)
      (erase-buffer)
      (if spans
          (format "refined body, extracted %d child card%s"
                  (length spans) (if (= (length spans) 1) "" "s"))
        "refined body"))))

(defun org-sm--capture-commit-plain (buf)
  "Commit BUF as new top-level topic card(s): preamble and any `**' headings."
  (let (bounds body cards done)
    (with-current-buffer buf
      (setq bounds (org-sm-capture-preamble-bounds)
            body   (string-trim (buffer-substring-no-properties
                                 (car bounds) (cdr bounds)))
            cards  (cdr (org-sm--capture-split-buffer))))
    (when (org-string-nw-p body)
      (org-sm-capture-card 'topic body)
      (set-buffer buf)
      (delete-region (car bounds) (cdr bounds))
      (push "captured new card" done))
    (when cards
      (dolist (c cards) (org-sm-capture-card 'topic (cdr c) nil nil (car c)))
      (set-buffer buf)
      (org-with-wide-buffer
       (delete-region (cdr (org-sm-capture-preamble-bounds)) (point-max)))
      (push (format "captured %d card%s"
                    (length cards) (if (= (length cards) 1) "" "s"))
            done))
    (and done (string-join (nreverse done) ", "))))

(defun org-sm-capture-commit ()
  "Commit this `org-sm-capture-mode' buffer and close it.
In a card workbench (`org-sm-capture-parent-marker' set), writes the body
back to the card and extracts every underlined span (`org-sm-pencils') into
a child topic card in place.  In plain capture, creates new top-level topic
card(s) from the leading text and any `**' headings."
  (interactive)
  (let* ((buf    (current-buffer))
         (marker (with-current-buffer buf (org-sm--capture-live-marker)))
         (status (if marker
                     (org-sm--capture-commit-workbench buf marker)
                   (org-sm--capture-commit-plain buf))))
    (unless status (user-error "Nothing to commit"))
    (set-buffer buf)
    (message "org-sm: %s" status)
    (when (string-blank-p (buffer-string)) (quit-window t))))

(defun org-sm-capture-abort ()
  "Discard the capture buffer without writing anything."
  (interactive)
  (quit-window t)
  (message "org-sm capture: aborted"))

(defvar org-sm-capture-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'org-sm-capture-commit)
    (define-key map (kbd "C-c C-k") #'org-sm-capture-abort)
    (define-key map (kbd "C-c C-t") #'org-sm-capture-change-target)
    (define-key map (kbd "M-n") #'org-sm-pencil-next)
    (define-key map (kbd "M-k") #'org-sm-pencil-dismiss)
    (define-key map (kbd "M-z") #'org-sm-item-extract)
    map)
  "Keymap for `org-sm-capture-mode'.
The AI extension (`org-sm-gptel') adds \`C-c C-a' in
`org-sm-card-workbench' buffers; plain capture never calls AI.")

(define-derived-mode org-sm-capture-mode org-mode "org-sm-cap"
  "Lightweight capture and workbench buffer.  Edit the content, then commit.

In a card workbench (`org-sm-card-workbench'), the buffer holds the card's
body, and wavy underlines (`org-sm-pencils') mark spans to extract as child
cards on commit.  In plain `org-sm-capture', leading text and `**' headings
become independent top-level cards.
\\<org-sm-capture-mode-map>\\[org-sm-capture-commit] commits;
\\[org-sm-capture-abort] discards without writing anything.")

(defun org-sm--capture-show-hint ()
  "Show in the header line what committing this buffer will write, and keys.
The buffer visits no file, so the header line is free (breadcrumb only
claims file buffers) and the hint stays put while you edit."
  (let ((hint
         (substitute-command-keys
          (concat
           "\\<org-sm-capture-mode-map>"
           (if org-sm-capture-parent-marker
               (concat "Body → this card · underlines → child cards ("
                       "\\[org-sm-item-extract] mark, "
                       "\\[org-sm-pencil-next] next, "
                       "\\[org-sm-pencil-dismiss] drop) · ")
             (format "[→ %s] Top text → one card · ** Title → one card each · "
                     (or (org-sm-book-label (org-sm-capture-target-file))
                         (file-name-nondirectory (org-sm-capture-target-file)))))
           "\\[org-sm-capture-commit] commit · "
           (unless org-sm-capture-parent-marker
             "\\[org-sm-capture-change-target] target · ")
           (when (and org-sm-capture-parent-marker (fboundp 'org-sm-gptel-card-ai))
             "\\[org-sm-gptel-card-ai] AI · ")
           "\\[org-sm-capture-abort] abort"))))
    ;; A header-line string is a mode-line construct: escape `%' in titles.
    (setq header-line-format (string-replace "%" "%%" hint))))

(defun org-sm--capture-buffer (content &optional name setup context)
  "Pop up an `org-sm-capture-mode' buffer named NAME, pre-filled with CONTENT.
NAME defaults to a shared \"*org-sm capture*\" buffer.  SETUP, when given,
is called with point in the new buffer after CONTENT is inserted -- use it
to set `org-sm-capture-parent-marker' / `org-sm-capture-card-type'
buffer-locally for callers that aren't the plain top-level capture flow.
CONTEXT, from `org-sm-source-context', appends a provenance line and binds
`org-sm-capture-file' in this buffer only, so routing one book's extract
never changes where the next unrelated capture goes."
  (let* ((name (or name "*org-sm capture*"))
         (existing (get-buffer name))
         (buf (or existing (get-buffer-create name)))
         (target (and context (org-sm--capture-target context))))
    ;; Reusing a draft name must not silently erase text typed in a prior sheet.
    (when (and existing (with-current-buffer existing
                          (not (string-blank-p (buffer-string)))))
      (user-error "Finish or abort the existing draft in %s first" name))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-sm-capture-mode) (org-sm-capture-mode))
      (kill-local-variable 'org-sm-capture-parent-marker)
      (kill-local-variable 'org-sm-capture-card-type)
      (kill-local-variable 'org-sm-capture-file)
      (kill-local-variable 'header-line-format)
      (erase-buffer)
      (insert content)
      (when-let* ((footer (org-sm-source-footer context)))
        (insert "\n\n" footer))
      (when target (setq-local org-sm-capture-file target))
      (goto-char (point-min))
      (when setup (funcall setup)))
    (pop-to-buffer buf
                   '(display-buffer-in-side-window
                     (side . right) (window-width . 0.5)))
    (with-current-buffer buf (org-sm--capture-show-hint))
    buf))

;;;###autoload
(defun org-sm-capture ()
  "Capture region/clipboard into an editable buffer, then commit as topic card(s).

Grabs the region or clipboard (see `org-sm--grab-content') into an
`org-sm-capture-mode' side buffer so you can see and edit it before
writing.  \\`C-c C-c' (`org-sm-capture-commit') writes the leading text as
one new topic card and each `**' heading as its own independent topic
card, whichever are present.  \\`C-c C-k' discards.

No AI here by design -- capture only stores the raw material.  Refining and
splitting happen later, when you review the card, via
`org-sm-card-workbench'.

When the capture comes from a PDF, EPUB or web page, a provenance link is
appended (see `org-sm-source') and the card is filed with the rest of that
document's cards rather than in the shared inbox.  A PDF page cannot be
selected inside Emacs, so there this delegates to `org-sm-capture-page'."
  (interactive)
  (let ((context (org-sm-source-context)))
    (if (eq (plist-get context :kind) 'pdf)
        (org-sm-capture-page)
      (org-sm--capture-buffer (org-sm--grab-content) nil nil context))))

;;;###autoload
(defun org-sm-capture-page ()
  "Capture the current PDF page, which cannot be selected inside Emacs.
The page's text layer goes to the kill ring and to an ordinary capture
buffer together with a `page:' link; trim it to the passage that matters."
  (interactive)
  (let* ((context (or (org-sm-source-context) (user-error "Not in a document")))
         (file (plist-get context :file))
         (page (plist-get context :page)))
    (unless (and file page) (user-error "This document has no pages"))
    (let ((text (or (org-sm-source-page-text file page)
                    (user-error "Page %d has no text layer (a scan?)" page))))
      (kill-new text)
      (org-sm--capture-buffer text nil nil context)
      (message "%s" (substitute-command-keys
                     (format "org-sm: page %d — trim it, then \\<org-sm-capture-mode-map>\\[org-sm-capture-commit]"
                             page))))))

(defun org-sm--capture-target (context)
  "Return the file to capture into for CONTEXT, falling back to the inbox.
Filing every book's extracts into one shared inbox is how a vault ends up
with hundreds of cards under a single heading; prefer the file already
holding this document's cards, but never mistake the inbox for that home."
  (or (and context
           (org-sm-source-target-file context (org-sm-capture-target-file)))
      (org-sm-capture-target-file)))

;;;###autoload
(defun org-sm-capture-clipboard-notify ()
  "Capture clipboard text as a topic card immediately and notify the user.

Unlike `org-sm-capture', this command has no editable confirmation buffer:
it writes one topic card directly to `org-sm-capture-file' /
`org-sm-capture-olp', saves its target buffer, and sends a desktop
notification containing the captured text.  Signal a `user-error' when the
clipboard is empty."
  (interactive)
  (let ((content (org-sm--clipboard-content)))
    (org-sm-capture-card 'topic content)
    (let ((coding-system-for-write 'utf-8-unix))
      (save-buffer))
    (message "org-sm: captured clipboard")
    ;; Notification failure must not make a successfully saved capture look
    ;; failed; a caller might otherwise retry and create a duplicate card.
    (when (require 'notifications nil t)
      (ignore-errors
        (notifications-notify :title "org-sm 已摘录" :body content :urgency 'normal)))))

;;;; ---- Mark / Extract ------------------------------------------------------

;;;###autoload
(defun org-sm-item-mark (&optional type)
  "Mark current heading as a topic or cloze SRS item.
TYPE is a symbol (`topic' or `cloze'); when nil, prompt interactively."
  (interactive)
  (unless (org-at-heading-p) (org-back-to-heading t))
  (when (org-sm-type)
    (unless (yes-or-no-p (format "Already a %s item.  Re-mark and reset? " (org-sm-type)))
      (user-error "Aborted")))
  (let ((type (or type (intern (completing-read "Mark as: " '("topic" "cloze") nil t)))))
    (org-edit-headline (concat (pcase type
                                 ('topic org-sm-topic-prefix)
                                 ('cloze org-sm-cloze-prefix))
                               (org-get-heading t t t t)))
    (org-sm--schedule (time-add (current-time) (days-to-time 1)))
    (org-id-get-create)
    (org-sm--init-item type)
    (org-sm--card-changed 'mark)
    (message "org-sm: marked as %s%s — due tomorrow%s" type
             (if (and (eq type 'cloze) (not (org-sm--cloze-markers-p)))
                 " (⚠ no {{cloze}} markers found)" "")
             ;; org-sm assumes headings inside a card are cards; say so now
             ;; rather than let a long article fail later, in the web reader.
             (let ((n (and (eq type 'topic) (org-sm-section-count))))
               (if (and n (> n 0))
                   (substitute-command-keys
                    (format " (%d plain section%s; \\[org-sm-sectionize-card] splits them)"
                            n (if (= n 1) "" "s")))
                 "")))))

(defun org-sm--insert-child (level type title body id &optional days)
  "Append a scheduled + inited SRS heading after point's subtree; return ID.
Insert a heading at LEVEL+1 at the end of the current subtree, titled TITLE,
typed TYPE, tagged with ID, scheduled DAYS from now (default 1), and filled
with BODY.
When point's heading is a card with a priority cookie, the child inherits it:
an extract of an [#A] passage is usually just as urgent, and retyping that on
every extraction is how a vault ends up almost entirely unprioritized.
Point-relative and excursion-agnostic: the caller owns buffer/point state
(`save-excursion', `org-with-wide-buffer', etc.) and computes LEVEL."
  (let ((priority (and (org-at-heading-p)
                       (plist-get (org-sm-card-at-point) :priority))))
    (org-end-of-subtree t t)
    (unless (bolp) (insert "\n"))
    (org-insert-heading nil t (1+ level))
    (insert title)
    (when priority (org-priority priority)))
  ;; Schedule before org-set-property / org-sm--init-item: those move point
  ;; into the PROPERTIES drawer, after which org-schedule would climb up to
  ;; the previous sibling heading (see `org-sm--schedule').
  (org-sm--schedule (time-add (current-time) (days-to-time (or days 1))))
  (org-set-property "ID" id)
  (org-sm--init-item type)
  (org-end-of-meta-data t)
  (unless (bolp) (insert "\n"))
  (insert body "\n")
  id)

(defun org-sm--strip-extract-marks (str)
  "Remove parent extraction markers (`⌈' and `[[id:...][<T>/<C>]]') from STR."
  (replace-regexp-in-string
   "⌈\\|\\[\\[id:[^]\n]+\\]\\[<[TC]>\\]\\]" "" str))

(defcustom org-sm-extract-origin-chars 800
  "Characters of parent text kept on each side of an extract's origin."
  :type 'integer
  :group 'org-sm)

(defun org-sm--extract-span-start (mark floor)
  "Return the `⌈' opening the extract whose link starts at MARK, else MARK.
Searches back to FLOOR.  A backlink appended by the workbench has no `⌈'
of its own; the nearest one before it then opens another extract, which
is closed before MARK."
  (save-excursion
    (goto-char mark)
    (let ((open (search-backward "⌈" floor t)))
      (if (and open
               (progn (goto-char (1+ open))
                      (not (re-search-forward "\\]\\[<[TC]>\\]\\]" mark t))))
          open
        mark))))

(defun org-sm-extract-origin ()
  "Return the parent text around where the card at point was extracted.
The result is (BEFORE . AFTER): up to `org-sm-extract-origin-chars' of the
parent body on each side of the extracted span, markers stripped.  nil
when the parent is not a card or does not link this one.  An extract
keeps the sentence that mattered; what its pronouns refer to, and the
step it skips, stayed in the parent."
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (when-let* (((org-at-heading-p))
                (id (org-entry-get nil "ID"))
                ((org-up-heading-safe))
                ((org-sm-type))
                (body (org-sm-body-bounds)))
      (goto-char (car body))
      (when (search-forward (format "[[id:%s][" id) (cdr body) t)
        (let* ((mark (match-beginning 0))
               (span (org-sm--extract-span-start mark (car body)))
               (end (or (search-forward "]]" (cdr body) t) (point)))
               (n org-sm-extract-origin-chars)
               (text (lambda (from to)
                       (string-trim (org-sm--strip-extract-marks
                                     (buffer-substring-no-properties from to))))))
          (cons (funcall text (max (car body) (- span n)) span)
                (funcall text end (min (cdr body) (+ end n)))))))))

(defun org-sm-extract (type selected sel-start sel-end)
  "Create a child SRS card of TYPE from the parent at point.
Point must be on the parent heading.  SELECTED is the extracted text.
SEL-START and SEL-END are its character offsets within the parent body
\(as returned by `org-sm-body-bounds').  Mark the extracted span in the
parent as `⌈...[[id:CHILD][<T>/<C>]]', append a scheduled + inited child
heading, and return the new child's id.  Pure buffer logic: no region,
no prompt, no overlay/UI.

- topic: child body is SELECTED verbatim; title is the topic prefix +
         first N chars of SELECTED.
- cloze: child body is the full parent body with SELECTED wrapped as
         {{answer}} at SEL-START; title is the cloze prefix + first N
         chars of the parent body."
  (let* ((lead      (- (length selected) (length (string-trim-left selected))))
         (trail     (- (length selected) (length (string-trim-right selected))))
         (selected  (string-trim selected))
         (sel-start (+ sel-start lead))
         (sel-end   (max sel-start (- sel-end trail)))
         (level     (org-current-level))
         (bounds    (org-sm-body-bounds))
         (body-raw  (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (clean-sel (string-trim (org-sm--strip-extract-marks selected)))
         (id        (org-id-new))
         (title     (pcase type
                      ('topic (concat org-sm-topic-prefix
                                      (org-sm--truncate-title clean-sel)))
                      ('cloze (concat org-sm-cloze-prefix
                                      (org-sm--truncate-title
                                       (org-sm--strip-extract-marks body-raw))))))
         (child-body (pcase type
                       ('topic clean-sel)
                       ('cloze (org-sm-body-clean
                                (org-sm--strip-extract-marks
                                 (concat (substring body-raw 0 sel-start)
                                         (format "{{%s}}" clean-sel)
                                         (substring body-raw sel-end))))))))
    (when (string-empty-p clean-sel)
      (user-error "Empty selection"))
    ;; Mark end before start so SEL-START is not shifted by insertion.
    (save-excursion
      (goto-char (+ (car bounds) sel-end))
      (insert (format "[[id:%s][%s]]" id (pcase type ('topic "<T>") ('cloze "<C>"))))
      (let ((span-end (point)))
        (when (looking-at-p "\\*+ ") (insert "\n"))
        (goto-char (+ (car bounds) sel-start))
        (insert "⌈")
        ;; Tag with `font-lock-multiline' so built-in
        ;; `font-lock-extend-region-multiline' extends single-line passes.
        (put-text-property (1- (point)) (1+ span-end) 'font-lock-multiline t)
        (when org-sm-mode
          (font-lock-flush (1- (point)) (1+ span-end)))))
    ;; Append child heading at end of current subtree.
    (save-excursion
      (org-sm--insert-child level type title child-body id)
      ;; [#C] marks material nobody has vouched for yet (imports start there,
      ;; see `org-sm-import-priority').  Extracting is the reader vouching
      ;; for a passage, so the child leaves C behind instead of inheriting it.
      (goto-char (org-find-property "ID" id))
      (when (eql (plist-get (org-sm-card-at-point) :priority) ?C)
        (org-priority 'remove)))
    (org-sm--card-changed 'extract)
    id))

;;;###autoload
(defun org-sm-card-workbench ()
  "Open an editable workbench to refine and/or split the card at point.

The card's body is copied into an `org-sm-capture-mode' buffer.  Wavy
underlines mark spans to extract as child topic cards (added by
`org-sm-gptel-card-ai' on \`C-c C-a', or by selecting text and pressing
\`M-z').  Nothing changes in the source until \`C-c C-c'
(`org-sm-capture-commit'), which writes the body back to this card and
extracts each underlined span in place via `org-sm-extract' -- leaving
`⌈...[[id:..][<T>]]' in the parent body.  \`C-c C-k' discards."
  (interactive)
  (when (org-before-first-heading-p) (user-error "Not on an org heading"))
  (org-back-to-heading t)
  (let* ((heading (org-get-heading t t t t))
         (body    (org-sm-body-text))
         (type    (org-sm-type))
         (marker  (point-marker)))
    (org-sm--capture-buffer
     body
     (format "*org-sm card: %s*"
             (truncate-string-to-width
              (replace-regexp-in-string "[^[:alnum:][:blank:]-]" "" heading)
              40 nil nil t))
     (lambda ()
       (setq-local org-sm-capture-parent-marker marker)
       (setq-local org-sm-capture-card-type type)))))

(defun org-sm--ensure-olp (file olp)
  "Return a marker on OLP's deepest heading in FILE, creating what is missing.

The marker must sit *on the heading line*: callers read `org-current-level'
there to place children (see `org-sm-capture-card').  Landing on whatever line
follows the insertion would silently file cards under an unrelated heading
whenever the target tree is not the last one in the file."
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (org-with-wide-buffer
       (or (ignore-errors (org-find-olp olp 'this-buffer))
           (let ((level 0))
             (dolist (head olp (point-marker))
               (setq level (1+ level))
               (if-let* ((m (ignore-errors
                              (org-find-olp (seq-take olp level) 'this-buffer))))
                   (progn (goto-char m) (set-marker m nil))
                 ;; Append inside the deepest parent found so far -- or at end
                 ;; of file for a new top-level heading -- then step back onto
                 ;; the inserted line so the next round descends from it.
                 (if (= level 1) (goto-char (point-max)) (org-end-of-subtree t t))
                 (unless (bolp) (insert "\n"))
                 (insert (make-string level ?*) " " head "\n")
                 (forward-line -1)))))))))

(defun org-sm-capture-card (type body &optional file olp title)
  "Create a new SRS card of TYPE with BODY under FILE / OLP; return its id.
Insert a scheduled + inited heading as the last child of the deepest heading
in OLP and fill it with BODY (via `org-sm--insert-child').  Like
`org-sm-extract' but with no parent and no back-reference; saves nothing.
FILE/OLP default to `org-sm-capture-target-file'/`org-sm-capture-olp'; the
file and headings are created if missing.  TITLE, when blank, is derived
from BODY.

Contract: leaves the target file's buffer current (deliberately, so callers
can save it), and that outlives any `with-current-buffer' around the call --
callers needing their own buffer back must `set-buffer' it explicitly."
  (let* ((body   (org-sm-body-clean (or body "")))
         (id     (org-id-new))
         (prefix (pcase type
                   ('topic org-sm-topic-prefix)
                   ('cloze org-sm-cloze-prefix)
                   (_ (error "Unknown capture type: %S" type))))
         (title  (concat prefix
                         (if (and title (org-string-nw-p title))
                             (string-trim title)
                           (org-sm--truncate-title body))))
         (target (org-sm--ensure-olp (or file (org-sm-capture-target-file))
                                     (or olp org-sm-capture-olp))))
    (set-buffer (marker-buffer target))
    (org-with-wide-buffer
     (goto-char target)
     (set-marker target nil)
     (org-sm--insert-child (org-current-level) type title body id)
     (org-sm--card-changed 'create)
     id)))

;;;###autoload
(defun org-sm-item-extract ()
  "Extract a passage, or mark a span in the workbench, or accept a suggestion.
One key (`M-z') handles all three:
- In a workbench with an active region: underline the region as a child-card
  span (committed on \\<org-sm-capture-mode-map>\\[org-sm-capture-commit]).
- On a suggestion underline with no region (in a source card): accept it
  into a child topic card immediately (`org-sm-pencil-accept').
- With an active region on a source card: prompt for topic/cloze and extract
  immediately via `org-sm-extract'."
  (interactive)
  (cond
   ((and (derived-mode-p 'org-sm-capture-mode) org-sm-capture-parent-marker)
    (if (region-active-p)
        (let ((beg (region-beginning)) (end (region-end)))
          (deactivate-mark)
          (org-sm-pencil-add beg end "手动标记")
          (goto-char beg)
          (message "%s" (org-sm-pencil-hint)))
      (if (org-sm-pencils) (org-sm-pencil-next)
        (user-error "Select text to mark as a child card, or C-c C-a for AI"))))
   ((and (not (region-active-p)) (org-sm--pencil-here))
    (org-sm-pencil-accept))
   ((not (region-active-p))
    (user-error "Select text to extract first"))
   (t
    (let* ((bounds    (org-sm-body-bounds))
           (sel-start (- (region-beginning) (car bounds)))
           (sel-end   (- (region-end) (car bounds)))
           (selected  (buffer-substring-no-properties
                       (region-beginning) (region-end)))
           (type      (intern (completing-read "Extract as: " '("topic" "cloze") nil t))))
      (org-sm-extract type selected sel-start sel-end))
    (when (eq org-sm--cloze-state 'revealed)
      (org-sm-cloze-remove-overlays)
      (org-sm-cloze-apply-overlays)))))

;;;; ---- Review session ------------------------------------------------------

(defun org-sm--dismissed-p ()
  "Return non-nil if current heading carries the :dismissed: tag."
  (member "dismissed" (org-get-tags nil t)))

(defun org-sm-sched-day-delta ()
  "Return whole-day offset from today to the current heading's SCHEDULED time.
Negative means overdue, 0 means today, positive means upcoming; nil when the
heading has no SCHEDULED time."
  (when-let* ((s (org-get-scheduled-time nil)))
    (- (time-to-days s) (time-to-days (current-time)))))

(defun org-sm-due-p ()
  "Return non-nil if current heading is a due (scheduled today or earlier) card."
  (and (org-sm-type)
       (not (org-sm--dismissed-p))
       (when-let* ((d (org-sm-sched-day-delta))) (<= d 0))))

(defun org-sm-map-items (fn &optional pred scope)
  "Map FN over SRS headings (skipping dismissed ones); collect non-nil results.
When PRED is non-nil, only entries for which it returns non-nil are visited.
SCOPE is an `org-map-entries' scope; nil means every vault file."
  (delq nil (org-map-entries
             (lambda ()
               (when (and (org-sm-type)
                          (not (org-sm--dismissed-p))
                          (or (null pred) (funcall pred)))
                 (funcall fn)))
             nil
             (or scope (org-sm-files)))))

(defun org-sm-due-markers (&optional pred scope)
  "Return (RANK TYPE MARKER CARD) for every SRS card matching PRED.
RANK is `org-get-priority' of the heading, larger meaning more important;
TYPE is `topic' or `cloze'; CARD is `org-sm-card-at-point' plus
`:late-days', how many days past due the card is.  SCOPE narrows the
search as in `org-sm-map-items'.  The one place a queue candidate is
defined, so every entry point orders the same pool."
  (org-sm-map-items
   (lambda ()
     (list (org-get-priority (org-get-heading t t nil t))
           (org-sm-type)
           (point-marker)
           (append (org-sm-card-at-point)
                   (list :late-days (- (or (org-sm-sched-day-delta) 0))))))
   pred scope))

(defun org-sm--cleanup-buffer ()
  "Clear review progress, narrowing, overlays, and state in previous buffer.
Widening belongs here rather than in the caller: a review queue spans files, so
by the time the next card is shown the previous buffer is no longer current and
would silently stay narrowed to the card just reviewed."
  (when (buffer-live-p org-sm--current-buffer)
    (with-current-buffer org-sm--current-buffer
      (setq org-sm--mode-line nil
            org-sm--header-line nil
            header-line-format (delete org-sm--header-line-construct
                                       header-line-format))
      (unless header-line-format
        (kill-local-variable 'header-line-format))
      (force-mode-line-update)
      (when (buffer-narrowed-p) (widen))
      (org-sm-pencils-clear)
      (when org-sm--cloze-state
        (org-sm-cloze-remove-overlays)
        (setq org-sm--cloze-state nil))))
  (when (markerp org-sm--current-card-marker)
    (set-marker org-sm--current-card-marker nil))
  (setq org-sm--current-buffer nil org-sm--current-card-marker nil))

(defun org-sm--goto-marker (marker)
  "Switch to MARKER's buffer, narrow to its subtree, set up cloze state."
  (unless (marker-buffer marker)
    (error "org-sm: stale marker — restart the review session"))
  (org-sm--cleanup-buffer)            ; clear leftover state from the last card
  (switch-to-buffer (marker-buffer marker))
  (setq org-sm--current-buffer (current-buffer))
  (widen)
  (goto-char marker)
  (org-back-to-heading t)
  (setq org-sm--current-card-marker (point-marker))
  (org-narrow-to-subtree)
  (org-fold-hide-subtree)
  ;; Topics are reading material: also show child heading lines, so the
  ;; outline is visible.  Clozes are atomic leaves -- keeping branches hidden
  ;; avoids leaking an answer should one ever have children.
  (org-fold-show-entry)
  (when (eq (org-sm-type) 'topic)
    (org-fold-show-branches))
  (goto-char (point-min))
  (recenter 0)
  (when (eq (org-sm-type) 'cloze)
    (org-sm-cloze-apply-overlays)
    (setq org-sm--cloze-state 'hidden))
  ;; Optional read point: jump to the inline anchor marking where you last read.
  (when-let* ((rp (save-excursion
                    (goto-char (point-min))
                    (org-back-to-heading t)
                    (org-sm--readpoint-target))))
    (goto-char rp)
    (org-fold-show-context 'org-goto)
    (recenter)))

(defun org-sm--confirm-key-label ()
  "Return the confirm action label with interval preview for the current card."
  (pcase (org-sm-type)
    ('topic
     (format "confirm(%dd)" (org-sm-topic-interval (org-sm-topic-afactor))))
    ('cloze
     (if (eq org-sm--cloze-state 'revealed)
         (let* ((preview (org-sm-preview-cloze))
                (keys    '((?a . :again) (?h . :hard) (?r . :good) (?e . :easy)))
                (labels  (mapcar
                          (lambda (kr)
                            (format "%c:%s" (car kr)
                                    (org-sm-interval-label
                                     (plist-get (cdr (assq (cdr kr) preview))
                                                :interval-secs))))
                          keys)))
           (format "rate(%s)" (string-join labels " ")))
       "reveal"))
    (_ "confirm")))

(defun org-sm-review-keys-hint ()
  "Return keybinding hint string for an active review session.
Leads with the action that actually resolves the current card: a stalled
topic or a leech offers `w', everything else confirms."
  (let* ((card    (org-sm-card-at-point))
         (stalled (and card (org-sm-stalled-p card org-sm-stall-review-threshold)))
         (leech   (and card (org-sm-leech-p card org-sm-leech-again-threshold)))
         (plain   (and card (eq (plist-get card :type) 'topic)
                       (> (org-sm-section-count) 0))))
    (substitute-command-keys
     (concat "\\<org-sm-review-repeat-map>"
             (if stalled "⚠ " "")
             (if leech "↻ " "")
             (if plain "¶ " "")
             "\\[org-sm-review-confirm] " (org-sm--confirm-key-label)
             ", \\[org-sm-card-workbench] workbench"
             (when plain
               (concat ", \\[org-sm-sectionize-card] 拆小节"))
             (when (fboundp 'org-sm-gptel-explain)
               ", \\[org-sm-gptel-explain] explain")
             ", \\[org-sm-item-dismiss] dismiss"
             ", \\[org-sm-cycle-priority] priority"
             ", \\[org-sm-review-abort] abort"))))

(defun org-sm-default-prompt (state)
  "Update review header line and show keybinding hint in the echo area."
  (let* ((pos    (or (plist-get state :position) "SM"))
         (prev   (plist-get state :prev))
         (cstate (plist-get state :cloze-state))
         (type   (pcase (plist-get (plist-get state :card) :type)
                   ('cloze (format "cloze (%s)"
                                   (if (eq cstate 'hidden) "hidden" "revealed")))
                   ('topic "topic")
                   (_      "not an SRS heading"))))
    (org-sm-set-header-line
     (string-join (delq nil (list pos type (and prev (format "✓ %s" prev))))
                  " · "))
    (message "%s" (org-sm-review-keys-hint))))

(defun org-sm--show-prompt (&optional prev)
  "Present the active card prompt via `org-sm-prompt-function'."
  (funcall org-sm-prompt-function
           (list :card (org-sm-card-at-point)
                 :position org-sm--mode-line
                 :cloze-state org-sm--cloze-state
                 :prev prev)))

;;;###autoload
(defcustom org-sm-review-light-size 6
  "Maximum number of cards in a light review session."
  :type 'integer
  :group 'org-sm)

(defun org-sm--progress-label ()
  "Return the mode-line progress of the running session.
\"SM 3/6\" while the whole session fits a light one; past that, only how
many cards are done.  A full session's total is the backlog, and a running
count of what is left is the guilt that SuperMemo's auto-postpone exists
to hide: the queue already serves the most urgent cards first, so stopping
anywhere is safe."
  (let ((done (- org-sm--review-total (length org-sm--queue))))
    (if (<= org-sm--review-total org-sm-review-light-size)
        (format "SM %d/%d" done org-sm--review-total)
      (format "SM %d" done))))

(defun org-sm--advance (&optional prev)
  "Advance to the next queued item, or end the session.
PREV is a string describing the last action."
  (let (found)
    (while (and org-sm--queue (not found))
      (let ((marker (pop org-sm--queue)))
        (if (marker-buffer marker)
            (progn (org-sm--goto-marker marker)
                   (when (org-sm-type) (setq found t)))
          (message "org-sm: skipping stale marker"))))
    (if found
        (progn
          (setq org-sm--mode-line (org-sm--progress-label))
          (force-mode-line-update)
          (org-sm--show-prompt prev))
      (org-sm--cleanup-buffer)
      (when-let* ((msg (funcall org-sm-finish-function prev nil)))
        (message "%s" msg)))))

(defun org-sm--review-markers (markers &optional what)
  "Start a review session over MARKERS (a list).  WHAT names them in messages."
  (org-sm--ensure-scheduler)
  (if (null markers)
      (message "org-sm: nothing to review%s 󱁖" (if what (format " (%s)" what) ""))
    (setq org-sm--queue markers
          org-sm--review-total (length markers))
    (run-hooks 'org-sm-review-start-hook)
    (org-sm--advance)))

(defun org-sm-cycle-priority ()
  "Raise the current card's priority one step: C -> B -> A, then back to C.
A card without a cookie counts as B, as it does for queue rank and
A-Factor, so it goes to A.  Raising is the usual move -- imports start at
C and earn their way up -- so one press is one step up.  The A-Factor
follows the cookie (see `org-sm-topic-afactor'), so this also retunes how
soon a topic returns."
  (interactive)
  (unless (org-sm-type) (user-error "Not on an SRS heading"))
  (save-excursion
    (org-back-to-heading t)
    (let ((next (pcase (plist-get (org-sm-card-at-point) :priority)
                  (?C ?B) (?A ?C) (_ ?A))))
      (org-priority next)
      (org-sm--card-changed 'priority)
      (message "org-sm: priority [#%c]" next))))

(defun org-sm-review-start (&optional limit)
  "Start a review of due cards: the first LIMIT of them, or all.
Order comes from `org-sm-queue-order': clozes most at risk first, topics
interleaved up to `org-sm-review-topic-proportion', so a light session is
simply the head of the same queue a full session walks through."
  (interactive)
  (org-sm--review-markers
   (mapcar #'caddr (org-sm-queue-order (org-sm-due-markers #'org-sm-due-p) limit))
   (if limit (format "light, max %d" limit) "due")))

;;;###autoload
(defun org-sm-review-branch (&optional whole-file)
  "Review the due cards under the heading at point, in the usual order.
With prefix argument WHOLE-FILE, or before the first heading, review the
whole file.  SuperMemo's branch review: work toward a near goal (an
interview, one book) without the rest of the vault competing for the time,
while every card keeps its long-term schedule."
  (interactive "P")
  (unless (derived-mode-p 'org-mode) (user-error "Not in an Org buffer"))
  (let ((scope (if (or whole-file (org-before-first-heading-p)) 'file 'tree)))
    (org-sm--review-markers
     (mapcar #'caddr (org-sm-queue-order (org-sm-due-markers #'org-sm-due-p scope)))
     (if (eq scope 'file) "this file" "this branch"))))

;;;###autoload
(defun org-sm-review-start-light ()
  "Start a light review session with `org-sm-review-light-size' cards."
  (interactive)
  (unless (and (integerp org-sm-review-light-size)
               (> org-sm-review-light-size 0))
    (user-error "org-sm-review-light-size must be a positive integer"))
  (org-sm-review-start org-sm-review-light-size))

(defun org-sm--review-confirm-topic ()
  "Interactive topic review: next repetition, dismiss, reschedule, explain.
Computation is delegated to the grading functions; this only collects
the user's choice and advances the queue.  A stalled topic (see
`org-sm-stalled-p') gets the same keys but different emphasis: the prompt
says so and `r'--the key that never helped--is labelled as such.

Selection is excluded from the menu on purpose.  Reading topics is where
backlog actually shrinks (a dismissed card is gone; a postponed one just
waits), so it must cost one keystroke, not a confirmation."
  (let* ((auto-ivl (org-sm-topic-interval (org-sm-topic-afactor)))
         (card     (org-sm-card-at-point))
         (stalled  (org-sm-stalled-p card org-sm-stall-review-threshold))
         (choices  `((?r ,(format "%s(%dd)"
                                 (if stalled "push again" "next") auto-ivl))
                     (?j "reschedule")
                     (?e "explain")
                     ,@(when (fboundp 'org-sm-gptel-suggest) '((?x "suggest")))
                     ,@(when (fboundp 'org-sm-card-workbench)
                         '((?w "workbench"))))))
    (org-sm--prompt-choice (if stalled
                               (format "Topic (⚠ 空读 %d 次、无子卡，建议 d 或 x): "
                                       (plist-get card :reviews))
                             "Topic: ")
                           choices
      (lambda (key)
        (pcase key
          ((or ?r ?n)
           (org-sm--advance (funcall org-sm-feedback-function (org-sm-grade-topic))))
          (?j
           (org-sm--advance
            (funcall org-sm-feedback-function
                     (org-sm-grade-reschedule (read-number "Reschedule days: " 7)))))
          (?e (if (fboundp 'org-sm-gptel-explain)
                  (progn (org-sm--log-review 'explain)
                         (org-sm-gptel-explain))
                (user-error "org-sm-gptel not loaded")))
          (?w (org-sm-card-workbench))
          ;; Suggest does not advance: the suggestions are drawn on this card
          ;; and the reader decides about each before moving on.
          (?x (if (fboundp 'org-sm-gptel-suggest)
                  (org-sm-gptel-suggest)
                (user-error "org-sm-gptel not loaded"))))))))

(defun org-sm--cloze-rating-label (rating secs)
  "Format a menu LABEL for RATING with interval SECS."
  (format "%s(%s)" rating (org-sm-interval-label secs)))

(defun org-sm--review-confirm-cloze ()
  "Interactive cloze review: reveal, then rate (or dismiss).
Rating computation is delegated to `org-sm-preview-cloze' and
`org-sm-grade-cloze'; this only handles overlays and the queue."
  (pcase org-sm--cloze-state
    ('hidden
     (org-sm-cloze-reveal-overlays)
     (setq org-sm--cloze-state 'revealed)
     (org-sm--show-prompt))
    ('revealed
     (org-sm-cloze-remove-overlays)
     (let* ((preview (org-sm-preview-cloze))
            (card    (org-sm-card-at-point))
            (leech   (org-sm-leech-p card org-sm-leech-again-threshold))
            ;; Map menu key → rating symbol; keys stay stable (r = reveal, r = good).
            (keymap  '((?a . :again) (?h . :hard) (?r . :good) (?e . :easy)))
            (choices (append
                      (mapcar
                       (lambda (kr)
                         (let ((secs (plist-get (cdr (assq (cdr kr) preview)) :interval-secs)))
                           (list (car kr) (org-sm--cloze-rating-label (cdr kr) secs))))
                       keymap)
                      ;; A leech has failed recall twice already; offer the
                      ;; repair instead of another rating round.
                      (when leech '((?w "rewrite"))))))
       (org-sm--prompt-choice
        (if leech
            (format "Rate (↻ 已 again %d 次，建议 %s 改写): "
                    (plist-get card :again-count)
                    (substitute-command-keys "\\[org-sm-card-workbench]"))
          "Rate: ")
        choices
         (lambda (key)
           (if (eq key ?w)
               (org-sm-card-workbench)
             (let* ((rating (cdr (assq key keymap)))
                    (r      (org-sm-grade-cloze rating preview)))
               (org-sm--advance (funcall org-sm-feedback-function r))))))))))

;;;###autoload
(defun org-sm-review-confirm ()
  "Confirm topic read or advance cloze state, then move to next item."
  (interactive)
  (org-sm--ensure-scheduler)
  (unless (or org-sm--queue org-sm--cloze-state
              (buffer-live-p org-sm--current-buffer))
    (user-error "No active review session — call org-sm-review-start"))
  (unless (org-sm--current-review-card-p)
    (user-error "Return to the current review card before confirming"))
  (pcase (or (org-sm-type) (user-error "Not on an SRS heading"))
    ('topic (org-sm--review-confirm-topic))
    ('cloze (org-sm--review-confirm-cloze))))


(defun org-sm-dismiss ()
  "Tag the current item :dismissed:, log it and return a review event.
This operation modifies the buffer but performs no UI.
Signals an error if not on an SRS heading or already dismissed.  All SRS
properties and scheduling data are preserved for `org-sm-item-undismiss'."
  (unless (org-sm-type) (user-error "Not on an SRS heading"))
  (when (org-sm--dismissed-p) (user-error "Already dismissed"))
  (let ((card (org-sm-card-at-point)))
    (org-toggle-tag "dismissed" 'on)
    (org-sm--log-review 'dismissed)
    (org-sm--commit-action 'dismiss card :old-days (plist-get card :interval-days))))

;;;###autoload
(defun org-sm-item-dismiss ()
  "Dismiss current SRS item, advancing the queue if a session is active."
  (interactive)
  (let* ((event (org-sm-dismiss))
         (fb (funcall org-sm-feedback-function event)))
    ;; A mutation of a different card must not consume this session's queue.
    (if (plist-get event :session)
        (org-sm--advance fb)
      (message "org-sm: %s" fb))))

(defun org-sm--prompt-choice (prompt choices action-fn)
  "Prompt user with CHOICES, appending built-in skip and dismiss options.
skip advances to the next card without any change (it stays due and
reappears next session); dismiss calls `org-sm-item-dismiss'.  For any
other key, call ACTION-FN with it."
  (let* ((all-choices (append choices '((?s "skip") (?d "dismiss"))))
         (choice (read-multiple-choice prompt all-choices))
         (key (car choice)))
    (pcase key
      (?s (org-sm--advance "skipped"))
      (?d (org-sm-item-dismiss))
      (_  (funcall action-fn key)))))

;;;###autoload
(defun org-sm-item-undismiss ()
  "Restore a dismissed SRS item by removing the :dismissed: tag."
  (interactive)
  (unless (org-sm-type) (user-error "Not on an SRS heading"))
  (unless (org-sm--dismissed-p) (user-error "Item is not dismissed"))
  (org-toggle-tag "dismissed" 'off)
  (org-sm--card-changed 'restore)
  (if-let* ((s (org-get-scheduled-time nil)))
      (message "org-sm: item restored — due %s" (format-time-string "%F" s))
    (message "org-sm: item restored (no schedule)")))

;;;; ---- Read point ----------------------------------------------------------
;; Marks where you last read inside a card via an inline "[[rp:CARDID][📖]]"
;; anchor at point: line-precise, edit-tolerant, unique per card, and distinct
;; from `id:' links.  One per card; setting again moves it; delete to clear.

(defun org-sm--readpoint-anchor (id)
  "Return the inline read-point anchor text for card ID."
  (format "[[rp:%s][📖]]" id))

(defun org-sm--readpoint-remove (id)
  "Remove any read-point anchor for card ID within the current subtree."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t) (point))))
      (while (re-search-forward (regexp-quote (org-sm--readpoint-anchor id)) end t)
        (replace-match "")))))

(defun org-sm--readpoint-move (id pos)
  "Place card ID's read-point anchor at POS, removing the previous one.
Point must be inside the card.  A marker carries POS across the removal,
so moving the anchor forward past its old position still lands on the
intended character.  Shared by Emacs and the web reader."
  (let ((target (copy-marker pos)))
    (unwind-protect
        (progn
          (org-sm--readpoint-remove id)
          (save-excursion
            (goto-char target)
            (insert (org-sm--readpoint-anchor id))))
      (set-marker target nil))
    (org-sm--card-changed 'readpoint)))

;;;###autoload
(defun org-sm-set-readpoint ()
  "Set the read point of the nearest enclosing SRS card at point.
Drops a unique inline anchor; the next review of that card jumps here, moving
any previous read point.  Clear it by deleting its 📖 anchor."
  (interactive)
  (let ((id (save-excursion
              (unless (org-sm--goto-enclosing-card)
                (user-error "org-sm: no enclosing SRS card found"))
              (org-id-get-create))))
    (org-sm--readpoint-move id (point))
    (message "org-sm: read point set")))

(defun org-sm--readpoint-target ()
  "Return the position of the current card's read-point anchor, or nil.
Point must be on the card heading."
  (when-let* ((id (org-id-get)))
    (save-excursion
      (let ((end (save-excursion (org-end-of-subtree t t) (point))))
        (when (search-forward (org-sm--readpoint-anchor id) end t)
          (match-beginning 0))))))



;;;; ---- Suggestions ("pencils") ------------------------------------------
;; A suggestion is a span of text that already exists, plus a reason.  It is
;; drawn as a wavy underline with no overlay keymap -- single keys on an
;; overlay would hijack normal/insert editing (e.g. Meow).  Instead, modifier
;; keys in `org-sm-mode-map' and `org-sm-capture-mode-map' act on underlines:
;; `M-n' next, `M-k' dismiss, `M-z' accept (or mark in a workbench).

(defface org-sm-pencil-face
  '((t :inherit highlight :underline (:style wave)))
  "Face marking a passage suggested for extraction."
  :group 'org-sm)

(defun org-sm-pencil-add (beg end reason)
  "Underline BEG..END as a suggestion, explained by REASON; return it."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'org-sm-pencil reason)
    (overlay-put ov 'face 'org-sm-pencil-face)
    (overlay-put ov 'help-echo reason)
    (overlay-put ov 'evaporate t)
    ov))

(defun org-sm-pencils (&optional beg end)
  "Return the suggestions between BEG and END (whole buffer), in order."
  (seq-sort-by #'overlay-start #'<
               (seq-filter (lambda (ov) (overlay-get ov 'org-sm-pencil))
                           (overlays-in (or beg (point-min)) (or end (point-max))))))

(defun org-sm-pencils-clear (&optional beg end)
  "Remove the suggestions between BEG and END (whole buffer)."
  (mapc #'delete-overlay (org-sm-pencils beg end)))

(defun org-sm--pencil-here ()
  "Return the suggestion overlay at point, or nil."
  (car (seq-filter (lambda (ov) (overlay-get ov 'org-sm-pencil))
                   (overlays-at (point)))))

(defun org-sm--pencil-at-point ()
  "Return the suggestion containing point, or signal a `user-error'."
  (or (org-sm--pencil-here) (user-error "No suggestion at point")))

(defun org-sm-pencil-hint ()
  "Return the key hint and reason for the suggestion at point."
  (let ((all (org-sm-pencils))
        (here (org-sm--pencil-at-point))
        (wb (and (derived-mode-p 'org-sm-capture-mode)
                 org-sm-capture-parent-marker)))
    (concat (substitute-command-keys
             (if wb
                 "\\<org-sm-capture-mode-map>\\[org-sm-capture-commit] 提交时摘为子卡  \\[org-sm-pencil-dismiss] 擦掉  \\[org-sm-pencil-next] 下一条"
               "\\[org-sm-item-extract] 摘为子卡  \\<org-sm-mode-map>\\[org-sm-pencil-dismiss] 擦掉  \\[org-sm-pencil-next] 下一条"))
            (format "  (%d/%d) 💡 %s"
                    (1+ (seq-position all here)) (length all)
                    (overlay-get here 'org-sm-pencil)))))

(defun org-sm-pencil-next ()
  "Move to the next suggestion, wrapping around, and show its reason."
  (interactive)
  (let* ((all (or (org-sm-pencils) (user-error "No suggestions left")))
         (next (or (seq-find (lambda (ov) (> (overlay-start ov) (point))) all)
                   (car all))))
    (goto-char (overlay-start next))
    (message "%s" (org-sm-pencil-hint))))

(defun org-sm-pencil-accept ()
  "Extract the suggestion at point into a child topic card.
Goes through `org-sm-extract', so the passage is highlighted in place and
the child records where it came from, exactly as a manual extraction.
In a workbench buffer, underlines are committed together on `C-c C-c', so
this simply advances to the next underline."
  (interactive)
  (if (and (derived-mode-p 'org-sm-capture-mode) org-sm-capture-parent-marker)
      (org-sm-pencil-next)
    (let* ((ov (org-sm--pencil-at-point))
           (beg (overlay-start ov))
           (end (overlay-end ov))
           (text (buffer-substring-no-properties beg end)))
      (delete-overlay ov)
      (save-excursion
        (goto-char beg)
        (org-sm--goto-enclosing-card)
        (let ((body (car (org-sm-body-bounds))))
          (org-sm-extract 'topic text (- beg body) (- end body))))
      (if (org-sm-pencils)
          (org-sm-pencil-next)
        (message "org-sm: 已摘为子卡。没有剩余建议")))))

(defun org-sm-pencil-dismiss ()
  "Remove the suggestion at point without extracting anything."
  (interactive)
  (delete-overlay (org-sm--pencil-at-point))
  (if (org-sm-pencils)
      (org-sm-pencil-next)
    (message "org-sm: 没有剩余建议")))

;;;; ---- Source navigation ---------------------------------------------------
;; A card's source is wherever its own ID is linked — `org-sm-extract' leaves an
;; [[id:CHILD]] link at the extraction point — so we just grep the vault for it.

;;;###autoload
(defun org-sm-goto-source ()
  "Grep the vault for links to this card's ID and jump to its source."
  (interactive)
  (let ((id (or (org-id-get) (user-error "org-sm: current heading has no ID"))))
    (grep (format "rg -n --glob '*.org' 'id:%s' %s"
                  id (shell-quote-argument
                      (expand-file-name (or org-sm-directory default-directory)))))))

;;;###autoload
(defun org-sm-review-abort ()
  "Stop the current review session and clean up its display state."
  (interactive)
  (unless (or org-sm--queue (buffer-live-p org-sm--current-buffer))
    (user-error "No active review session"))
  (setq org-sm--queue nil)
  (org-sm--cleanup-buffer)
  (when-let* ((msg (funcall org-sm-finish-function nil t)))
    (message "%s" msg)))

(defvar-keymap org-sm-review-repeat-map
  :doc "Repeat keys available during an org-sm review session."
  "r" #'org-sm-review-confirm
  "w" #'org-sm-card-workbench
  "d" #'org-sm-item-dismiss
  "p" #'org-sm-cycle-priority
  "s" #'org-sm-sectionize-card
  "a" #'org-sm-review-abort)

(dolist (command '(org-sm-review-start
                   org-sm-review-start-light
                   org-sm-tree-review
                   org-sm-review-confirm
                   org-sm-item-dismiss
                   org-sm-cycle-priority
                   org-sm-sectionize-card))
  (put command 'repeat-map 'org-sm-review-repeat-map))

;;;; ---- Tree view -----------------------------------------------------------
;; A read-only `org-mode' buffer aggregating cards (optionally filtered by due
;; days, type, and title keyword), preserving each file's outline so the whole
;; structure is visible and foldable — an antidote to one-card-at-a-time
;; fragmentation.  Edit in the real files (RET); review the subtree at point (r)
;; or all shown (C-u r).  Markers live in an `org-sm-marker' text property;
;; collection/review reuse the queue.

(defun org-sm-tree-goto ()
  "Jump to the real file location of the card at point."
  (interactive)
  (when-let* ((m (get-text-property (line-beginning-position) 'org-sm-marker)))
    (switch-to-buffer (marker-buffer m))
    (widen) (goto-char m) (org-fold-show-context 'org-goto) (recenter)))

(defun org-sm--tree-markers (beg end)
  "Return copied card markers on lines between BEG and END."
  (save-excursion
    (goto-char beg)
    (let (ms)
      (while (< (point) end)
        (when-let* ((m (get-text-property (line-beginning-position) 'org-sm-marker)))
          (push (copy-marker m) ms))
        (forward-line 1))
      (nreverse ms))))

(defun org-sm-tree-review (all)
  "Review the subtree at point, or with prefix ALL, every card in the tree."
  (interactive "P")
  (let ((beg (if all (point-min) (line-beginning-position)))
        (end (if all (point-max)
               (save-excursion
                 (if (outline-on-heading-p t)
                     (progn (outline-end-of-subtree) (min (point-max) (1+ (point))))
                   (line-end-position 2))))))
    (org-sm--review-markers (org-sm--tree-markers beg end) "tree")))

(defvar org-sm-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (keymap-set map "RET" #'org-sm-tree-goto)
    (keymap-set map "TAB" #'outline-cycle)
    (keymap-set map "<backtab>" #'outline-cycle-buffer)
    (keymap-set map "r" #'org-sm-tree-review)
    (keymap-set map "n" #'org-sm-neural-start)
    (keymap-set map "g" #'org-sm-tree-revert)
    map)
  "Keymap for `org-sm-tree-mode'.")

(define-derived-mode org-sm-tree-mode special-mode "org-sm-tree"
  "Clean tree view of SRS cards; TAB folds, RET jumps, r/C-u r reviews."
  (setq-local truncate-lines t
              outline-regexp "\\(?:▸\\| +├─\\)"
              outline-level (lambda ()
                              (if (looking-at "▸") 1
                                (1+ (/ (- (match-end 0) (match-beginning 0)) 2))))
              header-line-format
              (substitute-command-keys
               " \\[org-sm-tree-review] 复习当前(C-u全部)  \\[org-sm-neural-start] 关联漫游  \\[outline-cycle] 折叠  \\[org-sm-tree-goto] 跳转原文  \\[org-sm-tree-revert] 刷新"))
  (outline-minor-mode 1))

(defvar-local org-sm--tree-pred nil "Filter predicate for the current tree, or nil for all.")

(defun org-sm-tree-revert ()
  "Rebuild the tree from disk, keeping the current filter."
  (interactive)
  (org-sm--tree-render org-sm--tree-pred))

(defun org-sm--tree-render (pred)
  "Render the tree buffer, showing only cards satisfying PRED (all if nil)."
  (let ((buf (get-buffer-create "*org-sm tree*"))
        (rows (org-sm-map-items
               (lambda ()
                 (let* ((card  (org-sm-card-at-point))
                        (clean (replace-regexp-in-string
                                "\\`\\[[TC]\\][ \t]*" ""
                                (or (plist-get card :title) ""))))
                   (list (max 1 (1- (org-outline-level)))
                         (truncate-string-to-width clean 44 nil nil "…")
                         (plist-get card :type)
                         (plist-get card :interval-days)
                         (plist-get card :reviews)
                         (org-sm-sched-day-delta)
                         (point-marker))))
               pred)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-sm-tree-mode) (org-sm-tree-mode))
      (setq org-sm--tree-pred pred)
      (let ((inhibit-read-only t) (file nil))
        (erase-buffer)
        (pcase-dolist (`(,depth ,title ,type ,ivl ,rev ,delta ,m) rows)
          (let ((f (buffer-file-name (marker-buffer m))))
            (unless (equal f file)
              (when file (insert "\n"))
              (setq file f)
              (insert (propertize (format "▸ %s\n"
                                          (or (org-sm-book-label f)
                                              (file-name-nondirectory f)))
                                  'face '(:weight bold :foreground "#E6DB74")))))
          (let* ((indent (concat (make-string (* depth 2) ?\s) "├─ "))
                 (badge  (if (eq type 'cloze)
                             (propertize "[C] " 'face '(:foreground "#66D9EF" :weight bold))
                           (propertize "[T] " 'face '(:foreground "#A6E22E" :weight bold))))
                 (stat   (if (eq type 'cloze)
                             (propertize (if ivl (format "%4dd" ivl) "  new")
                                         'face '(:foreground "#66D9EF"))
                           (propertize (format "  ×%-2d" (or rev 0))
                                       'face (if (>= (or rev 0) 5)
                                                 '(:foreground "#F92672" :weight bold)
                                               'shadow))))
                 (due    (pcase delta
                           (`nil "")
                           (0 (propertize "due" 'face 'warning))
                           ((pred (> 0)) (propertize (format "%+dd" delta) 'face 'warning))
                           (_ (propertize (format "%+dd" delta) 'face 'shadow))))
                 (line   (concat (propertize indent 'face 'shadow)
                                 badge
                                 title
                                 (propertize " " 'display '(space :align-to 60))
                                 stat
                                 (propertize " " 'display '(space :align-to 68))
                                 due "\n")))
            (insert (propertize line 'org-sm-marker m))))
        (goto-char (point-min))))
    (pop-to-buffer buf)))

(defun org-sm--tree-filter (days keyword)
  "Return a card predicate combining DAYS and KEYWORD (each optional).
DAYS keeps cards due within that many days (overdue always included);
KEYWORD supports `@cloze', `@topic', forest stages (`@seedling', `@plant',
`@evergreen', `@ancient', `@elder'), monster tiers (`@mob', `@elite',
`@boss', `@demon'), or a regexp matched on the title."
  (lambda ()
    (and (or (not days)
             (when-let* ((d (org-sm-sched-day-delta))) (<= d days)))
         (if (null keyword)
             t
           (let* ((card (org-sm-card-at-point))
                  (type (plist-get card :type))
                  (ivl  (plist-get card :interval-days))
                  (rev  (or (plist-get card :reviews) 0)))
             (pcase keyword
               ("@cloze"     (eq type 'cloze))
               ("@topic"     (eq type 'topic))
               ("@seedling"  (and (eq type 'cloze) (or (null ivl) (< ivl 7))))
               ("@plant"     (and (eq type 'cloze) ivl (<= 7 ivl 29)))
               ("@evergreen" (and (eq type 'cloze) ivl (<= 30 ivl 89)))
               ("@ancient"   (and (eq type 'cloze) ivl (<= 90 ivl 179)))
               ("@elder"     (and (eq type 'cloze) ivl (>= ivl 180)))
               ("@mob"       (and (eq type 'topic) (<= rev 1)))
               ("@elite"     (and (eq type 'topic) (<= 2 rev 4)))
               ("@boss"      (and (eq type 'topic) (<= 5 rev 7)))
               ("@demon"     (and (eq type 'topic) (>= rev 8)))
               (_ (string-match-p keyword (or (plist-get card :title) "")))))))))

;;;###autoload
(defun org-sm-tree (days keyword)
  "Show cards as a foldable tree grouped by file, with optional filters.
Prompts for DAYS (blank = all; N = due within N days, overdue included) and
KEYWORD (blank = any; `@seedling'..`@elder', `@mob'..`@demon', or title regexp)."
  (interactive
   (list (let ((s (read-string "Due within N days (blank = all): ")))
           (unless (string-empty-p s) (string-to-number s)))
         (let ((s (read-string "Filter (@seedling..@elder, @mob..@demon, or regexp): ")))
           (unless (string-empty-p s) s))))
  (org-sm--tree-render
   (when (or days keyword) (org-sm--tree-filter days keyword))))

;;;; ---- Minor modes ---------------------------------------------------------

(defvar-keymap org-sm-mode-map
  :doc "Keys for suggestion underlines in `org-sm-mode' buffers."
  "M-n" #'org-sm-pencil-next
  "M-k" #'org-sm-pencil-dismiss)

;;;###autoload
(define-minor-mode org-sm-mode
  "Font-lock {{cloze}} markers and extracted spans in org-mode buffers."
  :lighter " SRS"
  :keymap org-sm-mode-map
  :group 'org-sm
  (if org-sm-mode
      (font-lock-add-keywords nil org-sm--font-lock-keywords 'append)
    (font-lock-remove-keywords nil org-sm--font-lock-keywords))
  (when (fboundp 'font-lock-flush) (font-lock-flush)))

;;;###autoload
(defvar org-sm-read-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-z") #'org-sm-capture)
    map))

(define-minor-mode org-sm-read-mode
  "Buffer-local keymap layer for `global-org-sm-read-mode'."
  :lighter nil :keymap org-sm-read-mode-map)

;;;###autoload
(define-globalized-minor-mode global-org-sm-read-mode
  org-sm-read-mode (lambda () (org-sm-read-mode 1))
  :group 'org-sm)

(provide 'org-sm)
;;; org-sm.el ends here
