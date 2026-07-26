;;; org-sm.el --- SuperMemo-style incremental reading for org-mode -*- lexical-binding: t -*-

;; Author: Lewis Liu
;; Package-Requires: ((emacs "29.1") (org "9.6") (fsrs "6.0"))
;; Keywords: org, spaced-repetition, incremental-reading

;;; Commentary:
;;
;; Minimal incremental reading for org-mode.  Two card types:
;;   topic  - repeated reading, A-Factor scheduling, no grading.
;;            org priority [#A]/[#B]/[#C] controls interval growth rate.
;;   cloze  - memory testing with {{cloze}} markers, FSRS scheduling.
;;
;; Commands:
;;   org-sm-item-mark      - mark heading as topic or cloze
;;   org-sm-item-extract   - extract region as child topic or cloze
;;   org-sm-item-dismiss   - dismiss item from SRS (tag :dismissed:, keep data)
;;   org-sm-item-undismiss - restore a dismissed item
;;   org-sm-review-start   - start review session
;;   org-sm-review-confirm - confirm topic read / advance cloze state
;;   org-sm-review-abort   - abort review session
;;   org-sm-tree           - foldable aggregated tree of all cards; filter + review in place
;;   org-sm-set-readpoint  - drop an inline read-point anchor at point; review jumps here
;;   org-sm-goto-source    - grep the vault for this card's source (extraction point)
;;   org-sm-capture-topic           - capture region/clipboard as topic.
;;   org-sm-capture-topic-from-input - interactive capture; prompts for heading/body.
;;
;; Two minor modes are provided:
;;   org-sm-mode             - buffer-local; {{cloze}} font-lock (use via :hook)
;;   global-org-sm-read-mode - global; binds M-z to `org-sm-capture-topic'
;;
;; Point `org-sm-directory' at the tree containing your SRS items:
;;
;;   (use-package org-sm
;;     :commands (org-sm-review-start org-sm-item-mark org-sm-item-extract org-sm-tree)
;;     :hook (org-mode . org-sm-mode)
;;     :config
;;     (setq org-sm-directory "~/org/incremental/")
;;     (setq org-sm-capture-file "~/org/inbox.org")
;;     (setq org-sm-capture-olp '("org-sm topics"))
;;     (org-sm-setup-capture)
;;     (global-org-sm-read-mode 1))
;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'org-macs)
(require 'fsrs)

(declare-function org-sm-gptel-explain "org-sm-gptel")
(declare-function org-capture-put-target-region-and-position "org-capture")
(defvar org-capture-templates)

;;;; ---- Customization -------------------------------------------------------

(defgroup org-sm nil
  "SuperMemo-style incremental reading for org-mode."
  :group 'org
  :prefix "org-sm-")

(defcustom org-sm-title-max-length 40
  "Max display-column width for auto-generated heading titles."
  :type 'integer)

(defcustom org-sm-topic-prefix "[T] "
  "Prefix string prepended to auto-generated topic heading titles."
  :type 'string)

(defcustom org-sm-cloze-prefix "[C] "
  "Prefix string prepended to auto-generated cloze heading titles."
  :type 'string)

;;;; ---- File discovery ------------------------------------------------------

(defcustom org-sm-directory nil
  "Root directory scanned by `org-sm-files' for SRS items."
  :type '(choice (const :tag "Unset" nil) directory))

(defun org-sm-files ()
  "Return org files under `org-sm-directory' that contain SRS items.
Rescanned live on each call, so the result never goes stale."
  (when org-sm-directory
    (let ((rg  (or (executable-find "rg")
                   (user-error "org-sm: ripgrep (rg) not found on PATH")))
          (dir (expand-file-name org-sm-directory)))
      (with-temp-buffer
        (call-process rg nil t nil "-l0" "--glob" "*.org" ":SRS_TYPE:" dir)
        (split-string (buffer-string) "\0" t)))))

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

(defvar-local org-sm--cloze-state nil
  "Cloze review state for the current buffer: `hidden' or `revealed'.")

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
   (let* ((ts   (format-time-string (org-time-stamp-format 'long 'inactive)))
          (name (symbol-name type))
          (line (concat "- " name
                        (when rating (format "  %s" rating))
                        (when extra  (format "  %s" extra))
                        (format "  %s\n" ts))))
     (goto-char (org-log-beginning t))
     (insert line))))

;;;; ---- Topic scheduling ----------------------------------------------------

(defun org-sm--topic-afactor ()
  "Return A-Factor for current heading derived from org priority.
[#A] → 1.2 (slow growth, review frequently)
[#B] → 1.5
[#C] or none → 1.8 (fast growth, review less)"
  (pcase (org-entry-get nil "PRIORITY")
    ("A" 1.2) ("B" 1.5) (_ 1.8)))

(defun org-sm--topic-read (a)
  "Calculate the automatic next interval in days for current topic using A-Factor A.
Pure function: reads org properties but writes nothing.
Uses SCHEDULED time as reference to avoid early/late review distortion."
  (let* ((last (org-entry-get nil "SRS_LAST"))
         (ref  (or (org-get-scheduled-time nil) (current-time))))
    (max 1 (round (* a (if last
                           (max 1 (/ (float-time (time-subtract ref (parse-iso8601-time-string last)))
                                     86400.0))
                         1))))))

(defun org-sm--topic-write (ivl)
  "Apply IVL (days) to current topic: update SCHEDULED and SRS_LAST.
 This is a real repetition: SRS_LAST is set to now, SCHEDULED to now+IVL."
  (org-sm--schedule (time-add (current-time) (days-to-time ivl)))
  (org-entry-put nil "SRS_LAST" (format-time-string "%FT%TZ" (current-time) "UTC0")))

(defun org-sm--topic-postpone (days)
  "Postpone current topic by DAYS without counting as a repetition.
Both SRS_LAST and SCHEDULED are shifted forward by DAYS, preserving
the interval between them so A-Factor calculation is unaffected."
  (let* ((last-str  (org-entry-get nil "SRS_LAST"))
         (sched     (or (org-get-scheduled-time nil) (current-time)))
         (shift     (days-to-time days))
         (new-sched (time-add sched shift))
         (new-last  (if last-str
                        (time-add (parse-iso8601-time-string last-str) shift)
                      (time-subtract new-sched (days-to-time 1)))))
    (org-sm--schedule new-sched)
    (org-entry-put nil "SRS_LAST"
                   (format-time-string "%FT%TZ" new-last "UTC0"))))

;;;; ---- Grading (pure business layer, no UI) --------------------------------
;; Single source of truth for "what a review does": update scheduling, log it,
;; return a result plist.  Shared by the interactive command and other front-ends.

(defun org-sm--topic-grade ()
  "Reschedule current topic by its A-Factor and log the repetition.
Return plist (:interval-days N :afactor A)."
  (let* ((a   (org-sm--topic-afactor))
         (ivl (org-sm--topic-read a)))
    (org-sm--topic-write ivl)
    (org-sm--log-review 'topic nil (format "a=%.1f  %2dd" a ivl))
    (list :interval-days ivl :afactor a)))

(defun org-sm--topic-postpone-grade (days)
  "Postpone current topic by DAYS (not a repetition) and log it.
Return plist (:interval-days DAYS)."
  (org-sm--topic-postpone days)
  (org-sm--log-review 'topic nil (format "postpone  %2dd" days))
  (list :interval-days days))

;;;; ---- Cloze overlays ------------------------------------------------------

(defvar org-sm--cloze-regexp "{{\\([^}\n]+\\)}}"
  "Regexp matching {{cloze}} markup; group 1 is the answer.")

(defface org-sm-cloze-face '((t :inherit bold :box t))
  "Face for {{cloze}} markers during normal reading."
  :group 'org-sm)

(defvar org-sm--font-lock-keywords
  (let ((re "\\({{\\)\\([^}\n]+\\)\\(}}\\)"))
    `((,re
       (1 '(face nil display "") prepend)   ; hide "{{"
       (2 'org-sm-cloze-face    prepend)    ; show answer with face
       (3 '(face nil display "") prepend)))) ; hide "}}"
  "Font-lock keywords for {{cloze}} markers; cached for reliable add/remove.")

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

(defconst org-sm-cloze-ratings '(:again :hard :good :easy)
  "Ordered list of FSRS ratings offered for cloze review.")

(defun org-sm--cloze-preview ()
  "Preview every rating for the current cloze without modifying anything.
Return an alist mapping each rating symbol to a plist
(:card CARD :interval-secs SECS), where CARD is the hypothetical FSRS card
and SECS the resulting interval.  Pure: reads properties, writes nothing."
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

(defun org-sm--cloze-grade (rating &optional preview)
  "Apply RATING (a symbol in `org-sm-cloze-ratings') to the current cloze.
Write the new FSRS card, log the review, and return a plist
(:rating R :interval-secs SECS :due ISO).  PREVIEW, if given, is the
alist from `org-sm--cloze-preview', reused to avoid recomputing."
  (let* ((entry (or (cdr (assq rating (or preview (org-sm--cloze-preview))))
                    (error "Unknown cloze rating: %S" rating)))
         (card  (plist-get entry :card))
         (secs  (plist-get entry :interval-secs))
         (days  (fsrs-seconds-days secs))
         (extra (if (< days 1)
                    (format "%s  %2dm" rating (round (/ secs 60)))
                  (format "%s  %2dd" rating (round days)))))
    (org-sm--cloze-write card)
    (org-sm--log-review 'cloze nil extra)
    (list :rating rating :interval-secs secs :due (fsrs-card-due card))))

;;;; ---- Heading helpers -----------------------------------------------------

(defun org-sm-type ()
  "Return SRS_TYPE of current heading as a symbol, or nil."
  (when-let* ((v (org-entry-get nil "SRS_TYPE"))) (intern v)))

(defun org-sm--card-p ()
  "Return non-nil if the current heading is an SRS card."
  (org-entry-get nil "SRS_TYPE"))

(defun org-sm--goto-enclosing-card ()
  "Move point to the nearest enclosing card heading (current or ancestor).
Return its start position, or nil if no ancestor is a card."
  (save-match-data
    (unless (org-at-heading-p) (org-back-to-heading t))
    (let (pos)
      (while (and (not pos) (org-at-heading-p))
        (if (org-sm--card-p)
            (setq pos (point))
          (unless (org-up-heading-safe) (goto-char (point-min)))))
      pos)))

(defun org-sm--body-bounds ()
  "Return (start . end) of current heading's body (excluding meta-data)."
  (save-excursion
    (org-back-to-heading t)
    (cons (progn (org-end-of-meta-data t) (point))
          (org-entry-end-position))))

(defun org-sm--body-clean (raw)
  "Trim surrounding whitespace from RAW."
  (string-trim raw))

(defun org-sm--cloze-markers-p ()
  "Return non-nil if current heading body contains {{cloze}} markers."
  (let ((bounds (org-sm--body-bounds)))
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
  "Target org file for `org-sm-capture-topic'."
  :type '(choice (const nil) file)
  :group 'org-sm)

(defcustom org-sm-capture-olp '("org-sm topics")
  "Outline path for the capture target (list of heading strings).
The card is inserted as a child of the deepest heading in the path.
Example: \\='(\"Topics\" \"Physics\")"
  :type '(repeat string)
  :group 'org-sm)

(defvar org-sm--pending-content ""
  "Temporary storage for capture template content.")

;;;###autoload
(defun org-sm-setup-capture ()
  "Register org-sm capture templates.
Call once after setting `org-sm-capture-file' and `org-sm-capture-olp'.

  org-sm-topic            - capture clipboard/region as topic (original).
  org-sm-topic-from-input - interactive capture; prompts for heading and body."
  (add-to-list 'org-capture-templates
               '("org-sm-topic" "org-sm topic" entry
                 (function org-sm--capture-goto-olp)
                 "** %(org-sm--truncate-title org-sm--pending-content)\n%(identity org-sm--pending-content)\n\n- source: %a"
                 :before-finalize (lambda () (org-sm-item-mark 'topic))))
  (add-to-list 'org-capture-templates
               '("org-sm-topic-from-input" "org-sm topic from input" entry
                 (function org-sm--capture-goto-olp)
                 "** %?\n:PROPERTIES:\n:PSA_FEELING: 记下这张卡片的感受\n:END:\n"
                 :before-finalize (lambda () (org-sm-item-mark 'topic)))))

(defun org-sm--capture-goto-olp ()
  "Jump to `org-sm-capture-olp' in `org-sm-capture-file' for org-capture."
  (let ((m (org-find-olp (cons (expand-file-name org-sm-capture-file) org-sm-capture-olp))))
    (set-buffer (marker-buffer m))
    (org-capture-put-target-region-and-position)
    (widen)
    (goto-char m)
    (set-marker m nil)))

;;;###autoload
(defun org-sm-capture-topic (&optional ask-file)
  "Capture region or clipboard as a topic card.
Triggers `M-w' when a region is active or in `reader-mode' (where selection
is not tracked by Emacs region).  Otherwise reads the clipboard directly.
With prefix arg, prompt to update `org-sm-capture-file' and `org-sm-capture-olp'."
  (interactive "P")
  (when ask-file
    (setq org-sm-capture-file
          (if (eq major-mode 'org-mode)
              (buffer-file-name)
            (read-file-name "Capture to file: " nil nil t nil
                            (lambda (f) (string-match-p "\\.org$" f)))))
    (org-sm--select-capture-olp))
  (unless org-sm-capture-file
    (user-error "Set `org-sm-capture-file' and call `org-sm-setup-capture' first"))
  (when (or (use-region-p) (derived-mode-p 'reader-mode))
    (execute-kbd-macro (kbd "M-w")))
  (setq org-sm--pending-content
        (substring-no-properties
         (or (ignore-errors (current-kill 0 t)) "")))
  (org-capture nil "org-sm-topic"))

;;;###autoload
(defun org-sm-capture-topic-from-input ()
  "Capture a new topic card by interactively entering heading and body."
  (interactive)
  (unless org-sm-capture-file
    (user-error "Set `org-sm-capture-file' and call `org-sm-setup-capture' first"))
  (org-capture nil "org-sm-topic-from-input"))

(defun org-sm--select-capture-olp ()
  "Prompt to select a heading in `org-sm-capture-file'; set `org-sm-capture-olp'."
  (with-current-buffer (or (find-buffer-visiting org-sm-capture-file)
                           (find-file-noselect org-sm-capture-file))
    (let* ((entries (org-map-entries
                     (lambda ()
                       (let ((olp (org-get-outline-path t)))
                         (cons (org-format-outline-path olp) olp)))
                     nil 'file))
           (choice (completing-read "Select heading: " (mapcar #'car entries) nil t)))
      (setq org-sm-capture-olp (cdr (assoc choice entries)))
      (message "org-sm capture olp: %s" org-sm-capture-olp))))
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
    (message "org-sm: marked as %s%s — due tomorrow" type
             (if (and (eq type 'cloze) (not (org-sm--cloze-markers-p)))
                 " (⚠ no {{cloze}} markers found)" ""))))

(defun org-sm--insert-child (level type title body id)
  "Append a scheduled + inited SRS heading after point's subtree; return ID.
Insert a heading at LEVEL+1 at the end of the current subtree, titled TITLE,
typed TYPE, tagged with ID, scheduled for tomorrow, and filled with BODY.
Point-relative and excursion-agnostic: the caller owns buffer/point state
(`save-excursion', `org-with-wide-buffer', etc.) and computes LEVEL."
  (org-end-of-subtree t t)
  (unless (bolp) (insert "\n"))
  (org-insert-heading nil t (1+ level))
  (insert title)
  ;; Schedule before org-set-property / org-sm--init-item: those move point
  ;; into the PROPERTIES drawer, after which org-schedule would climb up to
  ;; the previous sibling heading (see `org-sm--schedule').
  (org-sm--schedule (time-add (current-time) (days-to-time 1)))
  (org-set-property "ID" id)
  (org-sm--init-item type)
  (org-end-of-meta-data t)
  (unless (bolp) (insert "\n"))
  (insert body "\n")
  id)

(defun org-sm--extract (type selected sel-start sel-end)
  "Create a child SRS card of TYPE from the parent at point.
Point must be on the parent heading.  SELECTED is the extracted text.
SEL-START and SEL-END are its character offsets within the parent body
\(as returned by `org-sm--body-bounds').  Insert a back-reference at the
selection, append a scheduled + inited child heading, and return the new
child's id.  Pure buffer logic: no region, no prompt, no overlay/UI.

- topic: child body is SELECTED verbatim; title is the topic prefix +
         first N chars of SELECTED.
- cloze: child body is the full parent body with SELECTED wrapped as
         {{answer}} at SEL-START; title is the cloze prefix + first N
         chars of the parent body."
  (let* ((level    (org-current-level))
         (bounds   (org-sm--body-bounds))
         (body-raw (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (id       (org-id-new))
         (title    (pcase type
                     ('topic (concat org-sm-topic-prefix
                                     (org-sm--truncate-title selected)))
                     ('cloze (concat org-sm-cloze-prefix
                                     (org-sm--truncate-title body-raw)))))
         (child-body (pcase type
                       ('topic selected)
                       ('cloze (org-sm--body-clean
                                (concat (substring body-raw 0 sel-start)
                                        (format "{{%s}}" selected)
                                        (substring body-raw sel-end)))))))
    ;; Insert a back-reference after the selection; parent text stays intact.
    (goto-char (+ (car bounds) sel-end))
    (insert (format "[[id:%s][%s]]" id (pcase type ('topic "<T>") ('cloze "<C>"))))
    ;; Append child heading at end of current subtree.
    (save-excursion
      (org-sm--insert-child level type title child-body id))))

(defun org-sm--capture (type body &optional file olp title)
  "Create a new SRS card of TYPE with BODY under FILE / OLP; return its id.
Insert a scheduled + inited heading as the last child of the deepest heading
in OLP and fill it with BODY (via `org-sm--insert-child').  Like
`org-sm--extract' but with no parent and no back-reference; saves nothing.
FILE/OLP default to `org-sm-capture-file'/`org-sm-capture-olp'; TITLE, when
blank, is derived from BODY."
  (let* ((body   (org-sm--body-clean (or body "")))
         (id     (org-id-new))
         (prefix (pcase type
                   ('topic org-sm-topic-prefix)
                   ('cloze org-sm-cloze-prefix)
                   (_ (error "Unknown capture type: %S" type))))
         (title  (concat prefix
                         (if (and title (org-string-nw-p title))
                             (string-trim title)
                           (org-sm--truncate-title body))))
         (target (org-find-olp (cons (expand-file-name (or file org-sm-capture-file))
                                     (or olp org-sm-capture-olp)))))
    (unless target (user-error "Capture target not found"))
    (set-buffer (marker-buffer target))
    (org-with-wide-buffer
     (goto-char target)
     (set-marker target nil)
     (org-sm--insert-child (org-current-level) type title body id))))

;;;###autoload
(defun org-sm-item-extract ()
  "Extract the active region as a child topic or cloze heading.
Prompts for the card type, then delegates to `org-sm--extract'.  The
parent body is not modified; a back-reference link is inserted at the
selection."
  (interactive)
  (unless (region-active-p) (user-error "Select text to extract first"))
  (let* ((bounds   (org-sm--body-bounds))
         (sel-start (- (region-beginning) (car bounds)))
         (sel-end   (- (region-end) (car bounds)))
         (selected  (buffer-substring-no-properties
                     (region-beginning) (region-end)))
         (type      (intern (completing-read "Extract as: " '("topic" "cloze") nil t))))
    (org-sm--extract type selected sel-start sel-end))
  (when (eq org-sm--cloze-state 'revealed)
    (org-sm-cloze-remove-overlays)
    (org-sm-cloze-apply-overlays)))

;;;; ---- Review session ------------------------------------------------------

(defun org-sm--dismissed-p ()
  "Return non-nil if current heading carries the :dismissed: tag."
  (member "dismissed" (org-get-tags nil t)))

(defun org-sm--sched-day-delta ()
  "Return whole-day offset from today to the current heading's SCHEDULED time.
Negative means overdue, 0 means today, positive means upcoming; nil when the
heading has no SCHEDULED time."
  (when-let* ((s (org-get-scheduled-time nil)))
    (- (time-to-days s) (time-to-days (current-time)))))

(defun org-sm--due-p ()
  "Return non-nil if current heading is a due (scheduled today or earlier) card."
  (and (org-sm-type)
       (not (org-sm--dismissed-p))
       (when-let* ((d (org-sm--sched-day-delta))) (<= d 0))))

(defun org-sm--map-items (fn &optional pred)
  "Map FN over SRS headings (skipping dismissed ones); collect non-nil results.
When PRED is non-nil, only entries for which it returns non-nil are visited."
  (delq nil (org-map-entries
             (lambda ()
               (when (and (org-sm-type)
                          (not (org-sm--dismissed-p))
                          (or (null pred) (funcall pred)))
                 (funcall fn)))
             nil
             (org-sm-files))))

(defun org-sm--collect-markers (&optional pred)
  "Return priority-sorted markers for SRS cards satisfying PRED (all if nil)."
  (mapcar #'cdr
          (sort (org-sm--map-items
                 (lambda () (cons (org-get-priority (org-get-heading t t t t))
                                  (point-marker)))
                 pred)
                (lambda (a b) (> (car a) (car b))))))

(defun org-sm--collect-due-markers ()
  "Return priority-sorted markers for all due SRS cards."
  (org-sm--collect-markers #'org-sm--due-p))

(defun org-sm--cleanup-buffer ()
  "Clear cloze overlays and state in the previously reviewed buffer."
  (when (buffer-live-p org-sm--current-buffer)
    (with-current-buffer org-sm--current-buffer
      (when org-sm--cloze-state
        (org-sm-cloze-remove-overlays)
        (setq org-sm--cloze-state nil))))
  (setq org-sm--current-buffer nil))

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
  (org-narrow-to-subtree)
  (org-fold-hide-subtree)
  ;; Topics are structured reading material: show the entry body plus child
  ;; heading lines (bodies stay folded) so the outline is visible.
  ;; Clozes are atomic leaf cards: show only the entry body.  We never extract
  ;; sub-headings under a cloze, so hiding branches avoids both leaks and
  ;; visual noise on the rare occasion one exists.
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

(defun org-sm--show-prompt (&optional prev)
  "Show review hint in the echo area, optionally prefixed with PREV result."
  (let ((pre (if prev (format "✓ %s  |  " prev) "")))
    (pcase (org-sm-type)
      ('cloze (message "org-sm %s[%d left] cloze: %s — M-x review-confirm" pre (length org-sm--queue)
                       (if (eq org-sm--cloze-state 'hidden) "hidden" "revealed")))
      ('topic (message "org-sm %s[%d left] topic — M-x review-confirm" pre (length org-sm--queue)))
      (_      (message "org-sm: not an SRS heading")))))

(defun org-sm--advance (&optional prev)
  "Advance to the next queued item, or end the session.
PREV is a string describing the last action, shown in the echo area."
  (when (buffer-narrowed-p) (widen))
  (let (found)
    (while (and org-sm--queue (not found))
      (let ((marker (pop org-sm--queue)))
        (if (marker-buffer marker)
            (progn (org-sm--goto-marker marker)
                   (when (org-sm-type) (setq found t)))
          (message "org-sm: skipping stale marker"))))
    (if found
        (org-sm--show-prompt prev)
      (org-sm--cleanup-buffer)
      (message "org-sm: done 󱁖 %s" (if prev (format "  (last: %s)" prev) "")))))

(defun org-sm--review-markers (markers &optional what)
  "Start a review session over MARKERS (a list).  WHAT names them in messages."
  (org-sm--ensure-scheduler)
  (if (null markers)
      (message "org-sm: nothing to review%s 󱁖" (if what (format " (%s)" what) ""))
    (setq org-sm--queue markers)
    (org-sm--advance)))

;;;###autoload
(defun org-sm-review-start ()
  "Collect all due SRS items and start a review session."
  (interactive)
  (org-sm--review-markers (org-sm--collect-due-markers) "due"))

(defun org-sm--review-confirm-topic ()
  "Interactive topic review: reschedule, postpone, explain, or dismiss.
Computation is delegated to the pure grading functions; this only
collects the user's choice and advances the queue."
  (let* ((auto-ivl (org-sm--topic-read (org-sm--topic-afactor)))
         (choices  `((?r ,(format "rsch(%dd)" auto-ivl))
                     (?p "postpone")
                     (?e "explain"))))
    (org-sm--prompt-choice "Topic: " choices
      (lambda (key)
        (pcase key
          (?r (let ((r (org-sm--topic-grade)))
                (org-sm--advance (format "topic → %d days" (plist-get r :interval-days)))))
          (?p (let ((r (org-sm--topic-postpone-grade (read-number "Postpone days: " 7))))
                (org-sm--advance (format "topic postpone → %d days" (plist-get r :interval-days)))))
          (?e (if (fboundp 'org-sm-gptel-explain)
                  (progn (org-sm--log-review 'topic nil "explain")
                         (org-sm-gptel-explain))
                (user-error "org-sm-gptel not loaded"))))))))

(defun org-sm--cloze-rating-label (rating secs)
  "Format a menu LABEL for RATING with interval SECS."
  (let ((days (fsrs-seconds-days secs)))
    (if (< days 1)
        (format "%s(%dm)" rating (round (/ secs 60)))
      (format "%s(%dd)" rating days))))

(defun org-sm--review-confirm-cloze ()
  "Interactive cloze review: reveal, then rate (or dismiss).
Rating computation is delegated to `org-sm--cloze-preview' and
`org-sm--cloze-grade'; this only handles overlays and the queue."
  (pcase org-sm--cloze-state
    ('hidden
     (org-sm-cloze-reveal-overlays)
     (setq org-sm--cloze-state 'revealed)
     (message "org-sm: cloze revealed — edit if needed, M-x review-confirm to rate"))
    ('revealed
     (org-sm-cloze-remove-overlays)
     (let* ((preview (org-sm--cloze-preview))
            ;; Map menu key → rating symbol; keys stay stable (r = reveal, r = good).
            (keymap  '((?a . :again) (?h . :hard) (?r . :good) (?e . :easy)))
            (choices (mapcar
                      (lambda (kr)
                        (let ((secs (plist-get (cdr (assq (cdr kr) preview)) :interval-secs)))
                          (list (car kr) (org-sm--cloze-rating-label (cdr kr) secs))))
                      keymap)))
       (org-sm--prompt-choice "Rate: " choices
         (lambda (key)
           (let* ((rating (cdr (assq key keymap)))
                  (r      (org-sm--cloze-grade rating preview))
                  (due    (format-time-string
                           "%F %H:%M" (parse-iso8601-time-string (plist-get r :due)))))
             (org-sm--advance (format "cloze %s → %s" rating due)))))))))

;;;###autoload
(defun org-sm-review-confirm ()
  "Confirm topic read or advance cloze state, then move to next item."
  (interactive)
  (org-sm--ensure-scheduler)
  (unless (or org-sm--queue org-sm--cloze-state)
    (user-error "No active review session — call org-sm-review-start"))
  (pcase (or (org-sm-type) (user-error "Not on an SRS heading"))
    ('topic (org-sm--review-confirm-topic))
    ('cloze (org-sm--review-confirm-cloze))))


(defun org-sm--dismiss ()
  "Tag the current item :dismissed: and log it.  Pure data operation, no UI.
Signals an error if not on an SRS heading or already dismissed.  All SRS
properties and scheduling data are preserved for `org-sm-item-undismiss'."
  (unless (org-sm-type) (user-error "Not on an SRS heading"))
  (when (org-sm--dismissed-p) (user-error "Already dismissed"))
  (org-toggle-tag "dismissed" 'on)
  (org-sm--log-review 'dismissed))

;;;###autoload
(defun org-sm-item-dismiss ()
  "Dismiss current SRS item, advancing the queue if a session is active."
  (interactive)
  (org-sm--dismiss)
  ;; Only advance the review queue if a session is active.
  (if org-sm--queue
      (org-sm--advance "dismissed")
    (message "org-sm: item dismissed")))

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
    (org-sm--readpoint-remove id)
    (insert (org-sm--readpoint-anchor id))
    (message "org-sm: read point set")))

(defun org-sm--readpoint-target ()
  "Return the position of the current card's read-point anchor, or nil.
Point must be on the card heading."
  (when-let* ((id (org-id-get)))
    (save-excursion
      (let ((end (save-excursion (org-end-of-subtree t t) (point))))
        (when (search-forward (org-sm--readpoint-anchor id) end t)
          (match-beginning 0))))))


;;;; ---- Source navigation ---------------------------------------------------
;; A card's source is wherever its own ID is linked — `org-sm--extract' leaves an
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
  "Abort the current review session, cleaning up state and overlays."
  (interactive)
  (unless (or org-sm--queue (buffer-live-p org-sm--current-buffer))
    (user-error "No active review session"))
  (setq org-sm--queue nil)
  (org-sm--cleanup-buffer)
  (when (buffer-narrowed-p) (widen))
  (message "org-sm: review aborted"))

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
        (end (if all (point-max) (save-excursion (org-end-of-subtree t t) (point)))))
    (org-sm--review-markers (org-sm--tree-markers beg end) "tree")))

(defvar org-sm-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-sm-tree-goto)
    (define-key map "r" #'org-sm-tree-review)
    (define-key map "g" #'org-sm-tree-revert)
    map)
  "Keymap for `org-sm-tree-mode' (native org folding keys still apply).")

(define-derived-mode org-sm-tree-mode org-mode "org-sm-tree"
  "Read-only aggregated tree of all SRS cards; RET jumps, r/C-u r review."
  (setq buffer-read-only t))

(defvar-local org-sm--tree-pred nil "Filter predicate for the current tree, or nil for all.")

(defun org-sm-tree-revert ()
  "Rebuild the tree from disk, keeping the current filter."
  (interactive)
  (org-sm--tree-render org-sm--tree-pred))

(defun org-sm--tree-render (pred)
  "Render the tree buffer, showing only cards satisfying PRED (all if nil)."
  (let ((buf (get-buffer-create "*org-sm tree*"))
        (rows (org-sm--map-items
               (lambda ()
                 (list (make-string (org-outline-level) ?*)
                       (org-get-heading t t t t)
                       (org-sm--sched-day-delta)
                       (org-entry-get nil "PRIORITY")
                       (point-marker)))
               pred)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-sm-tree-mode) (org-sm-tree-mode))
      (setq org-sm--tree-pred pred)
      (let ((inhibit-read-only t) (file nil))
        (erase-buffer)
        (pcase-dolist (`(,stars ,title ,delta ,prio ,m) rows)
          (let ((f (buffer-file-name (marker-buffer m))))
            (unless (equal f file)
              (setq file f)
              (insert (format "* %s\n" (file-name-nondirectory f)))))
          (insert (propertize
                   (format "*%s %s%s%s\n" stars title
                           (if prio (format " [#%s]" prio) "")
                           (pcase delta (`nil "") (0 " [today]") (_ (format " [%+dd]" delta))))
                   'org-sm-marker m)))
        (goto-char (point-min))
        (org-fold-show-all)))
    (pop-to-buffer buf)))

(defun org-sm--tree-filter (days keyword)
  "Return a card predicate combining DAYS and KEYWORD (each optional).
DAYS keeps cards due within that many days (overdue always included);
KEYWORD is a regexp matched against the title."
  (lambda ()
    (and (or (not days)
             (when-let* ((d (org-sm--sched-day-delta))) (<= d days)))
         (or (not keyword)
             (string-match-p keyword (org-get-heading t t t t))))))

;;;###autoload
(defun org-sm-tree (days keyword)
  "Show cards as a foldable tree grouped by file, with optional filters.
Prompts for DAYS (blank = all; N = due within N days, overdue included) and
KEYWORD (blank = any; a regexp on the title)."
  (interactive
   (list (let ((s (read-string "Due within N days (blank = all): ")))
           (unless (string-empty-p s) (string-to-number s)))
         (let ((s (read-string "Title keyword regexp (blank = any): ")))
           (unless (string-empty-p s) s))))
  (org-sm--tree-render
   (when (or days keyword) (org-sm--tree-filter days keyword))))

;;;; ---- Minor modes ---------------------------------------------------------

;;;###autoload
(define-minor-mode org-sm-mode
  "Font-lock {{cloze}} markers in org-mode buffers."
  :lighter " SRS"
  :group 'org-sm
  (if org-sm-mode
      (font-lock-add-keywords nil org-sm--font-lock-keywords 'append)
    (font-lock-remove-keywords nil org-sm--font-lock-keywords))
  (when (fboundp 'font-lock-flush) (font-lock-flush)))

;;;###autoload
(defvar org-sm-read-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-z") #'org-sm-capture-topic)
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
