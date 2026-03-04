;;; org-sm.el --- SuperMemo-style incremental reading for org-mode -*- lexical-binding: t -*-

;; Author: Lewis Liu
;; Package-Requires: ((emacs "29.1") (org "9.6") (fsrs "6.0"))
;; Keywords: org, spaced-repetition, incremental-reading

;;; Commentary:
;;
;; A minimal incremental reading system for org-mode, inspired by SuperMemo.
;;
;; Two card types:
;;   topic  - for repeated reading, scheduled with A-Factor algorithm.
;;            No explicit grading: confirming you've read the topic is enough.
;;            Use org priority [#A]/[#B]/[#C] to express interest level.
;;   cloze  - for memory testing, scheduled with FSRS.
;;
;; Usage:
;;   org-sm-extract        (C-c s e)   - extract selection as topic or cloze
;;   org-sm-review-confirm (C-c s SPC) - confirm topic read / advance cloze state
;;   org-sm-dismiss        (C-c s d)   - dismiss current item
;;
;; Setup:
;;   (require 'org-sm)
;;   (add-hook 'org-mode-hook #'org-sm-mode)
;;   (org-sm-setup)

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'fsrs)

;;;; ---- Customization -------------------------------------------------------

(defgroup org-sm nil
  "SuperMemo-style incremental reading for org-mode."
  :group 'org
  :prefix "org-sm-")

(defcustom org-sm-title-max-length 40
  "Maximum display-column width for auto-generated heading titles."
  :type 'integer
  :group 'org-sm)

(defcustom org-sm-initial-afactor 1.2
  "Initial A-Factor for new topic cards."
  :type 'float
  :group 'org-sm)

(defcustom org-sm-dismiss-date "2999-12-31"
  "Date string used to mark dismissed items (SCHEDULED far future)."
  :type 'string
  :group 'org-sm)

(defcustom org-sm-files nil
  "List of org files containing SRS items.
When non-nil, overrides `org-agenda-files' for the SRS agenda view.
When nil, `org-agenda-files' is used (the recommended setup)."
  :type '(repeat file)
  :group 'org-sm)

;;;; ---- FSRS scheduler ------------------------------------------------------

(defvar org-sm--scheduler
  (fsrs-make-scheduler
   :desired-retention 0.9
   :learning-steps    '((1 :minute) (10 :minute))
   :enable-fuzzing-p  t)
  "Global FSRS scheduler instance.")

;;;; ---- Property helpers ----------------------------------------------------

(defun org-sm-get (prop)
  "Get SRS property PROP at point's heading.  Return nil if absent or empty."
  (let ((v (org-entry-get nil prop)))
    (and v (not (string-empty-p v)) v)))

(defun org-sm-set (prop val)
  "Set SRS property PROP to VAL at point's heading."
  (org-entry-put nil prop (or val "")))

(defun org-sm-type ()
  "Return SRS_TYPE of current heading as a symbol, or nil."
  (when-let* ((v (org-sm-get "SRS_TYPE")))
    (intern v)))

;;;; ---- FSRS card read / write ----------------------------------------------

(defun org-sm--read-card ()
  "Build an `fsrs-card' from the current heading's properties."
  (fsrs-make-card
   :state       (if-let* ((s (org-sm-get "SRS_STATE"))) (read s) :learning)
   :stability   (when-let* ((s (org-sm-get "SRS_STABILITY")))   (string-to-number s))
   :difficulty  (when-let* ((d (org-sm-get "SRS_DIFFICULTY")))  (string-to-number d))
   :last-review (org-sm-get "SRS_LAST")
   :due         (or (when-let* ((t_ (org-get-scheduled-time nil)))
                      (format-time-string "%FT%TZ" t_ "UTC0"))
                    (fsrs-now))))

(defun org-sm--write-card (card)
  "Persist CARD state to the current heading's properties and SCHEDULED."
  (org-sm-set "SRS_STATE"      (prin1-to-string (fsrs-card-state card)))
  (org-sm-set "SRS_STABILITY"  (when-let* ((s (fsrs-card-stability card)))
                                  (number-to-string s)))
  (org-sm-set "SRS_DIFFICULTY" (when-let* ((d (fsrs-card-difficulty card)))
                                  (number-to-string d)))
  (org-sm-set "SRS_LAST"       (fsrs-card-last-review card))
  (org-schedule nil (format-time-string
                     "%F" (parse-iso8601-time-string (fsrs-card-due card)))))

;;;; ---- Topic A-Factor scheduling ------------------------------------------

(defun org-sm--topic-afactor ()
  "Return A-Factor of current topic heading, defaulting to `org-sm-initial-afactor'."
  (string-to-number (or (org-sm-get "SRS_AFACTOR")
                        (number-to-string org-sm-initial-afactor))))

(defun org-sm--topic-last-interval ()
  "Return the interval in days between SRS_LAST and SCHEDULED for current topic.
Falls back to 1 day if either timestamp is absent."
  (let ((last      (org-sm-get "SRS_LAST"))
        (scheduled (org-get-scheduled-time nil)))
    (if (and last scheduled)
        (max 1 (round (/ (float-time
                          (time-subtract scheduled
                                         (date-to-time last)))
                         86400.0)))
      1)))

(defun org-sm--topic-review ()
  "Reschedule current topic using the A-Factor algorithm.
No grade is needed: confirming you have read the topic is sufficient.
The A-Factor grows automatically each review, stretching the interval."
  (let* ((a     (org-sm--topic-afactor))
         (new-a (max 1.2 (min 6.9 (+ (* 1.18 a) -0.20))))
         (ivl   (max 1 (ceiling (* (org-sm--topic-last-interval) new-a)))))
    (org-sm-set "SRS_AFACTOR" (number-to-string new-a))
    (org-sm-set "SRS_LAST"    (format-time-string "%FT%TZ" (current-time) "UTC0"))
    (org-schedule nil (format-time-string
                       "%F" (time-add (current-time) (days-to-time ivl))))))

;;;; ---- Cloze overlay -------------------------------------------------------

(defvar org-sm--cloze-regexp
  "\\(?:^\\|[ \t]\\)\\(~\\([^~\n]+\\)~\\)"
  "Regexp matching org ~verbatim~ markup used as cloze markers.
Group 1 = full ~text~, group 2 = text inside tildes.")

(defun org-sm--cloze-overlays-in (start end)
  "Return all org-sm-cloze overlays between START and END."
  (cl-remove-if-not (lambda (ov) (eq (overlay-get ov 'category) 'org-sm-cloze))
                    (overlays-in start end)))

(defun org-sm-cloze-apply-overlays ()
  "Replace all ~cloze~ markers in current heading with [___] overlays."
  (let ((start (org-entry-beginning-position))
        (end   (org-entry-end-position)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward org-sm--cloze-regexp end t)
        (let ((ov (make-overlay (match-beginning 1) (match-end 1))))
          (overlay-put ov 'category     'org-sm-cloze)
          (overlay-put ov 'display      (propertize "[___]" 'face '(bold highlight)))
          (overlay-put ov 'org-sm-answer (match-string 2)))))))

(defun org-sm-cloze-reveal-overlays ()
  "Show answers in all cloze overlays of the current heading."
  (dolist (ov (org-sm--cloze-overlays-in (org-entry-beginning-position)
                                          (org-entry-end-position)))
    (overlay-put ov 'display
                 (propertize (format "[%s]" (overlay-get ov 'org-sm-answer))
                             'face 'success))))

(defun org-sm-cloze-remove-overlays ()
  "Remove all cloze overlays from the current heading."
  (mapc #'delete-overlay
        (org-sm--cloze-overlays-in (org-entry-beginning-position)
                                    (org-entry-end-position))))

;;;; ---- Heading body helpers ------------------------------------------------

(defun org-sm--body-bounds ()
  "Return (start . end) of current heading's body (after meta-data, before next heading)."
  (save-excursion
    (org-back-to-heading t)
    (cons (progn (org-end-of-meta-data t) (point))
          (org-entry-end-position))))

(defun org-sm--body-clean (raw)
  "Strip leading whitespace from every line of RAW and trim the whole string."
  (mapconcat #'string-trim-left
             (split-string (string-trim raw) "\n")
             "\n"))

(defun org-sm--cloze-body-at (sel-beg sel-end selected)
  "Return parent body text with only the selection at SEL-BEG..SEL-END cloze-marked.
SELECTED is the selected text.  All other occurrences are left untouched."
  (let* ((bounds     (org-sm--body-bounds))
         (body-start (car bounds))
         (body-raw   (buffer-substring-no-properties body-start (cdr bounds)))
         (offset     (- sel-beg body-start)))
    (org-sm--body-clean
     (concat (substring body-raw 0 offset)
             (format "~%s~" selected)
             (substring body-raw (+ offset (length selected)))))))

(defun org-sm--truncate-title (str)
  "Return first line of STR truncated to `org-sm-title-max-length' display columns."
  (let* ((line  (car (split-string (string-trim str) "\n")))
         (limit (- org-sm-title-max-length 1))     ; reserve 1 col for ellipsis
         (width (string-width line)))
    (if (<= width org-sm-title-max-length)
        line
      ;; Binary-search the cut point: find largest n where string-width<=limit
      (let ((lo 0) (hi (length line)))
        (while (< lo hi)
          (let ((mid (/ (+ lo hi 1) 2)))
            (if (<= (string-width (substring line 0 mid)) limit)
                (setq lo mid)
              (setq hi (1- mid)))))
        (concat (substring line 0 lo) "…")))))

;;;; ---- Extract -------------------------------------------------------------

;;;###autoload
(defun org-sm-extract ()
  "Extract the selected region as a child heading.

Prompts for type:
  topic - child body is the selected text itself.
  cloze - child body is the full parent body; only the selected
          instance is wrapped as a ~cloze~.

The selected region in the parent is replaced with an [[id:...][text]] link."
  (interactive)
  (unless (region-active-p)
    (user-error "Select text to extract first"))
  (let* ((sel-beg  (region-beginning))
         (sel-end  (region-end))
         (selected (buffer-substring-no-properties sel-beg sel-end))
         (type     (intern (completing-read "Extract as: "
                                            '("topic" "cloze") nil t)))
         ;; Capture cloze body before the buffer is modified
         (child-body (pcase type
                       ('cloze (org-sm--cloze-body-at sel-beg sel-end selected))
                       ('topic selected)))
         (id       (org-id-new))
         (level    (org-current-level))
         (due      (format-time-string
                    "%F" (time-add (current-time) (days-to-time 1)))))
    ;; 1. Replace selection with id link in parent
    (delete-region sel-beg sel-end)
    (insert (format "[[id:%s][%s]]" id selected))
    ;; 2. Insert child heading after the current subtree
    (save-excursion
      (org-end-of-subtree t t)
      (unless (bolp) (insert "\n"))
      (org-insert-heading nil t (1+ level))
      (insert (org-sm--truncate-title selected))
      (newline)
      (org-set-property "ID"       id)
      (org-set-property "SRS_TYPE" (symbol-name type))
      (org-schedule nil due)
      (pcase type
        ('topic
         (org-set-property "SRS_AFACTOR" (number-to-string org-sm-initial-afactor))
         (org-set-property "SRS_LAST"
                           (format-time-string "%FT%TZ" (current-time) "UTC0")))
        ('cloze
         (org-set-property "SRS_STATE" ":learning")))
      ;; Insert body text after meta-data
      (org-end-of-meta-data t)
      (insert child-body "\n"))
    ;; 3. If we're currently reviewing a cloze, the parent body has changed
    ;;    (the selection is now a link).  Re-apply overlays so the review
    ;;    state stays consistent.
    (when (eq org-sm--cloze-state 'revealed)
      (org-sm-cloze-remove-overlays)
      (org-sm-cloze-apply-overlays))))

;;;; ---- Review queue --------------------------------------------------------

(defvar org-sm--queue nil
  "Current review session queue: list of markers, in review order.
Populated once by `org-sm-start-review', consumed by `org-sm--advance'.
nil means no active session.")

(defconst org-sm--agenda-key "r"
  "Key for the SRS agenda custom command, as set by `org-sm-setup'.")

(defun org-sm--agenda-buffer ()
  "Return the live SRS agenda buffer, or nil if it does not exist."
  (get-buffer (format "*Org Agenda(%s)*" org-sm--agenda-key)))

(defun org-sm--ensure-agenda ()
  "Ensure the SRS agenda buffer is up-to-date and return it.
Opens a fresh agenda if none exists; calls `org-agenda-redo' if stale."
  (require 'org-agenda)
  (let ((org-agenda-sticky t))           ; gives buffer a key-specific name
    (if-let* ((buf (org-sm--agenda-buffer)))
        (with-current-buffer buf
          (org-agenda-redo)
          buf)
      (org-agenda nil org-sm--agenda-key)
      (or (org-sm--agenda-buffer)
          (error "org-sm: agenda buffer not found after opening")))))

(defun org-sm--markers-from-agenda (buf)
  "Return a list of org-hd-markers from agenda buffer BUF, in display order."
  (with-current-buffer buf
    (let (markers)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when-let* ((m (get-text-property (point) 'org-hd-marker)))
            (push m markers))
          (forward-line 1)))
      (nreverse markers))))

(defun org-sm--goto-marker (marker)
  "Jump to MARKER, narrow to its subtree, and recenter.
For cloze headings, immediately apply answer overlays so the item
arrives with answers already hidden, ready for SPC to reveal."
  (switch-to-buffer (marker-buffer marker))
  (widen)
  (goto-char marker)
  (org-back-to-heading t)
  (org-fold-show-subtree)
  (org-narrow-to-subtree)
  (goto-char (point-min))
  (recenter 0)
  (when (eq (org-sm-type) 'cloze)
    (org-sm-cloze-apply-overlays)
    (setq org-sm--cloze-state 'hidden)))

;;;###autoload
(defun org-sm-start-review ()
  "Build the SRS queue from the agenda buffer and start reviewing.
Opens (or refreshes) the `org-sm--agenda-key' agenda view, reads the
display order from it — respecting whatever sorting strategy you have
configured — then narrows to the first due item.  Call again to rescan."
  (interactive)
  (let* ((buf     (org-sm--ensure-agenda))
         (markers (org-sm--markers-from-agenda buf)))
    (if (null markers)
        (message "org-sm: nothing due today 🎉")
      (setq org-sm--queue (cdr markers))
      (message "org-sm: %d items due — starting session" (length markers))
      (org-sm--goto-marker (car markers))
      (org-sm--show-review-prompt))))

(defun org-sm--advance (&optional prev-result)
  "Move to the next item in the session queue, or end the session.
PREV-RESULT, when non-nil, is a short string describing the last action.
Skips any queued marker that no longer points to a valid SRS heading
\(e.g. the heading was deleted or refiled during the session)."
  (when (buffer-narrowed-p) (widen))
  (if (null org-sm--queue)
      (message "org-sm: session complete — nothing more due 🎉%s"
               (if prev-result (format "  (last: %s)" prev-result) ""))
    (let ((marker (pop org-sm--queue)))
      (org-sm--goto-marker marker)
      (if (org-sm-type)
          (org-sm--show-review-prompt prev-result)
        ;; Heading was deleted/refiled; skip silently and try the next one.
        (org-sm--advance prev-result)))))

(defun org-sm--show-review-prompt (&optional prev-result)
  "Display a usage hint for the current item type.
When PREV-RESULT is non-nil, prepend it so the user sees both the last
action result and the next item's hint in one minibuffer message."
  (let ((prefix (if prev-result (format "✓ %s  |  " prev-result) "")))
    (pcase (org-sm-type)
      ('cloze (message "org-sm %s[%d left] cloze: %s"
                       prefix (length org-sm--queue)
                       (pcase org-sm--cloze-state
                         ('hidden   "SPC to reveal answer")
                         ('revealed "SPC to rate: a/h/g/e")
                         (_         "SPC to start"))))
      ('topic (message "org-sm %s[%d left] topic: SPC to confirm read, C-c s d to dismiss"
                       prefix (length org-sm--queue)))
      (_      (message "org-sm: not an SRS heading")))))

;;;; ---- Review --------------------------------------------------------------

;; Buffer-local: tracks the cloze review state for the current heading.
;;   nil       - not in a cloze review (or just arrived, overlays being applied)
;;   hidden    - overlays applied, answers hidden, waiting for SPC to reveal
;;   revealed  - answers shown, waiting for SPC to open rating prompt
(defvar-local org-sm--cloze-state nil
  "Cloze review state for the current heading: nil, `hidden', or `revealed'.")

;;;###autoload
(defun org-sm-review-confirm ()
  "Confirm review of the current SRS item and advance to the next.

For topic headings: marks the topic as read, reschedules it via the
A-Factor algorithm, then moves to the next due item.  No grade needed;
use org priority [#A]/[#B]/[#C] on the heading to express interest level,
which affects agenda sort order.

For cloze headings, three-step flow:
  1. Item is displayed with answers already hidden (done automatically).
  2. SPC — reveals the answers; user can read at their own pace.
  3. SPC — opens the minibuffer rating prompt (a/h/g/e)."
  (interactive)
  (pcase (or (org-sm-type) (user-error "Not on an SRS heading"))
    ('topic
     (org-sm--topic-review)
     (org-sm--advance (format "topic → due in %s days"
                               (org-sm--topic-last-interval))))
    ('cloze
     (pcase org-sm--cloze-state
       ('hidden
        (org-sm-cloze-reveal-overlays)
        (setq org-sm--cloze-state 'revealed)
        (message "org-sm: answer revealed — SPC to rate"))
       ('revealed
        (setq org-sm--cloze-state nil)
        (org-sm-cloze-remove-overlays)
        (org-sm--rate-cloze))
       (_
        ;; Defensive fallback: user pressed SPC outside a normal session.
        (org-sm-cloze-apply-overlays)
        (setq org-sm--cloze-state 'hidden)
        (message "org-sm: answers hidden — SPC to reveal"))))))

(defun org-sm--rate-cloze ()
  "Prompt for FSRS rating via minibuffer, write card, then advance."
  (let* ((choice (read-multiple-choice
                  "Rate: "
                  '((?a "again" "Forgot — repeat soon")
                    (?h "hard"  "Remembered with difficulty")
                    (?g "good"  "Remembered correctly")
                    (?e "easy"  "Remembered effortlessly"))))
         (rating (pcase (car choice)
                   (?a :again) (?h :hard) (?g :good) (?e :easy)))
         (card     (org-sm--read-card))
         (new-card (cl-nth-value 0 (fsrs-scheduler-review-card
                                    org-sm--scheduler card rating)))
         (next-due (format-time-string "%F" (parse-iso8601-time-string
                                             (fsrs-card-due new-card)))))
    (org-sm--write-card new-card)
    (org-sm--advance (format "cloze %s → due %s" rating next-due))))

;;;###autoload
(defun org-sm-dismiss ()
  "Dismiss current SRS heading, then advance to next due item."
  (interactive)
  (unless (org-sm-type) (user-error "Not on an SRS heading"))
  (org-schedule nil org-sm-dismiss-date)
  (org-sm--advance "dismissed"))

;;;; ---- Agenda integration --------------------------------------------------

(defun org-sm--agenda-skip-non-srs ()
  "Skip agenda entries that are not SRS items."
  (unless (org-entry-get nil "SRS_TYPE")
    (org-entry-end-position)))

(defun org-sm--agenda-type-label ()
  "Return a short type label for the current agenda entry."
  (pcase (org-entry-get nil "SRS_TYPE")
    ("topic" "[topic]")
    ("cloze" "[cloze]")
    (_       "")))

(defun org-sm-setup ()
  "Register the SRS agenda view and enable sticky agenda buffers.
Adds a custom command keyed by `org-sm--agenda-key' (default \"r\") to
`org-agenda-custom-commands'.  The command filters to SRS items only
and sorts by priority then scheduled date.

Topic interest level is expressed via org's built-in priority cookies
on the heading ([#A] / [#B] / [#C]).  The agenda view sorts by priority
first, so high-priority topics naturally surface before low-priority ones."
  (require 'org-agenda)
  (setq org-agenda-sticky t)
  (when org-sm-files
    (setq org-agenda-files org-sm-files))
  (add-to-list
   'org-agenda-custom-commands
   `(,org-sm--agenda-key "SRS Review"
     ((agenda ""
       ((org-agenda-span             'day)
        (org-agenda-start-on-weekday nil)
        (org-agenda-skip-function    '(org-sm--agenda-skip-non-srs))
        (org-agenda-sorting-strategy '(priority-down scheduled-up))
        (org-agenda-prefix-format    "  %-10(org-sm--agenda-type-label) "))))
     nil nil)))

;;;; ---- Minor mode ----------------------------------------------------------

(defvar org-sm-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c s e")   #'org-sm-extract)
    (define-key m (kbd "C-c s SPC") #'org-sm-review-confirm)
    (define-key m (kbd "C-c s d")   #'org-sm-dismiss)
    (define-key m (kbd "C-c s s")   #'org-sm-start-review)
    m))

;;;###autoload
(define-minor-mode org-sm-mode
  "Minor mode for SuperMemo-style incremental reading."
  :lighter " SRS"
  :keymap  org-sm-mode-map
  :group   'org-sm)

(provide 'org-sm)
;;; org-sm.el ends here
