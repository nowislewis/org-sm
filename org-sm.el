;;; org-sm.el --- SuperMemo-style incremental reading for org-mode -*- lexical-binding: t -*-

;; Author: Lewis Liu
;; Package-Requires: ((emacs "29.1") (org "9.6") (fsrs "6.0"))
;; Keywords: org, spaced-repetition, incremental-reading

;;; Commentary:
;;
;; Minimal incremental reading for org-mode.  Two card types:
;;   topic  - repeated reading, A-Factor scheduling, no grading.
;;            org priority [#A]/[#B]/[#C] controls interval growth.
;;   cloze  - memory testing with {{cloze}} markers, FSRS scheduling.
;;
;; Commands:
;;   org-sm-mark           - mark heading as topic or cloze
;;   org-sm-extract        - extract region as child topic or cloze
;;   org-sm-start-review   - start review session
;;   org-sm-review-confirm - confirm topic read / advance cloze state
;;   org-sm-dismiss        - dismiss current item
;;
;;   (use-package org-sm
;;     :commands (org-sm-start-review org-sm-mark org-sm-extract)
;;     :hook (org-mode . org-sm-mode))

;;; Code:

(require 'cl-lib)

(defvar org-agenda-files)
(defvar org-agenda-custom-commands)
(defvar org-agenda-sticky)

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

(defcustom org-sm-initial-afactor 1.2
  "Initial A-Factor for new topic cards."
  :type 'float)

(defcustom org-sm-directory nil
  "Directory to scan for SRS org files.  Nil falls back to `org-agenda-files'."
  :type '(choice (const nil) directory))

(defcustom org-sm-file-filter-regexp "\\.org$"
  "Regexp to filter files under `org-sm-directory'."
  :type 'regexp)

;;;; ---- FSRS ----------------------------------------------------------------

(defvar org-sm--scheduler nil)

(defun org-sm--read-card ()
  "Build an `fsrs-card' from the current heading's properties."
  (require 'fsrs)
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

(defun org-sm--write-card (card)
  "Persist CARD to current heading's properties and SCHEDULED."
  (org-entry-put nil "SRS_STATE" (prin1-to-string (fsrs-card-state card)))
  (org-sm--put-or-delete "SRS_STABILITY"
                         (when-let* ((s (fsrs-card-stability card))) (number-to-string s)))
  (org-sm--put-or-delete "SRS_DIFFICULTY"
                         (when-let* ((d (fsrs-card-difficulty card))) (number-to-string d)))
  (org-sm--put-or-delete "SRS_LAST" (fsrs-card-last-review card))
  (org-sm--put-or-delete "SRS_STEP"
                         (when-let* ((s (fsrs-card-step card))) (number-to-string s)))
  (org-sm--schedule (parse-iso8601-time-string (fsrs-card-due card))))

;;;; ---- Review log ----------------------------------------------------------

(defun org-sm--log-review (type &optional rating)
  "Log a review entry for TYPE (symbol) with optional RATING."
  (org-with-wide-buffer
   (let* ((ts   (format-time-string (org-time-stamp-format 'long 'inactive)))
          (name (symbol-name type))
          (line (if rating
                    (format "- review %s  %s  %s\n" name rating ts)
                  (format "- %s  %s\n" name ts))))
     (goto-char (org-log-beginning t))
     (insert line))))

;;;; ---- Topic scheduling ----------------------------------------------------

(defun org-sm--topic-review ()
  "Reschedule current topic via A-Factor; return new interval in days.
Uses scheduled time (not now) as interval reference, so early/late
reviews don't distort the interval.  Priority controls A-Factor growth:
  [#A] floor=1.2, no growth   [#B] ceil=2.0, gentle   [#C] ceil=6.9, fast"
  (let* ((a    (string-to-number (or (org-entry-get nil "SRS_AFACTOR")
                                     (number-to-string org-sm-initial-afactor))))
         (last (org-entry-get nil "SRS_LAST"))
         (ref-time (or (org-get-scheduled-time nil) (current-time)))
         (ivl-actual (if last
                         (max 1 (round (/ (float-time
                                           (time-subtract ref-time
                                                          (parse-iso8601-time-string last)))
                                          86400.0)))
                       1))
         (p    (org-get-priority (org-get-heading t t t t)))
         (pri-params (cond ((>= p 2000) '(1.2  1.00  0.00))
                           ((>= p 1000) '(2.0  1.04 -0.02))
                           (t           '(6.9  1.18  0.00))))
         (target-ceil (nth 0 pri-params))
         (growth      (nth 1 pri-params))
         (offset      (nth 2 pri-params))
         (new-a (if (> a target-ceil)
                    (max target-ceil (+ a (* 0.4 (- target-ceil a))))
                  (max 1.2 (min target-ceil (+ (* growth a) offset)))))
         (ivl   (max 1 (ceiling (* ivl-actual new-a)))))
    (org-entry-put nil "SRS_AFACTOR" (number-to-string new-a))
    (org-entry-put nil "SRS_LAST"    (format-time-string "%FT%TZ" (current-time) "UTC0"))
    (org-sm--schedule (time-add (current-time) (days-to-time ivl)))
    (org-sm--log-review 'topic)
    ivl))

;;;; ---- Cloze overlays ------------------------------------------------------

(defvar org-sm--cloze-regexp "{{\\([^}\n]+\\)}}"
  "Regexp matching {{cloze}} markup; group 1 is the answer.")

(defface org-sm-cloze-face '((t :inherit bold :underline t))
  "Face for {{cloze}} markers during normal reading."
  :group 'org-sm)

(defun org-sm--cloze-font-lock-keywords ()
  ;; Hide the {{ and }} delimiters; show the answer with cloze face.
  (let ((re "\\({{\\)\\([^}\n]+\\)\\(}}\\)"))
    `((,re
       (1 '(face nil display "") prepend)   ; hide "{{"
       (2 'org-sm-cloze-face    prepend)    ; show answer with face
       (3 '(face nil display "") prepend)))))  ; hide "}}"

(defun org-sm--cloze-overlays ()
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

;;;; ---- Heading helpers -----------------------------------------------------

(defun org-sm-type ()
  "Return SRS_TYPE of current heading as a symbol, or nil."
  (require 'org)
  (when-let* ((v (org-entry-get nil "SRS_TYPE"))) (intern v)))

(defun org-sm--body-bounds ()
  "Return (start . end) of current heading's body."
  (save-excursion
    (org-back-to-heading t)
    (cons (progn (org-end-of-meta-data t) (point))
          (org-entry-end-position))))

(defun org-sm--body-clean (raw)
  "Strip common leading indentation from RAW and trim surrounding whitespace."
  (with-temp-buffer
    (insert (string-trim raw))
    (indent-rigidly (point-min) (point-max)
                    (- (save-excursion
                         (goto-char (point-min))
                         (skip-chars-forward " \t")
                         (current-column))))
    (string-trim (buffer-string))))

(defun org-sm--cloze-markers-p ()
  "Return non-nil if current heading body contains {{cloze}} markers."
  (let ((bounds (org-sm--body-bounds)))
    (string-match-p org-sm--cloze-regexp
                    (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(defun org-sm--truncate-title (str)
  "Truncate STR to `org-sm-title-max-length' columns, collapsing newlines."
  (let ((flat (replace-regexp-in-string "[ \t]*\n[ \t]*" " " (string-trim str))))
    (truncate-string-to-width flat org-sm-title-max-length nil nil "…")))

(defun org-sm--schedule (time)
  "Set SCHEDULED of current heading to TIME."
  (org-schedule nil (format-time-string "%F %a %H:%M" time)))

(defconst org-sm--all-srs-props
  '("SRS_AFACTOR" "SRS_LAST" "SRS_STATE" "SRS_STABILITY" "SRS_DIFFICULTY" "SRS_STEP")
  "All SRS properties across card types; cleared on re-mark.")

(defun org-sm--init-item (type)
  "Initialize TYPE (symbol) SRS properties and schedule to tomorrow."
  (org-entry-put nil "SRS_TYPE" (symbol-name type))
  (org-sm--schedule (time-add (current-time) (days-to-time 1)))
  (dolist (p org-sm--all-srs-props) (org-delete-property p))
  (pcase type
    ('topic
     (org-entry-put nil "SRS_AFACTOR" (number-to-string org-sm-initial-afactor))
     (org-entry-put nil "SRS_LAST"    (format-time-string "%FT%TZ" (current-time) "UTC0")))
    ('cloze
     (org-entry-put nil "SRS_STATE" ":learning"))))

;;;; ---- Mark / Extract ------------------------------------------------------

;;;###autoload
(defun org-sm-mark ()
  "Mark current heading as a topic or cloze SRS item."
  (interactive)
  (require 'org)
  (require 'org-id)
  (unless (org-at-heading-p) (org-back-to-heading t))
  (when (org-sm-type)
    (unless (yes-or-no-p (format "Already a %s item.  Re-mark and reset? " (org-sm-type)))
      (user-error "Aborted")))
  (let ((type (intern (completing-read "Mark as: " '("topic" "cloze") nil t))))
    (org-id-get-create)
    (org-sm--init-item type)
    (message "org-sm: marked as %s%s — due tomorrow" type
             (if (and (eq type 'cloze) (not (org-sm--cloze-markers-p)))
                 " (⚠ no {{cloze}} markers found)" ""))))

;;;###autoload
(defun org-sm-extract ()
  "Extract selected region as a child topic or cloze heading.

The selected region in the parent is replaced with an [[id:...][title]] link.
- topic: title is `org-sm-topic-prefix' + first N chars of the selection;
         child body is the selected text verbatim.
- cloze: title is `org-sm-cloze-prefix' + first N chars of the parent body;
         child body is the full parent body with the selection wrapped as {{answer}}."
  (interactive)
  (require 'org)
  (require 'org-id)
  (unless (region-active-p) (user-error "Select text to extract first"))
  ;; Read everything from the buffer before completing-read moves point.
  (let* ((sel-beg    (region-beginning))
         (sel-end    (region-end))
         (selected   (buffer-substring-no-properties sel-beg sel-end))
         (level      (org-current-level))
         (bounds     (org-sm--body-bounds))
         (body-raw   (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (sel-offset (- sel-beg (car bounds)))
         ;; Minibuffer interaction happens here, after all buffer reads.
         (type       (intern (completing-read "Extract as: " '("topic" "cloze") nil t)))
         (id         (org-id-new))
         (title      (pcase type
                       ('topic (concat org-sm-topic-prefix
                                       (org-sm--truncate-title selected)))
                       ('cloze (concat org-sm-cloze-prefix
                                       (org-sm--truncate-title body-raw)))))
         (child-body (pcase type
                       ('topic selected)
                       ('cloze (org-sm--body-clean
                                (concat (substring body-raw 0 sel-offset)
                                        (format "{{%s}}" selected)
                                        (substring body-raw (+ sel-offset
                                                               (length selected)))))))))
    ;; Replace selection in parent with id link.
    ;; Link description uses the original selected text so the parent body
    ;; remains readable and coherent (not the prefixed/truncated heading title).
    (delete-region sel-beg sel-end)
    (insert (format "[[id:%s][%s]]" id (org-sm--truncate-title selected)))
    ;; Insert child heading at end of subtree.
    (save-excursion
      (org-end-of-subtree t t)
      (unless (bolp) (insert "\n"))
      (org-insert-heading nil t (1+ level))
      (insert title)
      (newline)
      ;; Pin the body insertion point now, before org-sm--init-item moves point.
      (let ((body-marker (point-marker)))
        (org-set-property "ID" id)
        (org-sm--init-item type)
        (goto-char body-marker)
        (set-marker body-marker nil)
        (org-end-of-meta-data t)
        (insert child-body "\n"))))
  (when (eq org-sm--cloze-state 'revealed)
    (org-sm-cloze-remove-overlays)
    (org-sm-cloze-apply-overlays)))

;;;; ---- Review session ------------------------------------------------------

(defvar org-sm--queue nil
  "Active review queue: list of markers consumed by `org-sm--advance'.")

(defconst org-sm--agenda-key "r")

(defvar-local org-sm--cloze-state nil
  "Cloze state: nil | `hidden' | `revealed'.")

(defun org-sm--goto-marker (marker)
  "Jump to MARKER, narrow to subtree, apply cloze overlays if needed."
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

(defun org-sm--show-prompt (&optional prev)
  "Show review hint for current item, optionally prefixed with PREV result."
  (let ((pre (if prev (format "✓ %s  |  " prev) "")))
    (pcase (org-sm-type)
      ('cloze (message "org-sm %s[%d left] cloze: %s" pre (length org-sm--queue)
                       (if (eq org-sm--cloze-state 'hidden)
                           "SPC to reveal" "SPC to rate: a/h/g/e")))
      ('topic (message "org-sm %s[%d left] topic: SPC to confirm" pre (length org-sm--queue)))
      (_      (message "org-sm: not an SRS heading")))))

(defun org-sm--advance (&optional prev)
  "Advance to the next queued item, or end session.  PREV is last result string."
  (when (buffer-narrowed-p) (widen))
  (let (found)
    (while (and org-sm--queue (not found))
      (let ((marker (pop org-sm--queue)))
        (when-let* ((buf (marker-buffer marker)))
          (when (buffer-live-p buf)
            (org-sm--goto-marker marker)
            (when (org-sm-type) (setq found t))))))
    (if found
        (org-sm--show-prompt prev)
      (message "org-sm: done 🎉%s" (if prev (format "  (last: %s)" prev) "")))))

(defun org-sm--ensure-agenda-buf ()
  "Return up-to-date SRS agenda buffer, opening it if necessary.
Handles both sticky (`*Org Agenda(KEY)*') and non-sticky (`*Org Agenda*')
buffer naming conventions."
  (let* ((sticky-name   (format "*Org Agenda(%s)*" org-sm--agenda-key))
         (nonsticky-name "*Org Agenda*")
         (existing       (or (get-buffer sticky-name)
                             (with-current-buffer (or (get-buffer nonsticky-name)
                                                      (current-buffer))
                               (when (derived-mode-p 'org-agenda-mode)
                                 (current-buffer))))))
    (if existing
        (with-current-buffer existing (org-agenda-redo) existing)
      (org-agenda nil org-sm--agenda-key)
      ;; After org-agenda returns, the agenda buffer is current.
      (let ((buf (current-buffer)))
        (unless (derived-mode-p 'org-agenda-mode)
          (error "org-sm: agenda buffer not found after opening"))
        buf))))

;;;###autoload
(defun org-sm-start-review ()
  "Build review queue from agenda and start session."
  (interactive)
  (org-sm-setup)
  (require 'org-agenda)
  (let* ((org-agenda-files (if org-sm-directory
                               (seq-filter
                                (lambda (f) (string-match-p org-sm-file-filter-regexp f))
                                (directory-files-recursively org-sm-directory "\\.org$"))
                             (bound-and-true-p org-agenda-files)))
         (buf     (org-sm--ensure-agenda-buf))
         (markers (with-current-buffer buf
                    (cl-delete-duplicates
                     (cl-loop for pos from (point-min) below (point-max)
                              for m = (get-text-property pos 'org-hd-marker)
                              when m collect m)
                     :test #'equal))))
    (if (null markers)
        (message "org-sm: nothing due 🎉")
      (setq org-sm--queue (cdr markers))
      (message "org-sm: %d items due" (length markers))
      (org-sm--goto-marker (car markers))
      (org-sm--show-prompt))))

;;;###autoload
(defun org-sm-review-confirm ()
  "Confirm read (topic) or advance cloze state, then move to next item."
  (interactive)
  (pcase (or (org-sm-type) (user-error "Not on an SRS heading"))
    ('topic
     (org-sm--advance (format "topic → %d days" (org-sm--topic-review))))
    ('cloze
     (pcase org-sm--cloze-state
       ('hidden
        (org-sm-cloze-reveal-overlays)
        (setq org-sm--cloze-state 'revealed)
        (message "org-sm: revealed — SPC to rate"))
       ('revealed
        (setq org-sm--cloze-state nil)
        (org-sm-cloze-remove-overlays)
        (let* ((choice (read-multiple-choice
                        "Rate: " '((?a "again") (?h "hard") (?g "good") (?e "easy"))))
               (rating (pcase (car choice) (?a :again) (?h :hard) (?g :good) (?e :easy)))
               (card     (org-sm--read-card))
               (new-card (cl-nth-value 0 (fsrs-scheduler-review-card
                                          org-sm--scheduler card rating)))
               (next-due (format-time-string "%F %H:%M"
                                             (parse-iso8601-time-string
                                              (fsrs-card-due new-card)))))
          (org-sm--write-card new-card)
          (org-sm--log-review 'cloze rating)
          (org-sm--advance (format "cloze %s → %s" rating next-due))))
       (_
        (org-sm-cloze-apply-overlays)
        (setq org-sm--cloze-state 'hidden)
        (message "org-sm: hidden — SPC to reveal"))))))

;;;###autoload
(defun org-sm-dismiss ()
  "Dismiss current SRS item (schedule to year 2999)."
  (interactive)
  (unless (org-sm-type) (user-error "Not on an SRS heading"))
  (org-sm--log-review 'dismissed)
  (org-sm--schedule (parse-iso8601-time-string "2999-12-31T00:00:00Z"))
  (org-sm--advance "dismissed"))

;;;; ---- Agenda --------------------------------------------------------------

(defun org-sm--agenda-skip-non-srs ()
  "Skip non-SRS or not-yet-due entries."
  (when-let* ((m (org-get-at-bol 'org-hd-marker)))
    (org-with-point-at m
      (when (or (null (org-entry-get nil "SRS_TYPE"))
                (when-let* ((t_ (org-get-scheduled-time nil)))
                  (> (float-time t_) (float-time))))
        (line-end-position)))))

(defun org-sm--agenda-type-label ()
  "Return type label for agenda prefix."
  (when-let* ((m (org-get-at-bol 'org-hd-marker)))
    (org-with-point-at m
      (pcase (org-entry-get nil "SRS_TYPE")
        ("topic" "[topic]") ("cloze" "[cloze]") (_ "")))))

;;;; ---- Setup / Minor mode --------------------------------------------------

(defvar org-sm--setup-done nil)

;;;###autoload
(defun org-sm-setup ()
  "Initialize FSRS scheduler and register SRS agenda view.  Idempotent."
  (interactive)
  (unless org-sm--setup-done
    (require 'org-agenda)
    (require 'fsrs)
    (setq org-sm--scheduler
          (fsrs-make-scheduler :desired-retention 0.9
                               :learning-steps    '((1 :minute) (10 :minute))
                               :enable-fuzzing-p  t))
    (add-to-list 'org-agenda-custom-commands
                 `(,org-sm--agenda-key "SRS Review"
                   ((agenda ""
                     ((org-agenda-span             'day)
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-skip-function    '(org-sm--agenda-skip-non-srs))
                      (org-agenda-sorting-strategy '(priority-down scheduled-up))
                      (org-agenda-prefix-format    "  %-10(org-sm--agenda-type-label) "))))
                   nil nil))
    (setq org-sm--setup-done t)))

;;;###autoload
(define-minor-mode org-sm-mode
  "Font-lock {{cloze}} markers in org buffers."
  :lighter " SRS"
  :group 'org-sm
  (if org-sm-mode
      (font-lock-add-keywords nil (org-sm--cloze-font-lock-keywords) 'append)
    (font-lock-remove-keywords nil (org-sm--cloze-font-lock-keywords)))
  (when (fboundp 'font-lock-flush) (font-lock-flush)))

(provide 'org-sm)
;;; org-sm.el ends here
