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
;;   org-sm-item-dismiss   - remove item from SRS
;;   org-sm-review-start   - start review session
;;   org-sm-review-confirm - confirm topic read / advance cloze state
;;   org-sm-review-abort   - abort review session
;;   org-sm-review-list    - browse all SRS items
;;
;;   (use-package org-sm
;;     :commands (org-sm-review-start org-sm-item-mark org-sm-item-extract org-sm-review-list)
;;     :hook (org-mode . org-sm-mode))

;;; Code:

(require 'cl-lib)

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

(defcustom org-sm-directory nil
  "Directory to scan for SRS org files.  Nil falls back to `org-agenda-files'."
  :type '(choice (const nil) directory))

(defcustom org-sm-file-filter-regexp "\\.org$"
  "Regexp to filter files under `org-sm-directory'."
  :type 'regexp)

;;;; ---- File list -----------------------------------------------------------

(defun org-sm--files ()
  "Return the list of org files to scan for SRS items."
  (if org-sm-directory
      (seq-filter (lambda (f) (string-match-p org-sm-file-filter-regexp f))
                  (directory-files-recursively org-sm-directory "\\.org$"))
    (bound-and-true-p org-agenda-files)))

;;;; ---- Schedule helper -----------------------------------------------------

(defun org-sm--schedule (time)
  "Set SCHEDULED of current heading to TIME.
Must be called before any `org-entry-put' / `org-set-property' on the same
heading, because those functions move point into the PROPERTIES drawer; after
that `org-schedule' (called internally) would climb to the wrong heading."
  (org-schedule nil (format-time-string "%F %a %H:%M" time)))

;;;; ---- FSRS ----------------------------------------------------------------

(defvar org-sm--scheduler nil
  "FSRS scheduler instance, initialized on first call to `org-sm-review-start'.")

(defun org-sm--ensure-scheduler ()
  "Initialize `org-sm--scheduler' if not already done."
  (unless org-sm--scheduler
    (require 'fsrs)
    (setq org-sm--scheduler
          (fsrs-make-scheduler :desired-retention 0.9
                               :learning-steps    '((1 :minute) (10 :minute))
                               :enable-fuzzing-p  t))))

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
    ("A" 1.2)
    ("B" 1.5)
    (_   1.8)))

(defun org-sm--topic-review (a)
  "Reschedule current topic with A-Factor A; return new interval in days.
Uses scheduled time as interval reference to avoid early/late review distortion."
  (let* ((last (org-entry-get nil "SRS_LAST"))
         (ref  (or (org-get-scheduled-time nil) (current-time)))
         (ivl  (max 1 (round (* a (if last
                                      (max 1 (/ (float-time (time-subtract ref (parse-iso8601-time-string last)))
                                                86400.0))
                                    1))))))
    ;; Schedule first — org-entry-put moves point into PROPERTIES drawer.
    (org-sm--schedule (time-add (current-time) (days-to-time ivl)))
    (org-entry-put nil "SRS_LAST" (format-time-string "%FT%TZ" (current-time) "UTC0"))
    (org-sm--log-review 'topic nil (format "a=%.1f" a))
    ivl))

;;;; ---- Cloze overlays ------------------------------------------------------

(defvar org-sm--cloze-regexp "{{\\([^}\n]+\\)}}"
  "Regexp matching {{cloze}} markup; group 1 is the answer.")

(defface org-sm-cloze-face '((t :inherit bold :underline t))
  "Face for {{cloze}} markers during normal reading."
  :group 'org-sm)

(defun org-sm--cloze-font-lock-keywords ()
  "Return font-lock keywords that render {{cloze}} markers."
  (let ((re "\\({{\\)\\([^}\n]+\\)\\(}}\\)"))
    `((,re
       (1 '(face nil display "") prepend)   ; hide "{{"
       (2 'org-sm-cloze-face    prepend)    ; show answer with face
       (3 '(face nil display "") prepend))))) ; hide "}}"

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

;;;; ---- Heading helpers -----------------------------------------------------

(defun org-sm-type ()
  "Return SRS_TYPE of current heading as a symbol, or nil."
  (require 'org)
  (when-let* ((v (org-entry-get nil "SRS_TYPE"))) (intern v)))

(defun org-sm--body-bounds ()
  "Return (start . end) of current heading's body (excluding meta-data)."
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

(defconst org-sm--all-srs-props
  '("SRS_LAST" "SRS_STATE" "SRS_STABILITY" "SRS_DIFFICULTY" "SRS_STEP")
  "All SRS scheduling properties; cleared on re-mark.")

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

;;;; ---- Mark / Extract ------------------------------------------------------

;;;###autoload
(defun org-sm-item-mark ()
  "Mark current heading as a topic or cloze SRS item."
  (interactive)
  (require 'org)
  (require 'org-id)
  (unless (org-at-heading-p) (org-back-to-heading t))
  (when (org-sm-type)
    (unless (yes-or-no-p (format "Already a %s item.  Re-mark and reset? " (org-sm-type)))
      (user-error "Aborted")))
  (let ((type (intern (completing-read "Mark as: " '("topic" "cloze") nil t))))
    (org-sm--schedule (time-add (current-time) (days-to-time 1)))
    (org-id-get-create)
    (org-sm--init-item type)
    (message "org-sm: marked as %s%s — due tomorrow" type
             (if (and (eq type 'cloze) (not (org-sm--cloze-markers-p)))
                 " (⚠ no {{cloze}} markers found)" ""))))

;;;###autoload
(defun org-sm-item-extract ()
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
  ;; Capture all buffer state before any minibuffer interaction moves point.
  (let* ((sel-beg    (region-beginning))
         (sel-end    (region-end))
         (selected   (buffer-substring-no-properties sel-beg sel-end))
         (level      (org-current-level))
         (bounds     (org-sm--body-bounds))
         (body-raw   (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (sel-offset (- sel-beg (car bounds)))
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
    ;; Replace selection in parent with a readable id link.
    (delete-region sel-beg sel-end)
    (insert (format "[[id:%s][%s]]" id (org-sm--truncate-title selected)))
    ;; Append child heading at end of current subtree.
    (save-excursion
      (org-end-of-subtree t t)
      (unless (bolp) (insert "\n"))
      (org-insert-heading nil t (1+ level))
      (insert title)
      ;; Schedule before org-set-property / org-entry-put: those functions move
      ;; point into the PROPERTIES drawer, after which org-schedule would climb
      ;; up to the previous sibling heading.
      (org-sm--schedule (time-add (current-time) (days-to-time 1)))
      (org-set-property "ID" id)
      (org-sm--init-item type)
      (org-end-of-meta-data t)
      (insert child-body "\n")))
  (when (eq org-sm--cloze-state 'revealed)
    (org-sm-cloze-remove-overlays)
    (org-sm-cloze-apply-overlays)))

;;;; ---- Review session ------------------------------------------------------

(defvar org-sm--queue nil
  "Active review queue: list of markers consumed by `org-sm--advance'.")

(defvar-local org-sm--cloze-state nil
  "Cloze review state for the current buffer: `hidden' or `revealed'.")

(defun org-sm--due-p ()
  "Return non-nil if current heading is a due SRS item."
  (and (org-entry-get nil "SRS_TYPE")
       (when-let* ((t_ (org-get-scheduled-time nil)))
         (<= (float-time t_) (float-time)))))

(defun org-sm--collect-due-markers ()
  "Return markers for all due SRS items across `org-sm--files', sorted by priority."
  (let ((results
         (org-map-entries
          (lambda ()
            (when (org-sm--due-p)
              (cons (org-get-priority (org-get-heading t t t t))
                    (point-marker))))
          nil
          (org-sm--files))))
    (mapcar #'cdr
            (sort (delq nil results)
                  (lambda (a b) (> (car a) (car b)))))))

(defun org-sm--goto-marker (marker)
  "Switch to MARKER's buffer, narrow to its subtree, set up cloze state."
  (unless (marker-buffer marker)
    (error "org-sm: stale marker — restart the review session"))
  (switch-to-buffer (marker-buffer marker))
  (widen)
  (goto-char marker)
  (org-back-to-heading t)
  ;; Fold subtree first so any previously-open children are hidden, then
  ;; re-open only the current entry's body (not its children).
  (org-fold-hide-subtree)
  (org-fold-show-entry)
  (org-narrow-to-subtree)
  (goto-char (point-min))
  (recenter 0)
  (when (eq (org-sm-type) 'cloze)
    (org-sm-cloze-apply-overlays)
    (setq org-sm--cloze-state 'hidden)))

(defun org-sm--show-prompt (&optional prev)
  "Show review hint in the echo area, optionally prefixed with PREV result."
  (let ((pre (if prev (format "✓ %s  |  " prev) "")))
    (pcase (org-sm-type)
      ('cloze (message "org-sm %s[%d left] cloze: %s" pre (length org-sm--queue)
                       (if (eq org-sm--cloze-state 'hidden)
                           "SPC to reveal" "SPC to rate: a/h/g/e")))
      ('topic (message "org-sm %s[%d left] topic: SPC to confirm" pre (length org-sm--queue)))
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
      (setq org-sm--cloze-state nil)
      (message "org-sm: done 󱁖 %s" (if prev (format "  (last: %s)" prev) "")))))

;;;###autoload
(defun org-sm-review-start ()
  "Collect all due SRS items and start a review session."
  (interactive)
  (require 'org)
  (org-sm--ensure-scheduler)
  (let ((markers (org-sm--collect-due-markers)))
    (if (null markers)
        (message "org-sm: nothing due 󱁖")
      (setq org-sm--queue (cdr markers))
      (message "org-sm: %d items due" (length markers))
      (org-sm--goto-marker (car markers))
      (org-sm--show-prompt))))

;;;###autoload
(defun org-sm-review-confirm ()
  "Confirm topic read or advance cloze state, then move to next item."
  (interactive)
  (unless (or org-sm--queue org-sm--cloze-state)
    (user-error "No active review session — call org-sm-review-start"))
  (pcase (or (org-sm-type) (user-error "Not on an SRS heading"))
    ('topic
     (let* ((a   (org-sm--topic-afactor))
            (ivl (org-sm--topic-review a)))
       (org-sm--advance (format "topic [a=%.1f] → %d days" a ivl))))
    ('cloze
     (pcase org-sm--cloze-state
       ('hidden
        (org-sm-cloze-reveal-overlays)
        (setq org-sm--cloze-state 'revealed)
        (message "org-sm: revealed — SPC to rate"))
       ('revealed
        (org-sm-cloze-remove-overlays)
        (let* ((card      (org-sm--read-card))
               (now       (fsrs-now))
               (previews  (mapcar
                           (lambda (r)
                             (let* ((rating  (caddr r))
                                    (c       (cl-nth-value 0 (fsrs-scheduler-review-card
                                                              org-sm--scheduler card rating)))
                                    (secs    (fsrs-timestamp-difference (fsrs-card-due c) now))
                                    (days    (fsrs-seconds-days secs))
                                    (label   (if (< days 1)
                                                 (format "%s(%dm)" (cadr r) (round (/ secs 60)))
                                               (format "%s(%dd)" (cadr r) days))))
                               (list (car r) label rating c)))
                           '((?a "again" :again) (?h "hard" :hard)
                             (?g "good"  :good)  (?e "easy" :easy))))
               (choice    (read-multiple-choice
                           "Rate: " (mapcar (lambda (p) (list (car p) (cadr p))) previews)))
               (matched   (assq (car choice) previews))
               (rating    (caddr matched))
               (new-card  (cadddr matched))
               (next-due  (format-time-string "%F %H:%M"
                                              (parse-iso8601-time-string
                                               (fsrs-card-due new-card)))))
          (org-sm--write-card new-card)
          (org-sm--log-review 'cloze rating)
          (org-sm--advance (format "cloze %s → %s" rating next-due))))))))

;;;###autoload
(defun org-sm-item-dismiss ()
  "Dismiss current SRS item by removing its SRS_TYPE property."
  (interactive)
  (unless (org-sm-type) (user-error "Not on an SRS heading"))
  (org-sm--log-review 'dismissed)
  (org-delete-property "SRS_TYPE")
  (org-sm--advance "dismissed"))

;;;###autoload
(defun org-sm-review-abort ()
  "Abort the current review session, cleaning up state and overlays."
  (interactive)
  (unless (or org-sm--queue org-sm--cloze-state)
    (user-error "No active review session"))
  (setq org-sm--queue nil)
  (setq org-sm--cloze-state nil)
  (org-sm-cloze-remove-overlays)
  (when (buffer-narrowed-p) (widen))
  (message "org-sm: review aborted"))

;;;###autoload
(defun org-sm-review-list ()
  "Browse all SRS items across `org-sm--files' in an agenda-style buffer."
  (interactive)
  (require 'org-agenda)
  (let ((org-agenda-files (org-sm--files)))
    (org-tags-view nil "SRS_TYPE={.+}")))

;;;; ---- Minor mode ----------------------------------------------------------

;;;###autoload
(define-minor-mode org-sm-mode
  "Font-lock {{cloze}} markers in org-mode buffers."
  :lighter " SRS"
  :group 'org-sm
  (if org-sm-mode
      (font-lock-add-keywords nil (org-sm--cloze-font-lock-keywords) 'append)
    (font-lock-remove-keywords nil (org-sm--cloze-font-lock-keywords)))
  (when (fboundp 'font-lock-flush) (font-lock-flush)))

(provide 'org-sm)
;;; org-sm.el ends here
