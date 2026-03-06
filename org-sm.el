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
;;   org-sm-mark           - mark current heading as topic or cloze
;;   org-sm-extract        - extract selection as topic or cloze child heading
;;   org-sm-start-review   - start a review session
;;   org-sm-review-confirm - confirm topic read / advance cloze state
;;   org-sm-dismiss        - dismiss current item
;;
;; Setup (lazy-load friendly):
;;
;;   (use-package org-sm
;;     :commands (org-sm-start-review org-sm-mark org-sm-extract)
;;     :hook (org-mode . org-sm-mode))
;;
;; `org-sm-setup' is called automatically the first time `org-sm-start-review'
;; is invoked.  `org-sm-mode' is a pure UI mode (font-lock only) and can be
;; added to `org-mode-hook' without triggering any eager loading.

;;; Code:

(require 'cl-lib)

;; Declare org-agenda dynamic variables so lexical-binding does not
;; treat our `let' rebindings as lexical locals.
(defvar org-agenda-files)
(defvar org-agenda-custom-commands)
(defvar org-agenda-sticky)

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

(defcustom org-sm-directory nil
  "Directory to scan recursively for SRS org files.
When non-nil, all .org files matching `org-sm-file-filter-regexp' under
this directory are used as the review pool, and `org-agenda-files' is
not affected.  When nil, `org-agenda-files' is used as fallback."
  :type '(choice (const nil) directory)
  :group 'org-sm)

(defcustom org-sm-file-filter-regexp "\\.org$"
  "Regexp to filter files found under `org-sm-directory'.
Matched against the full file path.  Default accepts all .org files.
Example: set to \"srs[^/]*\\\\.org$\" to only include files whose name
starts with \\='srs\\='."
  :type 'regexp
  :group 'org-sm)

;;;; ---- FSRS scheduler ------------------------------------------------------

(defvar org-sm--scheduler nil
  "Global FSRS scheduler instance, initialized lazily by `org-sm-setup'.")

;;;; ---- Property helpers ----------------------------------------------------

(defun org-sm-type ()
  "Return SRS_TYPE of current heading as a symbol, or nil."
  (require 'org)
  (when-let* ((v (org-entry-get nil "SRS_TYPE")))
    (intern v)))

;;;; ---- FSRS card read / write ----------------------------------------------

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
  "Set PROP to VALUE if non-nil, otherwise delete the property."
  (if value
      (org-entry-put nil prop value)
    (org-delete-property prop)))

(defun org-sm--write-card (card)
  "Persist CARD state to the current heading's properties and SCHEDULED."
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
  "Insert a review log entry into the current heading's LOGBOOK drawer.
TYPE is a string: \"topic\", \"cloze\", or \"dismissed\".
RATING, when non-nil, is a symbol such as :good or :again (cloze only)."
  (org-with-wide-buffer
   (let* ((ts   (format-time-string (org-time-stamp-format 'long 'inactive)))
          (line (if rating
                    (format "- review %s  %s  %s\n" type rating ts)
                  (format "- %s  %s\n" type ts))))
     (goto-char (org-log-beginning t))
     (insert line))))

;;;; ---- Topic A-Factor scheduling ------------------------------------------

(defun org-sm--topic-review ()
  "Reschedule current topic using the A-Factor algorithm.
No grade is needed: confirming you have read the topic is sufficient.
The A-Factor is adjusted based on the heading's org priority:
  [#A] high interest — A-Factor stays at floor, interval kept short
  [#B] normal        — A-Factor grows gently (default)
  [#C] low interest  — A-Factor grows aggressively, interval stretches fast
Returns the new interval in days."
  (let* ((a    (string-to-number (or (org-entry-get nil "SRS_AFACTOR")
                                     (number-to-string org-sm-initial-afactor))))
         (last (org-entry-get nil "SRS_LAST"))
         ;; Use the scheduled time (= intended interval end) rather than now,
         ;; so early or late reviews do not distort the actual interval.
         ;; Falls back to current-time when no scheduled time exists.
         (ref-time (or (org-get-scheduled-time nil) (current-time)))
         (ivl-actual (if last
                         (max 1 (round (/ (float-time
                                           (time-subtract ref-time
                                                          (parse-iso8601-time-string last)))
                                          86400.0)))
                       1))
         (p    (org-get-priority (org-get-heading t t t t)))
         ;; org default priority: A=2000 (highest), B=1000 (default), C=0 (lowest)
         ;; High priority = high interest = review frequently = interval stays short
         ;; Low priority  = low interest  = review rarely    = interval grows fast
         (pri-params (cond ((>= p 2000) '(1.2  1.00  0.00))   ; [#A]: floor, no growth
                           ((>= p 1000) '(2.0  1.04 -0.02))   ; [#B]: gentle growth
                           (t           '(6.9  1.18  0.00))))  ; [#C]: aggressive growth
         (target-ceil (nth 0 pri-params))
         (growth      (nth 1 pri-params))
         (offset      (nth 2 pri-params))
         ;; If current A-Factor exceeds target ceiling (e.g. priority was just
         ;; raised from C to A), converge 40% toward the ceiling each review.
         (new-a (if (> a target-ceil)
                    (max target-ceil (+ a (* 0.4 (- target-ceil a))))
                  (max 1.2 (min target-ceil (+ (* growth a) offset)))))
         (ivl   (max 1 (ceiling (* ivl-actual new-a)))))
    (org-entry-put nil "SRS_AFACTOR" (number-to-string new-a))
    (org-entry-put nil "SRS_LAST"    (format-time-string "%FT%TZ" (current-time) "UTC0"))
    (org-sm--schedule (time-add (current-time) (days-to-time ivl)))
    (org-sm--log-review "topic")
    ivl))

;;;; ---- Cloze overlay -------------------------------------------------------

(defvar org-sm--cloze-regexp
  "{{\\([^}\n]+\\)}}"
  "Regexp matching {{cloze}} markup.  Group 1 is the answer text.
Double curly braces are not special in org-mode, requiring no boundary
checks.  A custom font-lock rule renders them with `org-sm-cloze-face'
during normal reading; overlays replace them with [___] during review.")

(defface org-sm-cloze-face
  '((t :inherit bold :underline t))
  "Face for {{cloze}} markers during normal reading."
  :group 'org-sm)

(defun org-sm--cloze-font-lock-keywords ()
  "Return font-lock keywords that highlight {{cloze}} markers.
The {{ and }} delimiters are hidden; only the answer text is shown with
`org-sm-cloze-face'."
  `((,org-sm--cloze-regexp
     (0   '(face nil display "")   prepend)   ; hide the whole match first
     (1   'org-sm-cloze-face       prepend)   ; then re-highlight group 1
     )))

(defun org-sm--cloze-overlays ()
  "Return all org-sm-cloze overlays in the current heading."
  (seq-filter (lambda (ov) (eq (overlay-get ov 'category) 'org-sm-cloze))
              (overlays-in (org-entry-beginning-position)
                           (org-entry-end-position))))

(defun org-sm-cloze-apply-overlays ()
  "Hide all {{cloze}} markers in current heading with [___] overlays."
  (let ((start (org-entry-beginning-position))
        (end   (org-entry-end-position)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward org-sm--cloze-regexp end t)
        (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put ov 'category      'org-sm-cloze)
          (overlay-put ov 'display       (propertize "[___]" 'face '(bold highlight)))
          (overlay-put ov 'org-sm-answer (match-string 1))
          (overlay-put ov 'evaporate     t))))))

(defun org-sm-cloze-reveal-overlays ()
  "Show answers in all cloze overlays of the current heading."
  (dolist (ov (org-sm--cloze-overlays))
    (overlay-put ov 'display
                 (propertize (format "[%s]" (overlay-get ov 'org-sm-answer))
                             'face 'success))))

(defun org-sm-cloze-remove-overlays ()
  "Remove all cloze overlays from the current heading."
  (mapc #'delete-overlay (org-sm--cloze-overlays)))

;;;; ---- Heading body helpers ------------------------------------------------

(defun org-sm--body-bounds ()
  "Return (start . end) of current heading's body (after meta-data, before next heading)."
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

(defun org-sm--cloze-body-at (sel-beg selected)
  "Return parent body text with the selection at SEL-BEG marked as a cloze.
SELECTED is the selected text."
  (let* ((bounds     (org-sm--body-bounds))
         (body-start (car bounds))
         (body-raw   (buffer-substring-no-properties body-start (cdr bounds)))
         (offset     (- sel-beg body-start)))
    (org-sm--body-clean
     (concat (substring body-raw 0 offset)
             (format "{{%s}}" selected)
             (substring body-raw (+ offset (length selected)))))))

(defun org-sm--truncate-title (str)
  "Return STR truncated to `org-sm-title-max-length' display columns.
Newlines are collapsed to a single space so multi-line selections
produce a readable single-line title."
  (let ((flat (replace-regexp-in-string "[ \t]*\n[ \t]*" " " (string-trim str))))
    (truncate-string-to-width flat org-sm-title-max-length nil nil "…")))

(defun org-sm--schedule (time)
  "Schedule current heading to TIME (an Emacs time value)."
  (org-schedule nil (format-time-string "%F %a %H:%M" time)))

(defun org-sm--due-tomorrow ()
  "Return an Emacs time value for the same clock time tomorrow."
  (time-add (current-time) (days-to-time 1)))

(defun org-sm--init-item (type)
  "Write SRS properties for TYPE (symbol) and schedule to tomorrow.
Cleans up stale properties from the other type."
  (org-entry-put nil "SRS_TYPE" (symbol-name type))
  (org-sm--schedule (org-sm--due-tomorrow))
  (pcase type
    ('topic
     ;; SRS_LAST = now, used to compute the actual interval on next review.
     (org-entry-put nil "SRS_AFACTOR" (number-to-string org-sm-initial-afactor))
     (org-entry-put nil "SRS_LAST"    (format-time-string "%FT%TZ" (current-time) "UTC0"))
     (org-delete-property "SRS_STATE")
     (org-delete-property "SRS_STEP"))
    ('cloze
     ;; SRS_LAST is managed by FSRS; start clean so the first review is
     ;; treated as a brand-new card with no prior history.
     (org-entry-put nil "SRS_STATE" ":learning")
     (org-delete-property "SRS_AFACTOR")
     (org-delete-property "SRS_LAST")
     (org-delete-property "SRS_STEP"))))

;;;###autoload
(defun org-sm-mark ()
  "Mark the current heading itself as an SRS item (topic or cloze).

Unlike `org-sm-extract', which creates a *child* heading from a selected
region, this command registers the heading you are already on — no region
needed.  Useful for:
  - Converting existing notes into SRS items in bulk.
  - Quickly scheduling a newly written heading without extracting.

For cloze: the heading body must already contain {{cloze}} markers.
A warning is shown if none are found, but the heading is still marked
so you can add markers afterwards.

If the heading is already an SRS item you are prompted to confirm
before overwriting its scheduling data."
  (interactive)
  (require 'org)
  (require 'org-id)
  (unless (org-at-heading-p)
    (org-back-to-heading t))
  ;; Guard: already an SRS heading?
  (when (org-sm-type)
    (unless (yes-or-no-p
             (format "Heading is already a %s item.  Re-mark and reset schedule? "
                     (org-sm-type)))
      (user-error "Aborted")))
  (let* ((type (intern (completing-read "Mark as: " '("topic" "cloze") nil t))))
    ;; Ensure a stable ID exists
    (org-id-get-create)
    (org-sm--init-item type)
    (pcase type
      ('topic
       (message "org-sm: marked as topic — due tomorrow"))
      ('cloze
       (let* ((bounds (org-sm--body-bounds))
              (body   (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (if (string-match-p org-sm--cloze-regexp body)
             (message "org-sm: marked as cloze — due tomorrow")
           (message "org-sm: marked as cloze (⚠ no {{cloze}} markers found) — due tomorrow")))))))

;;;; ---- Extract -------------------------------------------------------------

;;;###autoload
(defun org-sm-extract ()
  "Extract the selected region as a child heading.

Prompts for type:
  topic - child body is the selected text itself.
  cloze - child body is the full parent body; only the selected
          instance is wrapped as a {{cloze}}.

The selected region in the parent is replaced with an [[id:...][text]] link."
  (interactive)
  (require 'org)
  (require 'org-id)
  (unless (region-active-p)
    (user-error "Select text to extract first"))
  (let* ((sel-beg  (region-beginning))
         (sel-end  (region-end))
         (selected (buffer-substring-no-properties sel-beg sel-end))
         (type     (intern (completing-read "Extract as: "
                                            '("topic" "cloze") nil t)))
         ;; Capture cloze body before the buffer is modified
         (child-body (pcase type
                       ('cloze (org-sm--cloze-body-at sel-beg selected))
                       ('topic selected)))
         (id       (org-id-new))
         (level    (org-current-level)))
    ;; 1. Replace selection with id link in parent.
    ;;    The link description must be single-line; use the same truncated
    ;;    title that the child heading will carry.
    (delete-region sel-beg sel-end)
    (insert (format "[[id:%s][%s]]" id (org-sm--truncate-title selected)))
    ;; 2. Insert child heading after the current subtree
    (save-excursion
      (org-end-of-subtree t t)
      (unless (bolp) (insert "\n"))
      (org-insert-heading nil t (1+ level))
      (insert (org-sm--truncate-title selected))
      (newline)
      (org-set-property "ID" id)
      (org-sm--init-item type)
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
Populated once by `org-sm-start-review', consumed linearly by `org-sm--advance'.
nil means no active session.")

(defconst org-sm--agenda-key "r"
  "Key for the SRS agenda custom command, as set by `org-sm-setup'.")

(defun org-sm--collect-files ()
  "Return list of org files to use as the SRS review pool.
If `org-sm-directory' is set, scans it recursively for files matching
`org-sm-file-filter-regexp'.  Otherwise falls back to `org-agenda-files'."
  (if org-sm-directory
      (seq-filter
       (lambda (f) (string-match-p org-sm-file-filter-regexp f))
       (directory-files-recursively org-sm-directory "\\.org$"))
    (bound-and-true-p org-agenda-files)))

(defun org-sm--agenda-buffer ()
  "Return the live SRS agenda buffer, or nil if it does not exist."
  (get-buffer (format "*Org Agenda(%s)*" org-sm--agenda-key)))

(defun org-sm--ensure-agenda ()
  "Ensure the SRS agenda buffer is up-to-date and return it.
Opens a fresh agenda if none exists; calls `org-agenda-redo' if stale.
`org-agenda-files' is dynamically rebound to the SRS file pool so the
user's global agenda configuration is not affected."
  (require 'org-agenda)
  (let ((org-agenda-files (org-sm--collect-files)))
    (if-let* ((buf (org-sm--agenda-buffer)))
        (with-current-buffer buf
          (org-agenda-redo)
          buf)
      (progn
        (org-agenda nil org-sm--agenda-key)
        (or (org-sm--agenda-buffer)
            (error "org-sm: agenda buffer not found after opening"))))))

(defun org-sm--markers-from-agenda (buf)
  "Return a deduplicated list of org-hd-markers from agenda buffer BUF, in display order."
  (with-current-buffer buf
    (cl-delete-duplicates
     (cl-loop for pos from (point-min) below (point-max)
              for m = (get-text-property pos 'org-hd-marker)
              when m collect m)
     :test #'equal)))

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
configured — then narrows to the first due item.  Call again to rescan
\(e.g. after waiting for a learning card's interval to elapse)."
  (interactive)
  (org-sm-setup)
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
  (let (found)
    (while (and org-sm--queue (not found))
      (let ((marker (pop org-sm--queue)))
        (when (and (marker-buffer marker)
                   (buffer-live-p (marker-buffer marker)))
          (org-sm--goto-marker marker)
          (when (org-sm-type)
            (setq found t)))))
    (if found
        (org-sm--show-review-prompt prev-result)
      (message "org-sm: session complete — nothing more due 🎉%s"
               (if prev-result (format "  (last: %s)" prev-result) "")))))

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
                         (_         "SPC to rate: a/h/g/e"))))
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
     (let ((ivl (org-sm--topic-review)))
       (org-sm--advance (format "topic → due in %d days" ivl))))
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
  "Prompt for FSRS rating via minibuffer, write card, then advance.

After rating, unconditionally advance to the next queued item.  Learning /
relearning cards that received :again or :hard will have SRS_DUE set to a
time a few minutes in the future; they will reappear naturally the next time
`org-sm-start-review' is called (which rescans the agenda)."
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
         (due-time (parse-iso8601-time-string (fsrs-card-due new-card)))
         (next-due (format-time-string "%F %H:%M" due-time)))
    (org-sm--write-card new-card)
    (org-sm--log-review "cloze" rating)
    (org-sm--advance (format "cloze %s → due %s" rating next-due))))

;;;###autoload
(defun org-sm-dismiss ()
  "Dismiss current SRS heading, then advance to next due item."
  (interactive)
  (unless (org-sm-type) (user-error "Not on an SRS heading"))
  (org-sm--log-review "dismissed")
  (org-sm--schedule (parse-iso8601-time-string "2999-12-31T00:00:00Z"))
  (org-sm--advance "dismissed"))

;;;; ---- Agenda integration --------------------------------------------------

(defun org-sm--agenda-skip-non-srs ()
  "Skip agenda entries that are not SRS items, are dismissed, or not yet due.
All card types use precise SCHEDULED timestamps; an entry is due only when
its scheduled time is ≤ now."
  (let ((skip (when-let* ((m (org-get-at-bol 'org-hd-marker)))
                (org-with-point-at m
                  (or (unless (org-entry-get nil "SRS_TYPE") t)
                      (when-let* ((t_ (org-get-scheduled-time nil)))
                        (> (float-time t_) (float-time))))))))
    (when skip
      (line-end-position))))

(defun org-sm--agenda-type-label ()
  "Return a short type label for the current agenda entry."
  (when-let* ((m (org-get-at-bol 'org-hd-marker)))
    (org-with-point-at m
      (pcase (org-entry-get nil "SRS_TYPE")
        ("topic" "[topic]")
        ("cloze" "[cloze]")
        (_       "")))))

(defvar org-sm--setup-done nil
  "Non-nil after `org-sm-setup' has successfully run.")

;;;###autoload
(defun org-sm-setup ()
  "Initialize org-sm: register the SRS agenda view and set up the scheduler.
Idempotent — safe to call multiple times; work is only done once.

Called automatically by `org-sm-mode' and `org-sm-start-review', so manual
invocation is only needed if you want to force a re-initialization (e.g.
after changing `org-sm-files' at runtime)."
  (interactive)
  (unless org-sm--setup-done
    (require 'org)
    (require 'org-agenda)
    (require 'fsrs)
    (setq org-sm--scheduler
          (fsrs-make-scheduler
           :desired-retention 0.9
           :learning-steps    '((1 :minute) (10 :minute))
           :enable-fuzzing-p  t))
    (add-to-list
     'org-agenda-custom-commands
     `(,org-sm--agenda-key "SRS Review"
       ((agenda ""
         ((org-agenda-span             'day)
          (org-agenda-start-on-weekday nil)
          (org-agenda-skip-function    '(org-sm--agenda-skip-non-srs))
          (org-agenda-sorting-strategy '(priority-down scheduled-up))
          (org-agenda-prefix-format    "  %-10(org-sm--agenda-type-label) "))))
       nil nil))
    (setq org-sm--setup-done t))
  (when (called-interactively-p 'interactive)
    (message "org-sm: %s" (if org-sm--setup-done "initialized" "already initialized"))))

;;;; ---- Minor mode ----------------------------------------------------------

;;;###autoload
(define-minor-mode org-sm-mode
  "Minor mode for SuperMemo-style incremental reading.
Provides only UI enhancements (cloze font-lock); no keybindings are added."
  :lighter " SRS"
  :group   'org-sm
  (if org-sm-mode
      (font-lock-add-keywords nil (org-sm--cloze-font-lock-keywords) 'append)
    (font-lock-remove-keywords nil (org-sm--cloze-font-lock-keywords)))
  (when (fboundp 'font-lock-flush)
    (font-lock-flush)))

(provide 'org-sm)
;;; org-sm.el ends here
