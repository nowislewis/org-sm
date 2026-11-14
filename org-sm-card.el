;;; org-sm-card.el --- Shared card metadata and scanning -*- lexical-binding: t; -*-

;;; Commentary:
;; Lightweight read-only storage interface.  No FSRS or review UI dependency.
;; File scans use temporary Org buffers without running user mode hooks.

;;; Code:
(require 'org)
(require 'cl-lib)
(require 'parse-time)

(defgroup org-sm nil
  "SuperMemo-style incremental reading for org-mode."
  :group 'org
  :prefix "org-sm-")

(defcustom org-sm-directory nil
  "Root directory scanned by `org-sm-files' for SRS items."
  :type '(choice (const :tag "Unset" nil) directory)
  :group 'org-sm)

;;;###autoload
(defun org-sm-files ()
  "Return org files under `org-sm-directory' that contain SRS items.
Rescanned live on each call, so the result never goes stale."
  (org-sm-rg "-l" "--glob" "*.org" ":SRS_TYPE:"))

(defun org-sm-rg (&rest args)
  "Run ripgrep with ARGS over `org-sm-directory'; return its output lines.
The one way org-sm searches the vault: file discovery, backlinks,
provenance routing and duplicate detection all go through here, so they
agree on which directory is the vault.  Returns nil when the directory is
unset; signals when ripgrep is missing, since every caller depends on it."
  (when org-sm-directory
    (unless (executable-find "rg")
      (user-error "org-sm: ripgrep (rg) not found on PATH"))
    (with-temp-buffer
      (apply #'call-process "rg" nil '(t nil) nil
             (append args (list (expand-file-name org-sm-directory))))
      (split-string (buffer-string) "\n" t))))

(defun org-sm-elapsed-days (start end)
  "Return elapsed fractional days from START to END (Emacs time values)."
  (/ (float-time (time-subtract end start)) 86400.0))

(defun org-sm-book-label (&optional file)
  "Return a clean book/topic title for FILE, stripping Denote metadata."
  (when-let* ((path (or file (buffer-file-name)))
              (name (file-name-nondirectory path)))
    (replace-regexp-in-string
     "\\`[0-9T:=-]+" ""
     (file-name-sans-extension
      (replace-regexp-in-string "__.*" "" name)))))

(defconst org-sm-card--priority-regexp
  "[ \t]*\\*+[ \t]+\\(?:[A-Z]+[ \t]+\\)?\\[#\\([A-Z0-9]\\)\\]"
  "Regexp matching an explicit priority cookie on a heading line.
`org-entry-get' cannot be used here: it substitutes `org-priority-default'
for headings without a cookie, which would make \"unprioritized\" and \"[#B]\"
indistinguishable.")

(defun org-sm-card--entry ()
  "Read current entry metadata without traversing descendants."
  ;; Non-card headings need no scheduling or ID reads.
  (let ((type (org-entry-get nil "SRS_TYPE")))
    (when (member type '("topic" "cloze"))
      (let ((last (org-entry-get nil "SRS_LAST"))
            (scheduled (org-get-scheduled-time nil))
            (dismissed (and (member "dismissed" (org-get-tags nil t)) t))
            (priority (save-excursion
                        (org-back-to-heading t)
                        (and (looking-at org-sm-card--priority-regexp)
                             (string-to-char (match-string 1)))))
            (end (save-excursion (outline-next-heading) (point)))
            (reviews 0)
            (again-count 0)
            dis-date)
	;; Historical explain/postpone entries used the topic prefix but were
	;; not repetitions.  Read them correctly without rewriting user files.
	;; `:again-count' rides along on the same scan so leech detection costs
	;; nothing extra on a whole-vault sweep.
	(save-excursion
          (org-back-to-heading t)
          (while (re-search-forward
                  "^[ \t]*- \\(topic\\|cloze\\|dismissed\\)\\b\\(.*\\)$" end t)
            (let ((kind (match-string 1))
                  (rest (match-string 2)))
              (if (equal kind "dismissed")
                  (when (and (null dis-date)
                             (string-match
                              "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" rest))
                    (setq dis-date (match-string 1 rest)))
                (unless (string-match-p "\\`[ \t]+\\(?:postpone\\|explain\\)\\b" rest)
                  (cl-incf reviews))
                (when (and (equal kind "cloze")
                           (string-match-p "\\`[ \t]+:again\\b" rest))
                  (cl-incf again-count))))))
	(let* ((last-time (and last (parse-iso8601-time-string last)))
               (days (when (and last-time scheduled)
                       (org-sm-elapsed-days last-time scheduled))))
          (list :type (intern type) :id (org-entry-get nil "ID")
                :title (org-get-heading t t t t)
                :priority priority
                :dismissed dismissed
                :date (or (and dismissed dis-date)
                          (and last-time (format-time-string "%F" last-time))
                          (and scheduled (format-time-string "%F" scheduled)))
                :interval-days (and days (max 0 (round days)))
                :interval-mins (and days (> days 0) (< days 0.5)
                                    (max 1 (round (* days 1440))))
                :reviews reviews
                :again-count again-count
                :child-topics 0 :child-clozes 0))))))

(defun org-sm-card-priority (card)
  "Return CARD's priority rank, 0 being highest ([#A] -> 0, [#B] -> 1 ...).
A card without a cookie ranks as `org-priority-default', the same rule
`org-get-priority' and `org-sm-topic-afactor' already apply."
  (- (or (plist-get card :priority) org-priority-default)
     org-priority-highest))

(defun org-sm-card-at-point ()
  "Read the current card and count all descendant topic/cloze cards.
Preserve point, restriction and buffer contents.  Return nil for non-cards."
  (save-excursion
    (org-back-to-heading t)
    (when-let* ((card (org-sm-card--entry)))
      (let ((end (save-excursion (org-end-of-subtree t t))))
        (outline-next-heading)
        (while (< (point) end)
          (pcase (org-entry-get nil "SRS_TYPE")
            ("topic" (cl-incf (plist-get card :child-topics)))
            ("cloze" (cl-incf (plist-get card :child-clozes))))
          (outline-next-heading)))
      card)))

(defun org-sm-cards-in-file (file)
  "Read all cards in FILE without visiting it or retaining a file buffer.
Use a visiting buffer's contents if present, including unsaved edits.
The reverse outline stack accumulates descendant counts in linear time."
  (let ((visiting (get-file-buffer file)) nodes cards stack)
    (with-temp-buffer
      (if visiting
          (insert (with-current-buffer visiting
                    (save-restriction
                      (widen)
                      (buffer-substring-no-properties (point-min) (point-max)))))
        (insert-file-contents file))
      (delay-mode-hooks (org-mode))
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (beginning-of-line)
        (push (cons (org-outline-level) (org-sm-card--entry)) nodes)
        (forward-line 1)))
    (dolist (node nodes)
      (let ((level (car node)) (card (cdr node)) (topics 0) (clozes 0))
        (while (and stack (> (caar stack) level))
          (let ((child (pop stack)))
            (cl-incf topics (nth 1 child))
            (cl-incf clozes (nth 2 child))))
        (when card
          (setf (plist-get card :child-topics) topics
                (plist-get card :child-clozes) clozes)
          (push card cards))
        (push (list level (+ topics (if (eq (plist-get card :type) 'topic) 1 0))
                    (+ clozes (if (eq (plist-get card :type) 'cloze) 1 0))) stack)))
    cards))

;;;; ---- Lifecycle predicates -------------------------------------------------
;; Pure functions over the metadata plist: no buffer access, no I/O, so both the
;; review UI and the web front-end can share one definition of "needs work".

(defun org-sm-stalled-p (card &optional threshold)
  "Return non-nil when CARD keeps coming back but never turns into anything.
A topic repassed THRESHOLD (default 4) times with no child card is the shape of
avoidance: the reader keeps pushing it, nothing gets extracted, nothing gets
dismissed.  Callers use this to stop offering the same `next' key."
  (and (eq (plist-get card :type) 'topic)
       (>= (or (plist-get card :reviews) 0) (or threshold 4))
       (= (or (plist-get card :child-topics) 0) 0)
       (= (or (plist-get card :child-clozes) 0) 0)))

(defun org-sm-leech-p (card &optional threshold)
  "Return non-nil when CARD keeps failing recall, i.e. it wants rewriting.
THRESHOLD defaults to 2 `again' ratings: one lapse is noise, two means the
card is probably testing something the reader never encoded."
  (and (eq (plist-get card :type) 'cloze)
       (>= (or (plist-get card :again-count) 0) (or threshold 2))))

(provide 'org-sm-card)
;;; org-sm-card.el ends here
