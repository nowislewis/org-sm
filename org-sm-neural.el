;;; org-sm-neural.el --- Association-driven review sessions -*- lexical-binding: t -*-

;; Author: Lewis Liu
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: org, spaced-repetition, incremental-reading

;;; Commentary:
;;
;; Spaced repetition answers "is this still in memory?".  It cannot answer
;; "how does this connect to everything else?": cards arrive one at a time,
;; by date, stripped of their neighbourhood.  Knowledge stays retrievable but
;; not linked -- the fragmentation problem.
;;
;; A neural session ignores dates and walks one neighbourhood instead: the
;; card that held this passage, the cards extracted from it, its siblings,
;; and every card it links to or that links to it.  SuperMemo calls this
;; neural review; the mechanism is breadth-first spreading from one seed,
;; capped at `org-sm-neural-size' so the cluster fits one sitting.
;;
;; The session reuses the normal review flow: grading works as usual (an
;; early review), and `s' skips a card when you only want to read it.

;;; Code:

(require 'seq)
(require 'org)
(require 'org-id)
(require 'org-sm-card)

(declare-function org-sm-type "org-sm")
(declare-function org-sm-body-bounds "org-sm")
(declare-function org-sm--goto-enclosing-card "org-sm")
(declare-function org-sm--dismissed-p "org-sm")
(declare-function org-sm--review-markers "org-sm")

(defcustom org-sm-neural-size 6
  "How many cards one neural session visits.
Small enough to read as one cluster rather than as another queue."
  :type 'integer
  :group 'org-sm)

(defun org-sm-neural--card-marker ()
  "Return a marker on the nearest card enclosing point, or nil."
  (save-excursion
    (when (org-sm--goto-enclosing-card) (point-marker))))

(defun org-sm-neural--structural ()
  "Return markers of the parent card, child cards and sibling cards at point.
Point must be on a card heading.  Parent first: the passage this card was
extracted from is the context most likely to make it make sense again."
  (let (parent children siblings)
    (save-excursion
      (when (org-up-heading-safe)
        (setq parent (org-sm-neural--card-marker))))
    (save-excursion
      (let ((end (save-excursion (org-end-of-subtree t t))))
        (while (and (outline-next-heading) (< (point) end))
          (when (org-sm-type) (push (point-marker) children)))))
    (save-excursion
      (let ((me (point)))
        (when (org-up-heading-safe)
          (let ((end (save-excursion (org-end-of-subtree t t))))
            (while (and (outline-next-heading) (< (point) end))
              (when (and (/= (point) me) (org-sm-type))
                (push (point-marker) siblings)))))))
    (delq nil (append (list parent) (nreverse children) (nreverse siblings)))))

(defun org-sm-neural--forward-links ()
  "Return markers of cards that the current card body links to by ID."
  (let ((bounds (org-sm-body-bounds)) markers)
    (save-excursion
      (goto-char (car bounds))
      (while (re-search-forward org-link-bracket-re (cdr bounds) t)
        (let ((target (match-string-no-properties 1)))
          (when-let* (((string-prefix-p "id:" target))
                      (m (org-id-find (substring target 3) 'marker)))
            (push m markers)))))
    (nreverse markers)))

(defun org-sm-neural--backlinks (id)
  "Return markers of cards anywhere under `org-sm-directory' linking to ID.
One ripgrep search per seed instead of one vault walk per visited card."
  (when id
    (delq nil
          (mapcar (lambda (line)
                    (when (string-match "\\`\\(.+?\\):\\([0-9]+\\):" line)
                      (let ((file (match-string 1 line))
                            (n (string-to-number (match-string 2 line))))
                        (with-current-buffer (find-file-noselect file)
                          (org-with-wide-buffer
                           (goto-char (point-min))
                           (forward-line (1- n))
                           (org-sm-neural--card-marker))))))
                  (org-sm-rg "-n" "--no-heading" "--glob" "*.org"
                             "-F" (concat "[[id:" id "]"))))))

(defun org-sm-neural--neighbours (marker)
  "Return markers related to the card at MARKER, nearest relation first."
  (org-with-point-at marker
    (append (org-sm-neural--structural)
            (org-sm-neural--forward-links)
            (org-sm-neural--backlinks (org-entry-get nil "ID")))))

(defun org-sm-neural-collect (seed limit)
  "Return up to LIMIT card markers reached by spreading from SEED.
SEED comes first.  Dismissed cards are walked through but never queued: the
usual workflow extracts clozes and then dismisses the source topic, so the
dismissed parent is exactly the bridge between sibling clozes.  The walk
stops after a bounded number of visits so a densely linked vault cannot
turn one keypress into a full scan."
  (let ((seen (make-hash-table :test #'equal))
        (queue (list seed))
        (budget (* 8 limit))
        result)
    (while (and queue (< (length result) limit) (> budget 0))
      (let* ((marker (pop queue))
             (key (cons (marker-buffer marker) (marker-position marker))))
        (unless (gethash key seen)
          (puthash key t seen)
          (setq budget (1- budget))
          (when (org-with-point-at marker (org-sm-type))
            (unless (org-with-point-at marker (org-sm--dismissed-p))
              (push marker result))
            (when (< (length result) limit)
              (setq queue (append queue (org-sm-neural--neighbours marker))))))))
    (nreverse result)))

;;;###autoload
(defun org-sm-neural-start (&optional seed)
  "Review the neighbourhood of SEED instead of today's queue.
SEED defaults to the card at point, or the card under the cursor in
`org-sm-tree'.  It comes first, then its parent, children, siblings and
linked cards, capped at `org-sm-neural-size'.  Grading works as an early
review; press `s' in the review prompt to read a card and move on unchanged."
  (interactive)
  (require 'org-sm)
  (let ((seed (or seed
                  (get-text-property (line-beginning-position) 'org-sm-marker)
                  (org-sm-neural--card-marker)
                  (user-error "Point is not on an org-sm card"))))
    (let ((markers (org-sm-neural-collect seed org-sm-neural-size)))
      (if (cdr markers)
          (org-sm--review-markers markers (format "neural, %d" (length markers)))
        (message "org-sm: this card has no related cards yet")))))

(provide 'org-sm-neural)
;;; org-sm-neural.el ends here
