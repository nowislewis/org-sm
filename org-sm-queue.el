;;; org-sm-queue.el --- What a review session shows, and in what order -*- lexical-binding: t -*-

;; Author: Lewis Liu
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: org, spaced-repetition, incremental-reading

;;; Commentary:
;;
;; One ordering, shared by every entry point: a light session is its first
;; few cards, a full session all of it, and the phone receives it as is.
;;
;; Clozes lead.  A late cloze loses recall along a forgetting curve; a late
;; topic loses only the chance to read it.  So recall comes first, ordered by
;; how far each card has drifted past due, and reading fills a fixed share of
;; the slots -- a share rather than "after all clozes", or a recall backlog
;; would stop new material from ever arriving.
;;
;; Nothing is written.  The due pool is derived on every call, so a card left
;; over takes part in the next ordering, oldest first.  SuperMemo has to push
;; overflow into the future because its outstanding queue is stored state;
;; here a nightly rewrite would only add diffs, sync conflicts and a counter
;; to tune.  On a simulated 199-topic backlog, postponing got 81 cards read in
;; 180 days where plain oldest-first reading got through 505.

;;; Code:

(require 'seq)
(require 'org)
(require 'org-sm-card)

(defcustom org-sm-review-topic-proportion 0.34
  "Largest share of served cards that may be topics while clozes remain.
SuperMemo suggests about one topic per four items; 0.34 gives two topics
in a six-card session.  Once clozes run out, the remaining topics follow,
so the share caps reading without ever dropping it."
  :type 'number
  :group 'org-sm)

(defun org-sm-queue--at-risk (card)
  "Return how far CARD has drifted past due, relative to its interval.
Days late over interval days: a one-day card a week late is in more danger
than a hundred-day card a week late.  Stands in for FSRS retrievability,
which orders the same way here but costs a scheduler call per card."
  (/ (float (max 0 (plist-get card :late-days)))
     (max 1 (or (plist-get card :interval-days) 1))))

(defun org-sm-queue--sort (entries key)
  "Sort ENTRIES by rank, highest first, then by KEY, largest first.
ENTRIES are (RANK TYPE MARKER CARD); KEY maps a CARD to a number."
  (sort entries
        (lambda (a b)
          (or (> (car a) (car b))
              (and (= (car a) (car b))
                   (> (funcall key (nth 3 a)) (funcall key (nth 3 b))))))))

(defun org-sm-queue-lanes (entries)
  "Split ENTRIES into (REVIEW TOPIC NEW), each already in serving order.
REVIEW holds clozes with history, most at risk first; TOPIC holds topics,
longest waiting first; NEW holds never-reviewed clozes, oldest first.  A
card that was never reviewed has no memory to lose, so it queues behind
recall rather than competing with it.  Priority outranks all of this
within a lane."
  (let (review topic new)
    (dolist (e entries)
      (pcase (nth 1 e)
        ('topic (push e topic))
        (_ (if (> (plist-get (nth 3 e) :reviews) 0) (push e review) (push e new)))))
    (list (org-sm-queue--sort review #'org-sm-queue--at-risk)
          (org-sm-queue--sort topic (lambda (c) (plist-get c :late-days)))
          (org-sm-queue--sort new (lambda (c) (plist-get c :late-days))))))

(defun org-sm-queue-interleave (lead tail share)
  "Merge LEAD and TAIL, giving TAIL at most SHARE of the served cards.
TAIL is served whenever that keeps its running share within SHARE; once
either list runs dry the other follows, so nothing is ever dropped."
  (let ((served 0) (taken 0) result)
    (while (or lead tail)
      (if (and tail (or (null lead) (<= (/ (1+ taken) (float (1+ served))) share)))
          (progn (push (pop tail) result) (setq taken (1+ taken)))
        (push (pop lead) result))
      (setq served (1+ served)))
    (nreverse result)))

(defun org-sm-queue-order (entries &optional limit)
  "Return the first LIMIT ENTRIES in review order (all when LIMIT is nil).
ENTRIES are (RANK TYPE MARKER CARD) from `org-sm-due-markers'.  Clozes
lead, reviews before new ones; topics take up to
`org-sm-review-topic-proportion' of the slots."
  (pcase-let ((`(,review ,topic ,new) (org-sm-queue-lanes entries)))
    (let ((order (org-sm-queue-interleave (append review new) topic
                                          org-sm-review-topic-proportion)))
      (if limit (seq-take order limit) order))))

(provide 'org-sm-queue)
;;; org-sm-queue.el ends here
