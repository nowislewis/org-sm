;;; org-sm-queue-test.el --- Serving order, lanes, lifecycle -*- lexical-binding: t; -*-
(require 'ert)
(require 'org-sm)

(defun org-sm-queue-test--card (title type &optional prio sched-delta extra)
  "Return Org text for a card titled TITLE of TYPE.
PRIO is a priority letter, SCHED-DELTA days from today, EXTRA raw lines
appended after the property drawer."
  (format "* %s%s\nSCHEDULED: <%s>\n:PROPERTIES:\n:SRS_TYPE: %s\n:SRS_LAST: %s\n:END:\n%s"
          (if prio (format "[#%c] " prio) "")
          title
          (format-time-string "%F %a"
                              (time-add (current-time)
                                        (days-to-time (or sched-delta 0))))
          type
          (format-time-string "%FT%TZ"
                              (time-add (current-time)
                                        (days-to-time (- (or sched-delta 0) 10)))
                              "UTC0")
          (or extra "")))

(defmacro org-sm-queue-test--with-vault (text &rest body)
  "Run BODY with `org-sm-files' returning one temp file containing TEXT."
  (declare (indent 1))
  `(let ((file (make-temp-file "org-sm-queue-" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,text))
           (cl-letf (((symbol-function 'org-sm-files) (lambda () (list file))))
             ,@body))
       (when-let* ((buf (get-file-buffer file)))
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf))
       (delete-file file))))

(defun org-sm-queue-test--entries (spec)
  "Build (RANK TYPE MARKER CARD) entries from SPEC, a list of (RANK TYPE . CARD)."
  (let ((n 0))
    (mapcar (lambda (s) (list (car s) (cadr s) (cl-incf n)
                              (append (caddr s) (list :title (format "c%d" n)))))
            spec)))

(defun org-sm-queue-test--names (order)
  "Name each entry of ORDER by its fake marker number: c1, c2 ..."
  (mapcar (lambda (e) (format "c%d" (nth 2 e))) order))

;;;; ---- Serving order ---------------------------------------------------------

(ert-deftest org-sm-queue-order-clozes-lead-and-topics-cap ()
  (let* ((entries (mapcar (lambda (s)
                            (list 1000 (nth 1 s) (+ 100 (car s))
                                  (list :type (nth 1 s) :late-days (nth 2 s)
                                        :interval-days (nth 3 s) :reviews 3)))
                          '((1 cloze 3 10) (2 cloze 3 10) (3 cloze 3 10) (4 cloze 3 10)
                            (5 cloze 3 10) (6 cloze 3 10)
                            (7 topic 9 1) (8 topic 9 1) (9 topic 9 1)
                            (10 topic 9 1) (11 topic 9 1) (12 topic 9 1))))
         (order (org-sm-queue-order entries 6)))
    (should (= (length order) 6))
    ;; Two of six may be topics; nothing is dropped, so a 7th card is a topic.
    (should (= (seq-count (lambda (e) (eq (nth 1 e) 'topic)) (seq-take order 6)) 2))
    (should (= (length (org-sm-queue-order entries)) 12))))

(ert-deftest org-sm-queue-order-backfills-a-scarce-side ()
  (let* ((entries (org-sm-queue-test--entries
                   (append (make-list 1 (list 1000 'cloze '(:reviews 2)))
                           (mapcar (lambda (i) (list 1000 'topic (list :late-days i)))
                                   (number-sequence 1 10)))))
         (order (org-sm-queue-order entries 6)))
    (should (= (length order) 6))
    ;; One cloze, five topics: the share caps reading only while clozes last.
    (should (= (length (org-sm-queue-order entries)) 11))))

(ert-deftest org-sm-queue-order-risk-and-waiting-decide ()
  (let* ((entries (list (list 1000 'cloze 1 '(:type cloze :reviews 3 :late-days 2 :interval-days 40))
                        (list 1000 'cloze 2 '(:type cloze :reviews 3 :late-days 10 :interval-days 5))
                        (list 1000 'cloze 3 '(:type cloze :reviews 3 :late-days 30 :interval-days 200))
                        (list 1000 'topic 4 '(:type topic :late-days 1))
                        (list 1000 'topic 5 '(:type topic :late-days 99))))
         (titles (org-sm-queue-test--names (org-sm-queue-order entries))))
    ;; Clozes by risk = late/interval (10/5 > 30/200 > 2/40), topics by wait
    ;; (99 > 1), merged C C T: the third slot is the first topic's.
    (should (equal titles '("c2" "c3" "c5" "c1" "c4")))))

(ert-deftest org-sm-queue-order-new-clozes-follow-reviews ()
  (let* ((entries (list (list 1000 'cloze 1 '(:type cloze :reviews 4 :late-days 1 :interval-days 30))
                        (list 1000 'cloze 2 '(:type cloze :reviews 0 :late-days 40 :interval-days 1))))
         (titles (org-sm-queue-test--names (org-sm-queue-order entries))))
    ;; A never-reviewed card has no memory to lose; it does not jump the queue.
    (should (equal titles '("c1" "c2")))))

(ert-deftest org-sm-queue-order-priority-works-within-a-lane ()
  ;; Type decides the quota, priority the order inside a type: an [#A] cloze
  ;; leads the clozes, but an [#A] topic still waits for a topic slot.
  (let* ((entries (list (list 1000 'cloze 1 '(:type cloze :reviews 2 :late-days 9 :interval-days 1))
                        (list 2000 'cloze 2 '(:type cloze :reviews 2 :late-days 1 :interval-days 30))
                        (list 3000 'topic 3 '(:type topic :late-days 1))))
         (titles (org-sm-queue-test--names (org-sm-queue-order entries))))
    (should (equal titles '("c2" "c1" "c3")))))

(ert-deftest org-sm-queue-light-is-the-head-of-the-full-queue ()
  (org-sm-queue-test--with-vault
      (concat (mapconcat (lambda (i)
                           (org-sm-queue-test--card (format "C%d" i) "cloze" nil -1
                                                    ":LOGBOOK:\n- cloze :good 1d [2026-01-01]\n:END:\n"))
                         (number-sequence 1 8) "")
              (mapconcat (lambda (i) (org-sm-queue-test--card (format "T%d" i) "topic" nil -1))
                         (number-sequence 1 8) ""))
    (let ((all (org-sm-due-markers #'org-sm-due-p)))
      (should (= (length all) 16))
      (should (= (length (org-sm-queue-order all 6)) 6))
      (should (equal (org-sm-queue-order all 6)
                     (seq-take (org-sm-queue-order all) 6))))))

(ert-deftest org-sm-queue-review-start-light-uses-one-order ()
  (org-sm-queue-test--with-vault
      (concat (mapconcat (lambda (i) (org-sm-queue-test--card (format "C%d" i) "cloze" nil -1))
                         (number-sequence 1 8) "")
              (mapconcat (lambda (i) (org-sm-queue-test--card (format "T%d" i) "topic" nil -1))
                         (number-sequence 1 8) ""))
    (let (queued)
      (cl-letf (((symbol-function 'org-sm--review-markers)
                 (lambda (markers &rest _)
                   (setq queued (mapcar (lambda (m)
                                          (org-with-point-at m (org-sm-type)))
                                        markers)))))
        (org-sm-review-start 6))
      (should (= (length queued) 6))
      (should (= (seq-count (lambda (ty) (eq ty 'topic)) queued) 2)))))

;;;; ---- Priority -------------------------------------------------------------

(ert-deftest org-sm-queue-cycle-priority-and-inherit ()
  (with-temp-buffer
    (org-mode)
    (insert (org-sm-queue-test--card "Parent" "topic" nil 1) "body text here\n")
    (goto-char (point-min))
    (let ((seen nil))
      ;; No cookie counts as B, so the first step is A; then C, B, A again.
      (dotimes (_ 4)
        (org-sm-cycle-priority)
        (push (plist-get (org-sm-card-at-point) :priority) seen))
      (should (equal (nreverse seen) '(?A ?C ?B ?A))))
    (goto-char (point-min))
    (let ((id (org-sm-extract 'topic "body" 0 4)))
      (re-search-forward (regexp-quote id))
      (org-back-to-heading t)
      (should (eq (plist-get (org-sm-card-at-point) :priority) ?A)))))

;;;; ---- Lifecycle ------------------------------------------------------------

(ert-deftest org-sm-queue-lifecycle-predicates ()
  (with-temp-buffer
    (org-mode)
    (insert (org-sm-queue-test--card "Stalled" "topic" nil 0
                                     ":LOGBOOK:\n- topic  a=1.5  1d  [2026-01-01]\n- topic  2d  [2026-01-02]\n- topic  3d  [2026-01-03]\n- topic  postpone 3d [2026-01-03]\n- topic  4d  [2026-01-04]\n:END:\n")
            (org-sm-queue-test--card "Leech" "cloze" nil 0
                                     ":LOGBOOK:\n- cloze  :again  10m  [2026-01-01]\n- cloze  :good  1d  [2026-01-02]\n- cloze  :again  10m  [2026-01-03]\n:END:\n"))
    (goto-char (point-min))
    (let ((stalled (org-sm-card-at-point)))
      (should (= (plist-get stalled :reviews) 4))
      (should (org-sm-stalled-p stalled))
      (should-not (org-sm-stalled-p stalled 5)))
    (outline-next-heading)
    (let ((leech (org-sm-card-at-point)))
      (should (= (plist-get leech :again-count) 2))
      (should (org-sm-leech-p leech))
      (should-not (org-sm-stalled-p leech)))))

;;; org-sm-queue-test.el ends here
