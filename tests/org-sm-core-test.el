;;; org-sm-core-test.el --- Storage and action contracts -*- lexical-binding: t; -*-
(require 'ert)
(require 'org-sm)

(defconst org-sm-test--tree
  "* Parent\nSCHEDULED: <2026-10-11 Sun 00:00>\n:PROPERTIES:\n:SRS_TYPE: topic\n:SRS_LAST: 2026-10-01T00:00:00+08:00\n:END:\n:LOGBOOK:\n- topic a=1.5 10d [2026-10-01 Thu]\n- topic postpone 2d [2026-10-01 Thu]\n- topic explain [2026-10-01 Thu]\n:END:\n** Wrapper\n*** Child\n:PROPERTIES:\n:SRS_TYPE: cloze\n:END:\n* Sibling\n:PROPERTIES:\n:SRS_TYPE: topic\n:END:\n")

(ert-deftest org-sm-core-scan-equivalence-and-no-side-effects ()
  (let ((file (make-temp-file "org-sm-core-" nil ".org"))
        (hook-runs 0))
    (unwind-protect
        (progn
          (with-temp-file file (insert org-sm-test--tree))
          (let* ((org-mode-hook (list (lambda () (cl-incf hook-runs))))
                 (cards (org-sm-cards-in-file file)))
            (should (= hook-runs 0))
            (should-not (get-file-buffer file))
            (should (= (length cards) 3))
            (with-temp-buffer
              (delay-mode-hooks (org-mode))
              (insert org-sm-test--tree)
              (goto-char (point-min))
              (let ((before (buffer-string)) (pos (point)))
                (should (equal (org-sm-card-at-point) (car cards)))
                (should (= (plist-get (car cards) :reviews) 1))
                (should (= (plist-get (car cards) :child-clozes) 1))
                (should (= (plist-get (car cards) :child-topics) 0))
                (should (= (point) pos))
                (should (equal before (buffer-string)))))))
      (delete-file file))))

(ert-deftest org-sm-core-postpone-preserves-interval-and-review-count ()
  (with-temp-buffer
    (org-mode) (insert org-sm-test--tree) (goto-char (point-min))
    (let* ((event (org-sm-grade-reschedule 7)))
      (should (= (plist-get event :postpone-days) 7))
      (should (= (plist-get event :new-days) 7))
      (should (= (plist-get (org-sm-card-at-point) :interval-days) 7))
      (should (= (plist-get (org-sm-card-at-point) :reviews) 1))
      (should-error (org-sm-grade-reschedule -1)))))

(ert-deftest org-sm-core-observer-failure-does-not-fail-dismiss ()
  (with-temp-buffer
    (org-mode) (insert org-sm-test--tree) (goto-char (point-min))
    (let ((received nil)
          (org-sm-after-review-functions (list (lambda (_) (error "extension failed")))))
      (let ((org-sm-after-card-change-functions (list (lambda (ev) (push ev received)))))
        (should (eq (plist-get (org-sm-dismiss) :action) 'dismiss))
        (should (plist-get (org-sm-card-at-point) :dismissed))
        (org-sm-item-undismiss)
        (should-not (plist-get (org-sm-card-at-point) :dismissed))
        (should (equal (mapcar (lambda (ev) (plist-get ev :action)) received)
                       '(restore dismiss)))))))

(ert-deftest org-sm-core-time-semantics ()
  (should (= (org-sm-elapsed-days (seconds-to-time 0) (seconds-to-time 43200)) 0.5))
  (with-temp-buffer
    (org-mode) (insert "* Card\nSCHEDULED: <2026-10-02 Fri 00:01>\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'current-time)
               (lambda () (encode-time 0 59 23 1 10 2026))))
      (should (= (org-sm-sched-day-delta) 1)))))

(ert-deftest org-sm-core-priority-order ()
  (let ((file (make-temp-file "org-sm-prio-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "* [#C] Low\n:PROPERTIES:\n:SRS_TYPE: topic\n:END:\n"
                    "* Medium\n:PROPERTIES:\n:SRS_TYPE: topic\n:END:\n"
                    "* [#A] High\n:PROPERTIES:\n:SRS_TYPE: topic\n:END:\n"))
          (cl-letf (((symbol-function 'org-sm-files) (lambda () (list file))))
            (let* ((markers (mapcar #'caddr (org-sm-queue-order (org-sm-due-markers))))
                   (titles (mapcar (lambda (m)
                                     (org-with-point-at m
                                       (org-get-heading t t t t)))
                                   markers)))
              (mapc (lambda (m) (set-marker m nil)) markers)
              (should (equal titles '("High" "Medium" "Low"))))))
      (when-let* ((buf (get-file-buffer file))) (kill-buffer buf))
      (delete-file file))))

(ert-deftest org-sm-core-scan-uses-unsaved-buffer ()
  (let* ((file (make-temp-file "org-sm-unsaved-" nil ".org"))
         (buffer (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buffer
          (insert org-sm-test--tree)
          (org-narrow-to-subtree)
          (let ((before (buffer-string)))
            (should (= (length (org-sm-cards-in-file file)) 3))
            (should (buffer-narrowed-p))
            (should (buffer-modified-p))
            (should (equal before (buffer-string)))))
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer)
      (delete-file file))))

(ert-deftest org-sm-core-edit-and-extract-notifications ()
  (with-temp-buffer
    (org-mode) (insert org-sm-test--tree) (goto-char (point-min))
    (let (actions child-id cloze-id)
      (let ((org-sm-after-card-change-functions
             (list (lambda (event) (push (plist-get event :action) actions)))))
        (org-sm-set-body "alpha beta\n")
        (goto-char (point-min))
        (org-sm-set-title "Renamed")
        (goto-char (point-min))
        (setq child-id (org-sm-extract 'topic "alpha" 0 5))
        (should (stringp child-id))
        (should (equal (org-sm-body-text)
                       (format "⌈alpha[[id:%s][<T>]] beta" child-id)))
        (let* ((body (org-sm-body-text))
               (bpos (string-match "beta" body)))
          (setq cloze-id (org-sm-extract 'cloze "beta" bpos (+ bpos 4))))
        (save-excursion
          (re-search-forward (concat ":ID:[ \t]+" (regexp-quote cloze-id)))
          (org-back-to-heading t)
          (should (equal (org-sm-body-text) "alpha {{beta}}")))
        (should (equal actions '(extract extract edit edit))))))
  ;; Verify multiline extracted span highlights across single-line jit-lock passes:
  (with-temp-buffer
    (org-mode)
    (org-sm-mode 1)
    (insert "* Multiline\n:PROPERTIES:\n:SRS_TYPE: topic\n:END:\n第一行文字\n第二行文字\n")
    (goto-char (point-min))
    (org-sm-extract 'topic "第一行文字\n第二行\n" 0 9)
    (let ((p1 (save-excursion (goto-char (point-min)) (search-forward "第一行") (1- (point))))
          (p2 (save-excursion (goto-char (point-min)) (search-forward "第二") (1- (point)))))
      (save-excursion
        (goto-char p1)
        (font-lock-fontify-region (line-beginning-position) (line-end-position)))
      (should (memq 'org-sm-extracted-face (ensure-list (get-text-property p1 'face))))
      (should (memq 'org-sm-extracted-face (ensure-list (get-text-property p2 'face)))))))

(ert-deftest org-sm-core-grade-event-session-identity ()
  (with-temp-buffer
    (org-mode) (insert org-sm-test--tree) (goto-char (point-min))
    (let ((org-sm-after-review-functions nil)
          (org-sm-after-card-change-functions nil))
      (should-not (plist-get (org-sm--commit-action 'topic nil) :session))
      (let ((org-sm--current-buffer (current-buffer))
            (org-sm--current-card-marker (point-marker)))
        (should (plist-get (org-sm--commit-action 'topic nil) :session))
        (outline-next-heading)
        (should-not (plist-get (org-sm--commit-action 'topic nil) :session))
        (set-marker org-sm--current-card-marker nil)))))

(ert-deftest org-sm-core-cloze-preview-and-grade-contract ()
  (with-temp-buffer
    (org-mode)
    (insert "* Memory\n:PROPERTIES:\n:SRS_TYPE: cloze\n:END:\n{{answer}}\n")
    (goto-char (point-min))
    (let* ((before (buffer-string))
           (preview (org-sm-preview-cloze)))
      (should (equal before (buffer-string)))
      (should (= (length preview) 4))
      (let ((event (org-sm-grade-cloze :good preview)))
        (should (eq (plist-get event :action) 'cloze))
        (should (eq (plist-get event :rating) :good))
        (should (numberp (plist-get event :interval-secs)))
        (should (stringp (plist-get event :due)))
        (should (= (plist-get (org-sm-card-at-point) :reviews) 1))))))

(ert-deftest org-sm-core-escaped-heading-example-is-not-a-card ()
  ;; Org requires a comma before heading syntax inside a literal example.
  ;; Both reading paths must ignore it while retaining real child cards.
  (let ((file (make-temp-file "org-sm-escaped-heading-" nil ".org"))
        (text (concat "* Parent\n:PROPERTIES:\n:SRS_TYPE: topic\n:END:\n"
                      "#+begin_src org\n,** Fake\n:PROPERTIES:\n"
                      ":SRS_TYPE: cloze\n:END:\n#+end_src\n"
                      "** Real child\n:PROPERTIES:\n:SRS_TYPE: cloze\n:END:\n")))
    (unwind-protect
        (progn
          (with-temp-file file (insert text))
          (let ((cards (org-sm-cards-in-file file)))
            (should (= (length cards) 2))
            (should (equal (mapcar (lambda (card) (plist-get card :title)) cards)
                           '("Parent" "Real child")))
            (should (= (plist-get (car cards) :child-clozes) 1))
            (with-temp-buffer
              (org-mode)
              (insert text)
              (goto-char (point-min))
              (should (equal (org-sm-card-at-point) (car cards))))))
      (delete-file file))))

;;;; ---- Branch review, progress, priority, provenance ----------------------

(ert-deftest org-sm-core-branch-review-stays-in-the-subtree ()
  (let ((file (make-temp-file "org-sm-branch-" nil ".org"))
        (due "SCHEDULED: <2020-01-01 Wed>\n:PROPERTIES:\n:SRS_TYPE: cloze\n:END:\n")
        queued)
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (insert "* Interview\n** [C] a\n" due "** [C] b\n" due
                  "* Elsewhere\n** [C] c\n" due)
          (cl-letf (((symbol-function 'org-sm--review-markers)
                     (lambda (markers &optional _) (setq queued markers))))
            (goto-char (point-min))
            (org-sm-review-branch)
            (should (equal (sort (mapcar (lambda (m) (org-with-point-at m
                                                       (org-get-heading t t t t)))
                                         queued)
                                 #'string<)
                           '("[C] a" "[C] b")))
            (org-sm-review-branch t)
            (should (= (length queued) 3))))
      (when-let* ((buf (get-file-buffer file)))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-file file))))

(ert-deftest org-sm-core-progress-hides-the-backlog ()
  ;; A light-sized session shows done/total; a full one never shows how much
  ;; is left, since the queue already put the urgent cards first.
  (let ((org-sm-review-light-size 6))
    (let ((org-sm--review-total 6) (org-sm--queue '(a a a a)))
      (should (equal (org-sm--progress-label) "SM 2/6")))
    (let ((org-sm--review-total 292) (org-sm--queue (make-list 290 'a)))
      (should (equal (org-sm--progress-label) "SM 2")))))

(ert-deftest org-sm-core-extract-leaves-import-priority-behind ()
  (with-temp-buffer
    (org-mode)
    (insert "* [#C] Imported\n:PROPERTIES:\n:SRS_TYPE: topic\n:END:\nalpha beta\n")
    (goto-char (point-min))
    (org-sm-extract 'topic "alpha" 0 5)
    (goto-char (point-min))
    (should (eql (plist-get (org-sm-card-at-point) :priority) ?C))
    (re-search-forward "^\\*\\* ")
    (should-not (plist-get (org-sm-card-at-point) :priority)))
  ;; Any other cookie is still inherited.
  (with-temp-buffer
    (org-mode)
    (insert "* [#A] Urgent\n:PROPERTIES:\n:SRS_TYPE: topic\n:END:\nalpha beta\n")
    (goto-char (point-min))
    (org-sm-extract 'topic "alpha" 0 5)
    (re-search-forward "^\\*\\* ")
    (should (eql (plist-get (org-sm-card-at-point) :priority) ?A))))

(ert-deftest org-sm-core-extract-origin-reads-the-parent-around-the-mark ()
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\n:PROPERTIES:\n:SRS_TYPE: topic\n:END:\n"
            "幼年神经元来自海马齿状回。这些年轻神经元更容易兴奋。之后才会整合进环路。\n")
    (goto-char (point-min))
    (let* ((body (org-sm-body-text))
           (from (string-match "这些" body))
           (to (+ from (length "这些年轻神经元更容易兴奋。"))))
      (org-sm-extract 'topic (substring body from to) from to))
    (re-search-forward "^\\*\\* ")
    (let ((origin (org-sm-extract-origin)))
      ;; What "these young neurons" refers to sits before the span.
      (should (equal (car origin) "幼年神经元来自海马齿状回。"))
      (should (equal (cdr origin) "之后才会整合进环路。")))
    ;; A top-level card, or text before any heading, has no origin.
    (goto-char (point-min))
    (should-not (org-sm-extract-origin)))
  (with-temp-buffer (insert "loose text") (should-not (org-sm-extract-origin))))

(ert-deftest org-sm-core-extract-origin-ignores-another-extracts-mark ()
  ;; Workbench children are linked at the end of the parent with no ⌈ of
  ;; their own; the ⌈ before them belongs to a different extract.
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\n:PROPERTIES:\n:SRS_TYPE: topic\n:END:\n"
            "前文 ⌈别的摘录[[id:other][<T>]] 后文 [[id:mine][<T>]]\n"
            "** [T] mine\n:PROPERTIES:\n:ID: mine\n:SRS_TYPE: topic\n:END:\n正文\n")
    (goto-char (point-max))
    (let ((origin (org-sm-extract-origin)))
      (should (equal (car origin) "前文 别的摘录 后文"))
      (should (equal (cdr origin) "")))))

(ert-deftest org-sm-core-workbench-hint-and-commit-extracts-in-place ()
  ;; The workbench header line explains underlines (M-z / M-n / M-k), and
  ;; committing writes the body back then extracts each underline via
  ;; `org-sm-extract', leaving `⌈...[[id:..][<T>]]' in the parent body.
  (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
    (with-temp-buffer
      (org-mode)
      (insert "* [T] Card\n:PROPERTIES:\n:SRS_TYPE: topic\n:END:\nold body 100%\n")
      (goto-char (point-min))
      (let ((card-buf (current-buffer))
            (buf (org-sm-card-workbench)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (should (string-match-p "underlines → child cards" header-line-format))
                (should (string-match-p "M-z mark" header-line-format))
                (should (string-match-p "M-n next" header-line-format))
                (should (string-match-p "M-k drop" header-line-format))
                ;; No overlay keymap: normal/insert single keys are untouched.
                (erase-buffer)
                (insert "引言。\n\n核心机制是缓存子问题的解。\n\n再加一段值得摘的结论。\n")
                (goto-char (point-min))
                (search-forward "核心机制")
                (set-mark (match-beginning 0))
                (search-forward "的解。")
                (activate-mark)
                (org-sm-item-extract)
                (should-not (overlay-get (car (org-sm-pencils)) 'keymap))
                (search-forward "值得摘的结论。")
                (org-sm-pencil-add (match-beginning 0) (match-end 0) "第二条")
                ;; Dismiss the second underline with M-k (`org-sm-pencil-dismiss').
                (goto-char (match-beginning 0))
                (org-sm-pencil-dismiss)
                (should (= (length (org-sm-pencils)) 1))
                (org-sm-capture-commit))
              (with-current-buffer card-buf
                (goto-char (point-min))
                (should (string-match-p
                         "引言。\n\n⌈核心机制是缓存子问题的解。\\[\\[id:[^]]+\\]\\[<T>\\]\\]\n\n再加一段值得摘的结论。"
                         (org-sm-body-text)))
                (should (= (plist-get (org-sm-card-at-point) :child-topics) 1))))
          (when (buffer-live-p buf)
            (with-current-buffer buf (erase-buffer))
            (kill-buffer buf))))))
  (let ((org-sm-capture-file "/tmp/inbox.org"))
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
      (let ((buf (org-sm--capture-buffer "text" "*org-sm hint test*")))
        (unwind-protect
            (with-current-buffer buf
              (should (string-match-p "\\*\\* Title → one card each" header-line-format)))
          (with-current-buffer buf (erase-buffer))
          (kill-buffer buf))))))

;;; org-sm-core-test.el ends here
