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
    (let* ((old (org-sm-card-at-point))
           (event (org-sm-grade-postpone 7)))
      (should (= (plist-get event :postpone-days) 7))
      (should (= (plist-get event :new-days) (plist-get old :interval-days)))
      (should (= (plist-get (org-sm-card-at-point) :reviews) 1))
      (should-error (org-sm-grade-postpone -1)))))

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
    (let (actions)
      (let ((org-sm-after-card-change-functions
             (list (lambda (event) (push (plist-get event :action) actions)))))
        (org-sm-set-body "alpha beta\n")
        (goto-char (point-min))
        (org-sm-set-title "Renamed")
        (goto-char (point-min))
        (should (stringp (org-sm-extract 'topic "alpha" 0 5)))
        (should (equal actions '(extract edit edit)))))))

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

;;; org-sm-core-test.el ends here
