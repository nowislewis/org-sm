;;; org-sm-gptel-test.el --- Workbench async safety tests -*- lexical-binding: t; -*-

(require 'ert)
(load-file (expand-file-name "../org-sm.el"
                             (file-name-directory (or load-file-name buffer-file-name))))
(load-file (expand-file-name "../org-sm-gptel.el"
                             (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest org-sm-gptel-test-async-response-does-not-erase-manual-edits ()
  (with-temp-buffer
    (org-sm-capture-mode)
    (insert "draft")
    (let (callback)
      (unwind-protect
          (progn
            ;; A live marker is sufficient to identify a workbench buffer.
            (setq-local org-sm-capture-parent-marker (point-marker))
            (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "refine"))
                      ((symbol-function 'org-sm-gptel--card-request)
                       (lambda (_text _type _mode _custom cb) (setq callback cb))))
              (org-sm-gptel-card-ai)
              (insert " user edit")
              (funcall callback "AI result" nil nil)
              (should (equal (buffer-string) "draft user edit"))))
        (set-marker org-sm-capture-parent-marker nil)))))

(ert-deftest org-sm-gptel-test-pick-system-prompt-has-own-contract ()
  ;; pick's BODY is a reason, so it must not get the refine contract appended.
  (let ((s (org-sm-gptel--card-system 'topic 'pick)))
    (should (equal s org-sm-gptel-system-pick))
    (should-not (string-match-p "优化后的正文" s))))

(ert-deftest org-sm-gptel-test-pick-renders-one-card-and-no-body ()
  ;; Committing a pick must never rewrite the source body: preamble stays empty,
  ;; the reason lives only in the header line, and only one card is kept.
  (with-temp-buffer
    (org-sm-capture-mode)
    (org-sm-gptel--render-pick "核心主张"
                               '(("A 导致 B" . "因为 A，所以 B。")
                                 ("多余" . "不应出现")))
    (should (string-match-p "核心主张" header-line-format))
    (should-not (string-match-p "核心主张" (buffer-string)))
    (should-not (string-match-p "多余" (buffer-string)))
    (should (string-blank-p (buffer-substring-no-properties
                             (car (org-sm-capture-preamble-bounds))
                             (cdr (org-sm-capture-preamble-bounds)))))
    (should (= 1 (length (cdr (org-sm--capture-split-buffer)))))))

(ert-deftest org-sm-gptel-test-pick-empty-result ()
  (with-temp-buffer
    (org-sm-capture-mode)
    (org-sm-gptel--render-pick "已经足够短，可以直接推后" nil)
    (should (string-blank-p (buffer-string)))
    (should (null (cdr (org-sm--capture-split-buffer))))))

(ert-deftest org-sm-test-card-at-point-and-actions ()
  (with-temp-buffer
    (org-mode)
    (insert "* [T] Parent Topic\n"
            ":PROPERTIES:\n:SRS_TYPE: topic\n:END:\n"
            ":LOGBOOK:\n"
            "- topic  a=1.5  10d  [2026-10-01 Thu 10:00]\n"
            "- topic  a=1.5  15d  [2026-10-10 Sat 10:00]\n"
            ":END:\n"
            "** [C] Child Cloze\n"
            ":PROPERTIES:\n:SRS_TYPE: cloze\n:END:\n")
    (goto-char (point-min))
    (let ((card (org-sm-card-at-point))
          (events nil))
      (should (eq (plist-get card :type) 'topic))
      (should (= (plist-get card :reviews) 2))
      (should (= (plist-get card :child-topics) 0))
      (should (= (plist-get card :child-clozes) 1))
      (let ((org-sm-after-review-functions (list (lambda (ev) (push ev events)))))
        (let ((ev1 (org-sm--commit-action 'cloze card
                                          :rating :good :old-days 14 :new-days 21))
              (ev2 (org-sm--commit-action 'dismiss card :old-days 5)))
          (should (equal (org-sm-default-feedback ev1)
                         "cloze :good · 14d → 21d"))
          (should (equal (org-sm-default-feedback ev2) "dismissed"))
          (should (= (length events) 2))
          (should (equal (org-sm-default-finish "dismissed" nil)
                         "org-sm: done 󱁖  (last: dismissed)"))
          (should (equal (org-sm-default-finish nil t)
                         "org-sm: review aborted")))))))

(provide 'org-sm-gptel-test)
;;; org-sm-gptel-test.el ends here
