;;; org-sm-web-test.el --- Regression tests for mobile writes -*- lexical-binding: t; -*-

(require 'ert)
;; Load sources explicitly: local .elc files can lag during development.
(let ((root (file-name-directory (or load-file-name buffer-file-name))))
  (load-file (expand-file-name "../org-sm.el" root))
  (load-file (expand-file-name "../org-sm-gptel.el" root))
  (load-file (expand-file-name "../org-sm-web.el" root)))

(defmacro org-sm-web-test--with-card (&rest body)
  "Run BODY on an in-memory scheduled topic without touching real notes."
  (declare (indent 0))
  `(with-temp-buffer
     (org-mode)
     (insert "* [T] Example\nSCHEDULED: <2026-09-24 Thu>\n:PROPERTIES:\n:SRS_TYPE: topic\n:END:\n😀alpha beta\n")
     (goto-char (point-min))
     ,@body))

(ert-deftest org-sm-web-test-postpone-parses-days ()
  (org-sm-web-test--with-card
    (let ((event (org-sm-web--apply-review nil "postpone" "7")))
      (should (= (plist-get event :postpone-days) 7))
      (should (plist-get event :new-days)))
    (should-error (org-sm-web--apply-review nil "postpone" "7garbage")
                  :type 'user-error)
    (should-error (org-sm-web--apply-review nil "postpone" "0")
                  :type 'user-error)))

(ert-deftest org-sm-web-test-revision-detects-text-title-and-schedule ()
  (org-sm-web-test--with-card
    (let ((original (org-sm-web--revision)))
      (org-sm-web--check-revision original)
      (org-sm-set-body "new body")
      (should-error (org-sm-web--check-revision original)
                    :type 'org-sm-web-conflict))
    (let ((original (org-sm-web--revision)))
      (org-edit-headline "Renamed")
      (should-error (org-sm-web--check-revision original)
                    :type 'org-sm-web-conflict))
    (goto-char (point-min))
    (let ((original (org-sm-web--revision)))
      (org-sm-web--apply-review nil "reschedule" nil)
      (should-error (org-sm-web--check-revision original)
                    :type 'org-sm-web-conflict))))

(ert-deftest org-sm-web-test-existing-draft-is-not-erased ()
  (let ((name "*org-sm test draft*"))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create name) (insert "unsaved draft"))
          (should-error (org-sm--capture-buffer "replacement" name)
                        :type 'user-error)
          (with-current-buffer name
            (should (equal (buffer-string) "unsaved draft"))))
      (when (get-buffer name) (kill-buffer name)))))

(ert-deftest org-sm-web-test-git-commit-does-not-include-other-staged-paths ()
  (let* ((root (make-temp-file "org-sm-git-test-" t))
         (notes (expand-file-name "notes" root))
         (org-sm-directory notes))
    (unwind-protect
        (progn
          (make-directory notes)
          (org-sm-web--git-run root "init" "-q")
          (org-sm-web--git-run root "config" "user.name" "Test")
          (org-sm-web--git-run root "config" "user.email" "test@example.org")
          (with-temp-file (expand-file-name "card.org" notes) (insert "before\n"))
          (with-temp-file (expand-file-name "other.txt" root) (insert "before\n"))
          (org-sm-web--git-run root "add" "-A")
          (org-sm-web--git-run root "commit" "-qm" "initial")
          (with-temp-file (expand-file-name "card.org" notes) (insert "after\n"))
          (with-temp-file (expand-file-name "new.org" notes) (insert "new card\n"))
          (with-temp-file (expand-file-name "other.txt" root) (insert "after\n"))
          (org-sm-web--git-run root "add" "other.txt")
          (should (string-match-p "card.org" (plist-get (org-sm-web--git-diff) :diff)))
          ;; No remote: push must report failure, but the commit already exists.
          (should-error (org-sm-web--git-push) :type 'error)
          (let ((files (org-sm-web--git-run root "show" "--pretty=format:" "--name-only" "HEAD")))
            (should (equal files "notes/card.org\nnotes/new.org")))
          (should (equal "other.txt"
                         (org-sm-web--git-run root "diff" "--cached" "--name-only"))))
      (delete-directory root t))))

;;;; ---- Browser reading: read point, images, priority ----------------------

(ert-deftest org-sm-web-test-readpoint-roundtrip ()
  (org-sm-web-test--with-card
    (org-entry-put nil "ID" "card-rp")
    (let* ((before (org-sm-body-text))
           (offset (string-search "beta" before)))
      ;; No anchor yet: the snapshot must say so rather than guess.
      (should-not (org-sm-web--readpoint-offset))
      (org-sm-web--set-readpoint offset)
      ;; Offset is measured on the cleaned body the browser was given.
      (should (= (org-sm-web--readpoint-offset) offset))
      (should (string-match-p "\\[\\[rp:" (org-sm--readpoint-anchor (org-id-get))))
      ;; Moving the anchor must not leave the old one behind.
      (org-sm-web--set-readpoint 0)
      (should (= (org-sm-web--readpoint-offset) 0))
      (should (= (count-matches "\\[\\[rp:" (point-min) (point-max)) 1))
      ;; The anchor stays in the body (the browser hides it at render time
      ;; and reports its offset), so the two views keep one text.
      (should (string-match-p "\\[\\[rp:" (org-sm-body-text))))))

(ert-deftest org-sm-web-test-image-path-is-confined ()
  (let* ((root (make-temp-file "org-sm-img-" t))
         (notes (expand-file-name "notes" root))
         (shared (expand-file-name "pics" root))
         (org-sm-directory notes)
         (org-sm-web-image-roots (list shared))
         (in (expand-file-name "a.png" notes))
         (out (expand-file-name "b.png" root))
         (allowed (expand-file-name "c.png" shared)))
    (make-directory notes) (make-directory shared)
    (unwind-protect
        (progn
          (dolist (f (list in out allowed))
            (with-temp-file f (insert "x")))
          (should (equal (org-sm-web--image-path in) in))
          (should (equal (org-sm-web--image-path allowed) allowed))
          ;; Outside every root, or not an image: refused.
          (should-error (org-sm-web--image-path out) :type 'user-error)
          (should-error (org-sm-web--image-path "/etc/passwd") :type 'user-error)
          (with-temp-file (expand-file-name "notes.txt" notes) (insert "x"))
          (should-error (org-sm-web--image-path (expand-file-name "notes.txt" notes))
                        :type 'user-error))
      (delete-directory root t))))

(ert-deftest org-sm-web-test-card-snapshot-carries-reading-state ()
  (let* ((dir (make-temp-file "org-sm-snap-" t))
         (org-sm-directory dir)
         (org-sm-web-image-roots nil)
         (file (expand-file-name "n.org" dir))
         (img (expand-file-name "pic.png" dir)))
    (unwind-protect
        (progn
          (with-temp-file img (insert "png"))
          (with-temp-file file
            (insert "* [T] Read me\n:PROPERTIES:\n:ID: card1\n:SRS_TYPE: topic\n"
                    ":END:\n首段\n[[file:pic.png][图示]]\nsecond\n"))
          (with-current-buffer (find-file-noselect file)
            (goto-char (point-min))
            (org-sm-web--set-readpoint 0)
            (let ((card (org-sm-web--card-at-point)))
              (should (= (plist-get card :readpoint) 0))
              (let ((image (aref (plist-get card :images) 0)))
                (should (= (length (plist-get card :images)) 1))
                (should (equal (plist-get image :alt) "图示"))
                (should (string-match-p "\\`/api/image\\?path=" (plist-get image :url)))))
            (set-buffer-modified-p nil))
        (when-let* ((b (get-file-buffer file))) (kill-buffer b))
        (delete-directory dir t)))))

(ert-deftest org-sm-web-test-priority-endpoint ()
  (org-sm-web-test--with-card
    (should-not (plist-get (org-sm-card-at-point) :priority))
    (let ((letter (aref "A" 0)))
      (org-priority letter))
    (should (eq (plist-get (org-sm-card-at-point) :priority) ?A))
    (should-error (org-priority (aref "Z" 0)) :type 'error)))

(ert-deftest org-sm-web-test-readpoint-moves-forward-past-itself ()
  ;; The browser's offset counts the old anchor; moving forward must still
  ;; land on the tapped character, not one anchor-length too far.
  (org-sm-web-test--with-card
    (org-entry-put nil "ID" "card-fw")
    (org-sm-web--set-readpoint 0)
    (let* ((body (org-sm-body-text))
           (target (string-search "beta" body)))
      (org-sm-web--set-readpoint target)
      (should (string-match-p "alpha \\[\\[rp:card-fw\\]\\[📖\\]\\]beta"
                              (org-sm-body-text))))))

;;;; ---- Browser AI: suggest, rewrite, explain ------------------------------

(ert-deftest org-sm-web-test-ai-suggest-returns-cleaned-body-offsets ()
  (with-temp-buffer
    (org-mode)
    (insert "* [T] Suggest\n:PROPERTIES:\n:ID: s1\n:SRS_TYPE: topic\n:END:\n\n"
            "⌈已摘录首句。[[id:c1][<T>]]\n\n缓存子问题的解以消除重复计算。\n")
    (goto-char (point-min))
    (let ((body (org-sm-body-text))
          picks)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_text &rest args)
                   (funcall (plist-get args :callback)
                            (json-encode
                             '(:picks [(:quote "⌈已摘录首句。[[id:c1][<T>]]" :why "跳过")
                                       (:quote "缓存子问题的解以消除重复计算。" :why "核心机制")
                                       (:quote "不存在的句子" :why "幻觉")]))
                            nil))))
        (org-sm-gptel-request-suggest body (lambda (p _err) (setq picks p))))
      (let ((pick (car picks)))
        (should (= (length picks) 1))
        (should (equal (substring body (plist-get pick :start) (plist-get pick :end))
                       "缓存子问题的解以消除重复计算。"))
        (should (equal (plist-get pick :why) "核心机制"))))))

(ert-deftest org-sm-web-test-ai-rewrite-region-splices-and-shifts-picks ()
  (org-sm-web-test--with-card
    (let* ((draft "第一段保持不变。\n\n第二段需要改写。\n\n第三段保持不变。")
           (s     (string-search "第二段" draft))
           (e     (+ s (length "第二段需要改写。")))
           sent out-body out-picks)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (text &rest args)
                   (setq sent text)
                   (funcall (plist-get args :callback)
                            (json-encode
                             '(:body "改写后的第二段，含关键机制。"
                               :picks [(:quote "改写后的第二段，含关键机制。" :why "关键因果")]))
                            nil))))
        (org-sm-gptel-request-rewrite
         draft "refine" nil s e
         (lambda (b p _err) (setq out-body b out-picks p))))
      (should (string-prefix-p "第二段需要改写。\n\n【出处上下文" sent))
      (should (string-match-p "第一段保持不变。" sent))
      (should (equal out-body
                     "第一段保持不变。\n\n改写后的第二段，含关键机制。\n\n第三段保持不变。"))
      (let ((pick (car out-picks)))
        (should (equal (substring out-body
                                  (plist-get pick :start)
                                  (plist-get pick :end))
                       "改写后的第二段，含关键机制。"))))))

(ert-deftest org-sm-web-test-ai-explain-bridges-emacs-chat-and-strips-tools ()
  (with-temp-buffer
    (org-mode)
    (insert "* [C] Cloze Card\n:PROPERTIES:\n:ID: c-exp\n:SRS_TYPE: cloze\n:END:\n"
            "前缀条件导致{{核心结论}}。\n")
    (goto-char (point-min))
    (let* ((span (org-sm-web--body-span))
           (chat (get-buffer-create "*org-sm-explain: C Cloze Card*"))
           passage1 reply1 reply2)
      (unwind-protect
          (let ((fail-first t))
            (cl-letf (((symbol-function 'gptel) (lambda (_name) chat))
                      ((symbol-function 'gptel-send)
                       (lambda ()
                         (let ((beg (point)))
                           (if fail-first
                               ;; Simulate network failure (empty response): the
                               ;; inserted prompt must be rolled back so retry works.
                               (progn
                                 (setq fail-first nil)
                                 (run-hook-with-args 'gptel-post-response-functions beg beg))
                             (insert
                              (propertize
                               (if (string-search "去掉「前缀条件」" (buffer-string))
                                   "\n回答正确，因为前缀锁死了范围。"
                                 (concat "\n#+begin_tool (Skill :skill \"book\")\nresult\n#+end_tool\n"
                                         "① 定义...\n哪个词去掉了，这句话就不成立？"))
                               'gptel 'response))
                             (run-hook-with-args 'gptel-post-response-functions beg (point)))))))
              (let (err0)
                (org-sm-gptel-explain-card
                 (car span) (cdr span) nil
                 (lambda (_p _r err) (setq err0 err)))
                (should err0))
              (org-sm-gptel-explain-card
               (car span) (cdr span) nil
               (lambda (p r _err) (setq passage1 p reply1 r)))
              (org-sm-gptel-explain-card
               nil nil "去掉「前缀条件」就不成立"
               (lambda (_p r _err) (setq reply2 r)))))
        (when (buffer-live-p chat) (kill-buffer chat)))
      (should (equal passage1 "前缀条件导致{{核心结论}}。"))
      (should-not (string-match-p "begin_tool" reply1))
      (should (string-match-p "哪个词去掉了" reply1))
      (should (string-match-p "回答正确" reply2)))))

(provide 'org-sm-web-test)
;;; org-sm-web-test.el ends here
