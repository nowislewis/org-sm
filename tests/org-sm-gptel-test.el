;;; org-sm-gptel-test.el --- AI actions: explain and workbench rewrite -*- lexical-binding: t; -*-

(require 'ert)
(let ((root (file-name-directory (or load-file-name buffer-file-name))))
  (load-file (expand-file-name "../org-sm.el" root))
  (load-file (expand-file-name "../org-sm-gptel.el" root)))

(defmacro org-sm-gptel-test--with-card (body &rest forms)
  "Run FORMS in a file-visiting topic card whose body is BODY."
  (declare (indent 1))
  `(let ((file (make-temp-file "org-sm-gptel-" nil ".org")))
     (unwind-protect
         (with-current-buffer (find-file-noselect file)
           (insert "* [T] Article\n:PROPERTIES:\n:ID: art\n:SRS_TYPE: topic\n:END:\n"
                   ,body "\n")
           (goto-char (point-min))
           (re-search-forward "^:END:\n")
           ,@forms)
       (when-let* ((buf (get-file-buffer file)))
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf))
       (delete-file file))))

;;;; ---- Inline underline picks: the AI quotes, the reader extracts ----------

(ert-deftest org-sm-gptel-test-locate-is-exact-then-loose ()
  (with-temp-buffer
    (insert "First line about caching.\nThe cache stores results\nof expensive calls.\nEnd.")
    ;; Exact quote.
    (should (equal (buffer-substring (car (org-sm-gptel--locate "caching" 1 (point-max)))
                                     (cdr (org-sm-gptel--locate "caching" 1 (point-max))))
                   "caching"))
    ;; Re-wrapped by the model: whitespace runs match loosely.
    (let ((span (org-sm-gptel--locate "The cache stores results of expensive calls."
                                      1 (point-max))))
      (should span)
      (should (string-prefix-p "The cache" (buffer-substring (car span) (cdr span)))))
    ;; Hallucinated: not in the text, so no span at all.
    (should-not (org-sm-gptel--locate "A sentence that never appears." 1 (point-max)))))

(ert-deftest org-sm-gptel-test-draw-picks-keeps-only-unextracted-quotes ()
  (org-sm-gptel-test--with-card
      "⌈已摘过的第一句。[[id:old][<T>]]\n\n缓存子问题的解，避免重复计算。\n\n例子从略。"
    (let ((beg (point)) (end (point-max)))
      (should (= (org-sm-gptel--draw-picks
                  '(("⌈已摘过的第一句。[[id:old][<T>]]" . "已摘录应跳过")
                    ("缓存子问题的解，避免重复计算。" . "核心机制")
                    ("原文里没有这句话。" . "幻觉"))
                  beg end)
                 1))
      (let ((ov (car (org-sm-pencils))))
        (should (equal (buffer-substring (overlay-start ov) (overlay-end ov))
                       "缓存子问题的解，避免重复计算。"))
        (should (equal (overlay-get ov 'org-sm-pencil) "核心机制")))
      ;; Underlines are display only: the text is untouched.
      (should-not (string-match-p "核心机制" (buffer-string))))))

;;;; ---- Explain: the paragraph, with its neighbourhood ----------------------

(ert-deftest org-sm-gptel-test-explain-sends-paragraph-and-context ()
  (org-sm-gptel-test--with-card
      "前文铺垫。\n\n难懂的这一段。\n\n后文延伸。"
    (org-entry-put nil "SOURCE" "[[page:/books/x.pdf::7][《x》p.7]]")
    (search-forward "难懂")
    (let (sent)
      (cl-letf (((symbol-function 'org-sm-gptel--open-chat)
                 (lambda (_kind _system message) (setq sent message))))
        (org-sm-gptel-explain))
      (should (string-match-p "#\\+begin_quote\n难懂的这一段。\n#\\+end_quote" sent))
      (should (string-match-p "前文铺垫" sent))
      (should (string-match-p "后文延伸" sent))
      (should (string-match-p "【卡片坐标：文件 .* 第 1 行，ID art，出处 \\[\\[page:/books/x\\.pdf::7\\]\\[《x》p\\.7\\]\\]" sent)))))

(ert-deftest org-sm-gptel-test-extracts-carry-their-origin ()
  (org-sm-gptel-test--with-card
      "幼年神经元来自齿状回。这些年轻神经元更容易兴奋。后文。"
    (org-entry-put nil "SOURCE" "[[page:/books/neuro.pdf::12][《神经》p.12]]")
    (let* ((body (org-sm-body-text))
           (from (string-match "这些" body))
           (to (+ from (length "这些年轻神经元更容易兴奋。"))))
      (goto-char (point-min))
      (org-sm-extract 'topic (substring body from to) from to))
    (re-search-forward "^\\*\\* ")
    ;; Even when narrowed to the child (as in a review session), explain still
    ;; resolves both the parent extract origin and the ancestor's `:SOURCE:'.
    (org-narrow-to-subtree)
    (let (sent)
      (cl-letf (((symbol-function 'org-sm-gptel--open-chat)
                 (lambda (_kind _sys msg) (setq sent msg))))
        (org-sm-gptel-explain))
      (should (string-match-p "幼年神经元来自齿状回。\n【……这张卡摘自这里……】\n后文。" sent))
      (should (string-match-p "出处 \\[\\[page:/books/neuro\\.pdf::12\\]\\[《神经》p\\.12\\]\\]" sent)))
    (widen)
    ;; workbench refine: same context, fenced off from the body to rewrite.
    (let ((card (point-marker)) text)
      (with-temp-buffer
        (org-sm-capture-mode)
        (insert "这些年轻神经元更容易兴奋。")
        (setq-local org-sm-capture-parent-marker card)
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "refine"))
                  ((symbol-function 'org-sm-gptel--card-request)
                   (lambda (tx &rest _) (setq text tx))))
          (org-sm-gptel-card-ai)))
      (should (string-prefix-p "这些年轻神经元更容易兴奋。\n\n【出处上下文" text))
      (should (string-match-p "幼年神经元来自齿状回" text)))))

(ert-deftest org-sm-gptel-test-provenance-falls-back-to-file-reference ()
  (let ((file (make-temp-file "org-sm-gptel-" nil ".org")))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (org-mode)
          (insert "#+title: 纳瓦尔宝典\n#+reference: naval_2022\n"
                  "* [T] 顶层章节\n:PROPERTIES:\n:ID: root\n:SRS_TYPE: topic\n:END:\n"
                  "正文段落。\n"
                  "** [C] 子填空\n:PROPERTIES:\n:ID: child\n:SRS_TYPE: cloze\n:END:\n"
                  "人生早期要在{{重大决定}}上花时间。\n")
          (goto-char (point-min))
          (re-search-forward "^\\*\\* ")
          (org-narrow-to-subtree)
          (should (equal (org-sm-gptel--card-provenance) "naval_2022")))
      (when-let* ((b (find-buffer-visiting file))) (kill-buffer b))
      (delete-file file))))


;;;; ---- Workbench rewrite + inline underline picks --------------------------

(ert-deftest org-sm-gptel-test-rewrite-draws-picks-and-commits-in-place ()
  (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
    (org-sm-gptel-test--with-card "粗糙的原文。"
      (goto-char (point-min))
      (let ((card-buf (current-buffer))
            (wb (org-sm-card-workbench)))
        (unwind-protect
            (progn
              (with-current-buffer wb
                (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "refine"))
                          ((symbol-function 'org-sm-gptel--card-request)
                           (lambda (text _type _mode _custom cb)
                             (should (equal text "粗糙的原文。"))
                             (funcall cb
                                      "第一句铺垫。\n\n缓存子问题的解以消除重复计算。\n\n结尾。"
                                      '(("缓存子问题的解以消除重复计算。" . "核心机制")
                                        ("不存在的幻觉引文" . "应丢弃"))
                                      nil))))
                  (org-sm-gptel-card-ai))
                (should (= (length (org-sm-pencils)) 1))
                (org-sm-capture-commit))
              (with-current-buffer card-buf
                (goto-char (point-min))
                (should (string-match-p
                         "第一句铺垫。\n\n⌈缓存子问题的解以消除重复计算。\\[\\[id:[^]]+\\]\\[<T>\\]\\]\n\n结尾。"
                         (org-sm-body-text)))
                (should (= (plist-get (org-sm-card-at-point) :child-topics) 1))))
          (when (buffer-live-p wb)
            (with-current-buffer wb (erase-buffer))
            (kill-buffer wb)))))))

(ert-deftest org-sm-gptel-test-rewrite-region-keeps-outside-text-and-underlines ()
  (with-temp-buffer
    (org-sm-capture-mode)
    (insert "第一段保持不变。\n\n第二段需要改写。\n\n第三段也保持不变。\n")
    (setq-local org-sm-capture-parent-marker (point-marker))
    ;; Manual underline on the first paragraph must survive rewriting the second.
    (goto-char (point-min))
    (org-sm-pencil-add 1 9 "保留的手动标记")
    (search-forward "第二段需要改写。")
    (set-mark (match-beginning 0))
    (goto-char (match-end 0))
    (activate-mark)
    (let ((transient-mark-mode t))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "refine"))
                ((symbol-function 'org-sm-gptel--card-request)
                 (lambda (text _type _mode _custom cb)
                   ;; Target comes first, surrounded by the rest of the workbench
                   ;; as reference-only context so pronouns in the region resolve.
                   (should (string-prefix-p "第二段需要改写。\n\n【出处上下文" text))
                   (should (string-match-p "第一段保持不变。\n【……这张卡摘自这里……】\n第三段也保持不变。" text))
                   (funcall cb "改写后的第二段，含关键机制。"
                            '(("改写后的第二段，含关键机制。" . "新标记"))
                            nil))))
        (org-sm-gptel-card-ai)))
    (should (equal (buffer-string)
                   "第一段保持不变。\n\n改写后的第二段，含关键机制。\n\n第三段也保持不变。\n"))
    (should (equal (mapcar (lambda (ov) (overlay-get ov 'org-sm-pencil))
                           (org-sm-pencils))
                   '("保留的手动标记" "新标记")))
    (set-marker org-sm-capture-parent-marker nil)))

(ert-deftest org-sm-gptel-test-async-response-does-not-erase-manual-edits ()
  (with-temp-buffer
    (org-sm-capture-mode)
    (insert "draft")
    (let (callback)
      (setq-local org-sm-capture-parent-marker (point-marker))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "refine"))
                ((symbol-function 'org-sm-gptel--card-request)
                 (lambda (_text _type _mode _custom cb) (setq callback cb))))
        (org-sm-gptel-card-ai)
        (insert " user edit")
        (funcall callback "AI result" nil nil)
        (should (equal (buffer-string) "draft user edit")))
      (set-marker org-sm-capture-parent-marker nil))))

(ert-deftest org-sm-gptel-test-card-system-uses-picks-not-cards ()
  (should (string-match-p "picks" (org-sm-gptel--card-system 'topic 'refine)))
  (should (string-match-p "picks 固定为空数组" (org-sm-gptel--card-system 'topic 'simplify)))
  (should (string-match-p "picks 固定为空数组" (org-sm-gptel--card-system 'cloze 'refine)))
  (should (string-prefix-p "只做这个" (org-sm-gptel--card-system 'topic 'custom "只做这个")))
  ;; A cloze response that dropped every `{{...}}' is rejected before it can
  ;; overwrite the user's draft in the workbench.
  (let (got-err)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (_text &rest args)
                 (funcall (plist-get args :callback)
                          "{\"body\":\"没有挖空的句子\",\"picks\":[]}" nil))))
      (org-sm-gptel--card-request
       "原句 {{空1}} → {{空2}}" 'cloze 'refine nil
       (lambda (_body _picks err) (setq got-err err))))
    (should (string-match-p "dropped {{cloze}}" got-err)))
  ;; Anthropic's `response_json' tool emulation can fire the callback once with
  ;; preamble text (while `:tool-use' is still pending) before delivering the
  ;; actual JSON, or a model may wrap JSON in markdown fences.
  (let (got-body)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (_text &rest args)
                 (let ((cb (plist-get args :callback)))
                   (funcall cb "好的，改写如下：" '(:tool-use ((:name "response_json"))))
                   (funcall cb "```json\n{\"body\":\"改写后的句子\",\"picks\":[]}\n```" nil)))))
      (org-sm-gptel--card-request
       "原句" 'topic 'refine nil
       (lambda (body _picks _err) (setq got-body body))))
    (should (equal got-body "改写后的句子"))))

(ert-deftest org-sm-gptel-test-style-reaches-read-text-only ()
  ;; Every rewrite mode and the explanation carry the writing rules ...
  (dolist (type '(topic cloze))
    (dolist (mode '(refine simplify))
      (should (string-search org-sm-gptel-style (org-sm-gptel--card-system type mode)))))
  (should (string-search org-sm-gptel-style
                         (org-sm-gptel--card-system 'topic 'custom "改成英文")))
  (org-sm-gptel-test--with-card "一段。"
    (let (system)
      (cl-letf (((symbol-function 'org-sm-gptel--open-chat)
                 (lambda (_kind sys _msg) (setq system sys))))
        (org-sm-gptel-explain))
      (should (string-search org-sm-gptel-style system))))
  ;; The explain prompt no longer sends the model after a retired skill.
  (should-not (string-match-p "learning-coach" org-sm-gptel-system-explain)))

;;;; ---- Review event formatting (shared with org-ladder) --------------------

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
      (should (= (plist-get card :child-clozes) 1))
      (let ((org-sm-after-review-functions (list (lambda (ev) (push ev events)))))
        (let ((ev1 (org-sm--commit-action 'cloze card
                                          :rating :good :old-days 14 :new-days 21))
              (ev2 (org-sm--commit-action 'dismiss card :old-days 5)))
          (should (equal (org-sm-default-feedback ev1) "cloze :good · 14d → 21d"))
          (should (equal (org-sm-default-feedback ev2) "dismissed"))
          (should (= (length events) 2)))))))

(provide 'org-sm-gptel-test)
;;; org-sm-gptel-test.el ends here
