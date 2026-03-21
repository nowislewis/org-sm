;;; org-sm-gptel.el --- AI extensions for org-sm using gptel -*- lexical-binding: t -*-

;; Author: Lewis Liu
;; Package-Requires: ((emacs "29.1") (org-sm "1.0") (gptel "0.9.0"))
;; Keywords: org, spaced-repetition, incremental-reading, ai

;;; Commentary:
;;
;; Optional AI extensions for org-sm, powered by `gptel'.
;; Load after both org-sm and gptel are configured:
;;
;;   (with-eval-after-load 'gptel
;;     (require 'org-sm-gptel))
;;
;; Commands:
;;   org-sm-gptel-explain          - Explain current item in a side window.
;;   org-sm-gptel-refine           - Refine current heading body in-place
;;                                   via gptel-rewrite.  Works on any org
;;                                   heading, including org-capture buffers.
;;   org-sm-gptel-generate-concept - Build a concept-map Topic from a subtree.

;;; Code:

(require 'org-sm)
(require 'gptel)

(declare-function gptel-rewrite "gptel-rewrite")
(defvar gptel--rewrite-directive)
(defvar-local gptel--rewrite-message nil)

;;;; ---- Customization -------------------------------------------------------

(defgroup org-sm-gptel nil
  "AI extensions for org-sm."
  :group 'org-sm
  :prefix "org-sm-gptel-")

(defcustom org-sm-gptel-system-explain
  "你是专注于帮助深度理解概念的学习导师。回答极其精炼，直击本质，\
符合 SuperMemo「先理解后记忆」原则。"
  "System prompt for `org-sm-gptel-explain'."
  :type 'string
  :group 'org-sm-gptel)

;; SuperMemo 渐进阅读中，Topic 是"待阅读的长文"，不是最终卡片。
;; 此 prompt 的任务是清洗原始散文，使其适合后续的渐进精炼：
;;   - 去除无信息量的废话，但保留完整段落结构（不压缩成摘要）
;;   - 消除代词歧义，使每段能独立被理解（规则4：最小信息原则的段落级应用）
;;   - 保持可阅读性，而非生成极简知识点——知识点由用户 Extract+Cloze 完成
(defcustom org-sm-gptel-system-topic
  "你是 SuperMemo 渐进阅读专家。你深知渐进阅读分为两个阶段：
阶段一（Topic）：阅读和理解，材料应保持完整段落，适合人类阅读。
阶段二（Cloze）：主动回忆，每张卡片只测一个极小的知识点。

你现在处理的是阶段一的 Topic 材料。你的任务：
1. 剥离无信息量的废话、营销语气和过度修辞，保留核心事实。
2. 将每段限定在一个独立的主题上；删除需要依赖上下文才能理解的代词指代（替换为具体名词）。
3. 保持段落的完整可读性——不要压缩成极简摘要，因为用户需要在阅读中理解材料，然后才能制作卡片。
4. 如果原文包含集合式列举（如「特点有：A、B、C、D」），将其改写为有逻辑因果关系的叙述，而非保留列表。"
  "System prompt for topic operations.
Used by `org-sm-gptel-refine' on topic items and in org-capture buffers."
  :type 'string
  :group 'org-sm-gptel)

;; Cloze 卡片是渐进阅读的终产物，直接用于主动回忆。
;; 此 prompt 严格遵守 SuperMemo 20 条规则的核心：
;;   - 规则4（最小信息原则）：一张卡只测一个知识点，答案越短越好
;;   - 规则5（填空题）：{{}} 周围保留最小防干扰上下文，其余全删
;;   - 规则9/10（避免集合/枚举）：遇到多个知识点必须拆分为多张
;;   - 规则11（防干扰）：上下文必须能精确定位，不与相似概念混淆
;;   - 规则12（优化措辞）：每个词必须有作用，冗余修辞是毒药
(defcustom org-sm-gptel-system-cloze
  "你是 SuperMemo 完形填空专家，严格遵守《有效学习的20条原则》。

完形填空的黄金标准（以死海为例）：
  ✗ 错误：死海位于以色列和约旦边界，海拔-396米，含盐量是海洋的7倍，{{高含盐量}}使游泳者漂浮。
  ✓ 正确：死海的含盐量是海洋的{{7}}倍。（一张卡一个知识点）
  ✓ 正确：死海游泳者能漂浮，因为{{高含盐量}}。（极简上下文）

操作规则：
1. 最小信息原则：一张卡只能测一个 {{}} 填空，答案通常为 1-5 个词。
   如果原文包含多个知识点，必须在回复中生成多张独立卡片（每张一行）。
2. 极简上下文：保留 {{}} 周围能唯一定位答案的最短上下文，删除其余一切。
3. 防干扰：上下文必须足够具体，不能与其他相似概念产生混淆。
   例如：「死海的含盐量」优于「该湖的含盐量」。
4. 禁止集合与枚举：「有三个原因：A、B、C」这类结构必须拆分为三张独立卡片。
5. 优化措辞：每个字都要有意义。删除「众所周知」「值得注意的是」等无信息量的修辞。"
  "System prompt for cloze card operations.
Used by `org-sm-gptel-refine' on cloze items."
  :type 'string
  :group 'org-sm-gptel)

(defcustom org-sm-gptel-system-concept
  "你是 SuperMemo 知识图谱专家。你深知孤立的知识会导致记忆干扰和遗忘。
你的任务是将零散的知识点串联成有意义的全局知识图谱（Concept Map），
指出底层的逻辑链条，并对容易混淆的概念进行「决定性特征」的对比辨析。
输出格式为 Org-mode，直接可用于渐进阅读复习。"
  "System prompt for `org-sm-gptel-generate-concept'."
  :type 'string
  :group 'org-sm-gptel)

;;;; ---- Internal helpers ----------------------------------------------------

(defun org-sm-gptel--explain-stream (prompt)
  "Send PROMPT to gptel and stream the response into a side window.
The buffer uses `org-mode' so Org and gptel syntax highlighting applies."
  (let ((buf (get-buffer-create "*org-sm-explain*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      ;; Enable org-mode for Org syntax highlighting, then gptel-mode
      ;; for streamed-response highlighting (font-lock overlays, etc.).
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (unless gptel-mode
        (gptel-mode 1))
      (let ((inhibit-read-only t))
        (insert (propertize "Explaining...\n\n" 'face 'bold)))
      (local-set-key (kbd "q")
                     (lambda () (interactive) (gptel-abort buf) (quit-window t)))
      (local-set-key (kbd "C-c C-k")
                     (lambda () (interactive) (gptel-abort buf) (quit-window t))))
    (display-buffer buf '(display-buffer-in-side-window
                          (side . right) (window-width . 0.4)))
    (gptel-request prompt
      :system   org-sm-gptel-system-explain
      :buffer   buf
      :stream   t
      :position (with-current-buffer buf (point-max)))))

(defun org-sm-gptel--init-topic ()
  "Set [#A] priority and mark current heading as an org-sm topic."
  (org-priority ?A)
  (org-sm-item-mark 'topic))

;;;; ---- 1. Explain ----------------------------------------------------------

;;;###autoload
(defun org-sm-gptel-explain ()
  "Stream an AI explanation of the current SRS item to a side window."
  (interactive)
  (unless (org-sm-type)
    (user-error "Not on an org-sm SRS heading"))
  (let* ((type    (org-sm-type))
         (content (string-trim
                   (let ((b (org-sm--body-bounds)))
                     (buffer-substring-no-properties (car b) (cdr b)))))
         (prompt
          (pcase type
            ('cloze
             (format "我正在复习以下完形填空卡片，请：
1. 用一句话解释 {{}} 填空背后的核心概念
2. 给一个具体的生活类比帮助记忆
3. 指出最容易与其混淆的概念（若有）并给出「决定性特征」区分

卡片：\n%s" content))
            ('topic
             (format "我正在渐进阅读以下段落，请：
1. 用 2-3 句话点出最值得制成记忆卡片的核心事实
2. 解释专业术语（若有），用通俗语言
3. 若段落违反了最小信息原则（内容过多），指出应如何拆分

段落：\n%s" content))
            (_ (error "Unknown org-sm type: %s" type)))))
    (org-sm-gptel--explain-stream prompt)))

;;;; ---- 2. Refine -----------------------------------------------------------

;;;###autoload
(defun org-sm-gptel-refine ()
  "Refine the current heading body in-place using `gptel-rewrite'.
Works on any org heading, including org-capture buffers before finalization.

Card type determines the system prompt and instruction:
  cloze  → enforce minimum-information principle, one {{}} per card
  topic or unset → clean prose for incremental reading, preserve paragraphs"
  (interactive)
  (require 'gptel-rewrite)
  (when (org-before-first-heading-p)
    (user-error "Not inside any org heading"))
  (let* ((bounds (org-sm--body-bounds))
         (type   (org-sm-type)))
    (goto-char (car bounds))
    (push-mark (cdr bounds) t t)
    (let ((gptel--rewrite-directive
           (if (eq type 'cloze)
               org-sm-gptel-system-cloze
             org-sm-gptel-system-topic))
          (gptel--rewrite-message
           (if (eq type 'cloze)
               "检查是否违反最小信息原则：若含多个知识点请拆分为多张卡片；\
保留 {{}} 关键词；上下文精简到唯一定位答案所需的最短长度。"
             "去除修辞废话，消除代词歧义（替换为具体名词），\
保持段落完整可读性——不要压缩成摘要，这是 Topic 阶段材料。")))
      (call-interactively #'gptel-rewrite))))

;;;; ---- 3. Generate Concept Map ---------------------------------------------

;;;###autoload
(defun org-sm-gptel-generate-concept ()
  "Build a concept-map Topic from the current subtree or region.
If called on a leaf node (e.g. a single cloze card) without a region,
it automatically moves up to the parent heading to capture the entire
concept group (sibling cards) for a broader context.

Inserts a new [#A] Topic immediately after the subtree and marks it for SRS."
  (interactive)
  (let* ((region-p (use-region-p))
         (bounds
          (if region-p
              (cons (region-beginning) (region-end))
            (save-excursion
              (org-back-to-heading t)
              ;; If on a leaf node (no children), go up one level
              ;; to capture the whole concept group context.
              (unless (save-excursion (org-goto-first-child))
                (org-up-heading-safe))
              (cons (point)
                    (save-excursion (org-end-of-subtree t t) (point))))))
         (beg      (car bounds))
         (end      (cdr bounds))
         (end-mark (copy-marker end t))
         (content  (buffer-substring-no-properties beg end))
         (src-buf  (current-buffer))
         (prompt   (format "以下是我间隔重复系统中的一组相关知识卡片。
请生成一份「概念关联与辨析图谱」，结构清晰，本身适合作为 SuperMemo Topic 长期复习。

输出三个部分（直接输出 Org 格式，不加代码块）：
1. *核心逻辑线*：一段话串联这些知识点的因果关系或发展脉络
2. *易混淆点辨析*：列出容易混淆的概念对，用「决定性特征」区分（可用表格）
3. *知识盲区*：指出还缺少哪些前提概念才能真正融会贯通

知识卡片：\n%s" content)))
    (when region-p (deactivate-mark))
    (message "org-sm-gptel: Generating concept map...")
    (gptel-request prompt
      :system org-sm-gptel-system-concept
      :callback
      (lambda (response info)
        (if (not response)
            (message "org-sm-gptel: Concept map failed — %s"
                     (plist-get info :status))
          (with-current-buffer src-buf
            (save-excursion
              (goto-char end-mark)
              (unless (bolp) (insert "\n"))
              (let* ((level  (1+ (save-excursion
                                   (goto-char beg)
                                   (or (org-current-level) 0))))
                     (stars  (make-string level ?*))
                     (ins-pt (point-marker)))
                (insert "\n" stars " " org-sm-topic-prefix "🧠 Concept Map\n"
                        (string-trim response) "\n")
                (goto-char ins-pt)
                (org-back-to-heading t)
                (org-sm-gptel--init-topic))))
          (set-marker end-mark nil)
          (message "org-sm-gptel: Concept map inserted and added to SRS."))))))

(provide 'org-sm-gptel)
;;; org-sm-gptel.el ends here
