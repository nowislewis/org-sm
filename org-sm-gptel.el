;;; org-sm-gptel.el --- AI extensions for org-sm using gptel -*- lexical-binding: t -*-

;; Author: Lewis Liu
;; Package-Requires: ((emacs "29.1") (org-sm "1.0") (gptel "0.9.0"))
;; Keywords: org, spaced-repetition, incremental-reading, ai

;;; Commentary:
;;
;; Optional AI extensions for org-sm, powered by `gptel'.
;;
;;   (with-eval-after-load 'gptel (require 'org-sm-gptel))
;;
;; Two commands, both serving one goal: understanding the knowledge itself.
;;
;; Commands:
;;   org-sm-gptel-explain  - AI explains TO you (use when stuck during review).
;;                           Why-before-what: explains the design motive / causal
;;                           chain first, then what it is; ends by asking which word
;;                           the sentence can't live without.  Opens a persistent
;;                           side-window chat (current subtree quoted, org file in
;;                           context); buffer *org-sm-explain: <heading>* reused.
;;   org-sm-gptel-refine   - Refine heading body in-place via gptel-rewrite.
;;                           cloze: checks direction first (why-type vs what-type),
;;                           then enforces the minimum-information principle.
;;                           topic: cleans prose, never deletes causal links.

;;; Code:

(require 'org-sm)
(require 'gptel)

(declare-function gptel-rewrite "gptel-rewrite")
(declare-function gptel-context-add-file "gptel-context")
(declare-function gptel--apply-preset "gptel")
(declare-function gptel-get-preset "gptel")
(declare-function gptel-agent-update "gptel-agent")
(defvar gptel--rewrite-directive)
(defvar-local gptel--rewrite-message nil)
(defvar gptel-context)

;;;; ---- Customization -------------------------------------------------------

(defgroup org-sm-gptel nil
  "AI extensions for org-sm."
  :group 'org-sm
  :prefix "org-sm-gptel-")

(defcustom org-sm-gptel-agent-preset 'gptel-agent
  "gptel preset applied buffer-locally to `org-sm-gptel-explain' chats.

Gives the explain chat the preset's agent tools — notably `Skill', so the
AI can read book skills under `~/.agents/skills' and `learning-coach'.
nil = plain `gptel' (no tools); any registered preset symbol also works."
  :type '(choice (const :tag "No preset (plain gptel)" nil)
                 (symbol :tag "Preset name"))
  :group 'org-sm-gptel)

(defcustom org-sm-gptel-system-explain
  "你是专注于帮助深度理解概念的学习导师。

你的角色：解释者。用户在渐进阅读复习中遇到了看不懂的内容，你负责帮他建立理解。

【输出结构】每次按三层，每层 1-2 句，重心在②③，①一句带过、不凑字：
  ① 是什么（一句话定义，需要时配一个类比）—— 只是地基锚点，别展开。
  ② 为什么会这样—— 原文从 A 跳到 C 时，把被省略的因果中间步骤 B 补出来。
  ③ 为什么它重要（★最高价值，必须有）—— 解决什么问题？不要会怎样？在更大体系里的位置？
     这一层把孤立知识「挂回主线」，绝不能略。先抛定义并停在定义，只会让人记住一个易忘、不可迁移的事实。

【工具（若具备 Skill/Read）】
- 解释前先 Skill 读 「learning-coach」拿学习偏好（method.md）并遵守。
- 遇领域知识就 Skill 读对应书 skill（ai-processor-architecture 等）查证，不凭空猜。
- 只读不写：可 Skill/Read/Grep 搜知识，绝不 Edit/Write 改文件。制卡是另一个流程。

约束：
- 只解释，不制卡，不生成填空/卡片格式/总结。
- 某层不适用时（纯约定/纯事实无②③），明说「这是纯约定，没有为什么」，不编造因果。
- 需类比时给 2-3 个不同领域（日常/工程/自然）供用户选；有层次/流程/对比关系时用 ASCII 图或 org 表格。
- 解释完问一句：「哪个词去掉了这句话就不成立？」等用户自答，不提示。
  用户答后一句话确认/指正是否抓住核心，不展开，制卡留给用户。"
  "System prompt for `org-sm-gptel-explain'."
  :type 'string :group 'org-sm-gptel)

(defcustom org-sm-gptel-system-topic
  "你是 SuperMemo 渐进阅读专家，处理阶段一（Topic）材料。任务：
1. 剥离废话、营销语气和过度修辞，保留核心事实。
2. 每段限定一个独立主题；消除代词歧义（替换为具体名词）。
3. 保持段落完整可读性——不要压缩成摘要，用户需要在阅读中理解后才制卡。
4. 集合式列举（「特点有：A、B、C」）改写为有因果逻辑的叙述。
5. 严禁删除因果链。原文中的「因为」「导致」「所以」「才能」等连接词及其前后内容必须完整保留。
   这类材料的核心价值正是「为什么」，删掉因果链等于把活知识变成死记忆。
6. 【显式化隐含逻辑】原文里藏着、但没明说的因果关系，把它显式写出来：
   补上被省略的主语、补上「因为…所以…」这类连接词、把「特点是A、B」改写成「A 导致 B」。
   目的：让用户重读时逻辑链是显式、连贯的，不用在脑子里重建。
   【硬约束】只能显式化原文已有、但隐含的逻辑；绝不引入原文没有的新解释、新事实、新推导。
   （那是 explain 的活；refine 只负责把原文藏的逻辑摔到表面，不替用户思考。）
7. 如果原文只有结论没有说明原因（且原文里也推不出来），在段末加注：「【需补充】为什么是这个结论？」
   提示用户主动去寻找原因，而不是直接给出。"
  "System prompt for topic refine operations."
  :type 'string :group 'org-sm-gptel)

(defcustom org-sm-gptel-system-cloze
  "你是 SuperMemo 完形填空专家，严格遵守最小信息原则。

黄金标准（以死海为例）：
  ✗ 死海位于以色列和约旦边界，{{高含盐量}}使游泳者漂浮。（多个知识点）
  ✓ 死海的含盐量是海洋的{{7}}倍。
  ✓ 死海游泳者能漂浮，因为{{高含盐量}}。

规则：
1. 一张卡只测一个 {{}}，答案 1-5 词；多个知识点须拆成多张卡。
2. 保留 {{}} 周围能唯一定位答案的最短上下文，其余删除。
3. 上下文须足够具体，不与相似概念混淆。
4. 禁止枚举：「三个原因：A、B、C」拆成三张。
5. 删除「众所周知」等无信息量修辞。

「为什么」型卡片规则（框架性认知类内容优先使用）：
6. 当原文包含因果关系时，优先制「为什么」型卡，而非「是什么」型卡。
   ✗ 是什么型：差序格局是指{{以自我为中心向外扩散的关系结构}}。（记定义，易忘）
   ✓ 为什么型：差序格局导致领导偏向「自己人」，因为{{距离中心越近义务越大、越被信任}}。（记因果，可推导）
7. 「为什么」型卡的 {{}} 填的是机制/原因/结果，而非名称或定义。
8. 禁止代劳：不要在卡片中直接给出完整推导过程；只保留能触发用户自己推导的最小线索。
   目标是让用户在回忆时自己完成推导，而不是复现你写的句子。"
  "System prompt for cloze refine operations."
  :type 'string :group 'org-sm-gptel)

;;;; ---- Internal helpers ----------------------------------------------------

(defun org-sm-gptel--buf-name (kind heading)
  "Return a stable chat buffer name for KIND (string) and HEADING string."
  (format "*org-sm-%s: %s*"
          kind
          (truncate-string-to-width
           (replace-regexp-in-string "[^[:alnum:][:blank:]-]" "" heading)
           40 nil nil t)))

(defun org-sm-gptel--current-subtree ()
  "Return heading line and body text, excluding metadata and child headings."
  (save-excursion
    (org-back-to-heading t)
    (let ((heading (org-get-heading t t t t))
          (bounds  (org-sm--body-bounds)))
      (concat heading "\n"
              (string-trim (buffer-substring-no-properties
                            (car bounds) (cdr bounds)))))))

(defun org-sm-gptel--open-chat (kind system intro)
  "Open or reuse a persistent gptel chat buffer for KIND on the current heading.

KIND   - string (\"explain\"), used in the buffer name.
SYSTEM - system prompt defining the AI role.
INTRO  - one-line task description; prepended to the opening message.

Opening message (sent automatically on first open):
  <intro>

  当前 heading：<heading>

  #+begin_quote
  <subtree>
  #+end_quote

Subsequent calls redisplay the existing buffer without sending again."
  (let* ((heading  (save-excursion
                     (org-back-to-heading t)
                     (org-get-heading t t t t)))
         (subtree  (org-sm-gptel--current-subtree))
         (buf-name (org-sm-gptel--buf-name kind heading))
         (new-p    (not (get-buffer buf-name)))
         (chat-buf (gptel buf-name))
         (src-file (buffer-file-name)))
    (with-current-buffer chat-buf
      ;; Apply the agent preset (brings tools + skill list); SYSTEM below
      ;; then overrides the role, keeping our why-first structure.
      ;; Idempotent: re-apply whenever tools aren't yet buffer-local, so
      ;; reused buffers (created before the preset existed) get fixed too.
      (when (and org-sm-gptel-agent-preset
                 (not (local-variable-p 'gptel-tools))
                 (fboundp 'gptel--apply-preset)
                 (gptel-get-preset org-sm-gptel-agent-preset))
        (when (fboundp 'gptel-agent-update) (gptel-agent-update))
        (gptel--apply-preset org-sm-gptel-agent-preset
                             (lambda (sym val)
                               (set (make-local-variable sym) val))))
      (setq-local gptel--system-message system)
      (when (and src-file (file-readable-p src-file))
        (require 'gptel-context)
        (setq-local gptel-context nil)
        (gptel-context-add-file src-file))
      (when new-p
        (goto-char (point-max))
        (insert (format "%s\n\n#+begin_quote\n%s\n#+end_quote"
                        intro subtree))
        (gptel-send)))
    (display-buffer chat-buf
                    '(display-buffer-in-side-window
                      (side . right) (window-width . 0.45)))))

;;;; ---- Commands ------------------------------------------------------------

;;;###autoload
(defun org-sm-gptel-explain ()
  "Open a persistent explanation chat for the current heading.
AI explains TO you (why-first, 3 layers).  Use when stuck.
Applies `org-sm-gptel-agent-preset' so the AI can read book skills and
`learning-coach' via the Skill tool.  Buffer reused on re-invoke."
  (interactive)
  (org-sm-gptel--open-chat
   "explain" org-sm-gptel-system-explain
   "我在复习以下内容时看不懂，请按三层极简解释：
① 是什么（一句话定义，需要时配一个类比）
② 为什么会这样（补上原文跳过的因果中间步骤）
③ 为什么它重要（它解决什么问题、不要会怎样、在更大体系里的位置）

重点放在②③，①一句带过。不要替我制卡，不要生成总结。"))

;;;###autoload
(defun org-sm-gptel-refine ()
  "Refine the current heading body in-place using `gptel-rewrite'.
Works on any org heading, including org-capture buffers.
Card type determines the system prompt:
  cloze → minimum-information principle, one {{}} per card.
  topic → clean prose for incremental reading, preserve paragraphs."
  (interactive)
  (require 'gptel-rewrite)
  (when (org-before-first-heading-p) (user-error "Not inside any org heading"))
  (let* ((bounds (org-sm--body-bounds))
         (type   (org-sm-type)))
    (goto-char (car bounds))
    (push-mark (cdr bounds) t t)
    (let ((gptel--rewrite-directive
           (if (eq type 'cloze) org-sm-gptel-system-cloze org-sm-gptel-system-topic))
          (gptel--rewrite-message
           (if (eq type 'cloze)
               "第一步：判断方向。先看这张卡的 {{}} 填的是「名称/定义」还是「机制/原因/结果」。\
如果是「名称/定义」型且原文有因果关系，先输出：「建议改为『因果型』：[XX导致...]，你要保留定义型还是改为因果型？」等用户决定再执行下一步。\
第二步：检查格式。检查是否违反最小信息原则：若含多个知识点请拆分；\
保留 {{}} 周围能唯一定位答案的最短上下文，其余删除。"
             "去除修辞废话，消除代词歧义（替换为具体名词），保持段落完整可读性——不压缩成摘要。\
重点：把原文藏着、但没明说的因果逻辑显式化（补主语/连接词、「特点是A、B」改「A导致B」），\
但只能显式化原文已有的逻辑，绝不引入新解释/新事实。")))
      (call-interactively #'gptel-rewrite))))
(provide 'org-sm-gptel)
;;; org-sm-gptel.el ends here
