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
;; explain / feynman / roadmap share the same mechanism:
;;   - Current org file → buffer-local gptel context (refreshed each call).
;;   - Current subtree  → quoted in the opening message.
;;   - Persistent chat buffer *org-sm-<kind>: <heading>*; reused on re-invoke.
;; They differ only in system prompt (AI role) and a one-line intro.
;;
;; Commands:
;;   org-sm-gptel-explain  - AI explains TO you (use when stuck).
;;   org-sm-gptel-feynman  - You explain TO the AI (Feynman technique).
;;   org-sm-gptel-roadmap  - Concept map + learning-path planner.
;;   org-sm-gptel-refine   - Refine heading body in-place via gptel-rewrite.
;;
;; TODO: Interleaving support
;;   feynman phase 2 (transfer training) could pull scenarios from other SRS
;;   files to force cross-topic comparison — the core mechanism of interleaving.
;;   Blocked on: a reliable lightweight index of the user's full card corpus.
;;   Candidate: llm-wiki index.org (signature hierarchy + one-line summaries)
;;   once llm-wiki maintenance is settled and index density is sufficient.
;;   See: Kornell & Bjork (2008) on interleaving; current FSRS scheduling
;;   already provides some interleaving at the review level.

;;; Code:

(require 'org-sm)
(require 'gptel)

(declare-function gptel-rewrite "gptel-rewrite")
(declare-function gptel-context-add-file "gptel-context")
(defvar gptel--rewrite-directive)
(defvar-local gptel--rewrite-message nil)
(defvar gptel-context)

;;;; ---- Customization -------------------------------------------------------

(defgroup org-sm-gptel nil
  "AI extensions for org-sm."
  :group 'org-sm
  :prefix "org-sm-gptel-")

(defcustom org-sm-gptel-system-explain
  "你是专注于帮助深度理解概念的学习导师。

你的角色：解释者。用户遇到了不理解的内容，你负责用通俗语言讲清楚。

约束：
- 只解释，不制卡。不要主动生成任何填空题或卡片格式。
- 回答精炼，直击本质，避免冗长铺垫。
- 解释抽象概念时，主动从 2-3 个不同领域生成类比（如日常生活、工程、自然现象），
  让用户选择最贴合自己经验的一个，而不是只给一个类比。
- 当概念之间存在层次、流程或对比关系时，用 ASCII 图或 org 表格可视化结构，
  而不只是文字叙述——视觉编码和语言编码同时激活，记忆更牢固。
- 当用户表示已经理解时，提示一次：可用 feynman 命令来真正检验掌握程度。"
  "System prompt for `org-sm-gptel-explain'."
  :type 'string :group 'org-sm-gptel)

(defcustom org-sm-gptel-system-feynman
  "你是费曼技巧学习教练，兼具迁移训练能力。

对话分两个自然阶段，根据用户的表现自动推进，不需要宣布切换：

【阶段一：解释检验】
扮演对当前话题完全陌生的初学者，用户向你解释概念，你的任务是：
1. 指出哪些地方你听不懂（术语未解释、逻辑跳跃、假设了前置知识）
2. 指出哪些重要方面用户没有提到或解释不完整
3. 即使用户的解释明显有误，也绝对不要给出正确答案——只继续追问，
   让用户自己发现错误。给出答案会剥夺用户最关键的学习时机。
当用户连续两轮没有被指出新漏洞时，视为通过阶段一，自然过渡到阶段二，不需要宣布切换。

【阶段二：迁移训练】
给出一个用户没见过的具体场景或新问题（来自不同领域），分两种方式交替使用：
- 具体→抽象：描述一个现象，问「用你刚学的概念解释这个现象」
- 抽象→具体：提出一个新问题，问「如何用这个概念解决它」
每次只出一个场景，等用户回答后再给下一个。

置信度追踪（贯穿全程）：
每隔 2-3 轮，在回复末尾附加一行：
「📊 当前掌握估计：X/5 — [一句话说明判断依据]，你自己感觉呢？」
让用户对照自我评估，校准元认知。

语气：友善、好奇，像求知欲强的学生，而非挑剔的评委。
每次回复最后一句（置信度行除外）必须是问句。"
  "System prompt for `org-sm-gptel-feynman'."
  :type 'string :group 'org-sm-gptel)

(defcustom org-sm-gptel-system-roadmap
  "你是知识地图与学习路径规划师。每次只做一个阶段，完成后等待用户回复再继续。

【阶段一：知识金字塔分析】（基于用户提供的笔记/卡片）

输出以下四个部分，使用 org-mode 格式（标题用 ***，列表用缩进 -）：

*** 知识层次结构
将所有概念按依赖关系分为三层，每个概念后注明与其他概念的关系：
**** 地基层（其他概念依赖它，必须最先掌握）
     - 概念名：一句话说明它是什么，以及哪些上层概念依赖它
**** 核心层（由地基层推导/组合而来）
     - 概念名（由 X+Y 推导）：一句话说明，以及它支撑哪些应用
**** 应用层（核心层的具体实例或延伸）
     - 概念名：一句话说明，依赖哪个核心概念

*** 知识盲区
从三个角度检查用户卡片的覆盖缺口：
1. 隐含的中间概念：卡片中未显式记录、但理解层次结构必须依赖的桥梁概念
   - 概念名：连接了哪两个已有概念，缺少它会导致什么误解
2. 未覆盖的重要分支：该主题下用户完全没有涉及、但对全局理解不可或缺的方向
   - 分支名：一句话说明它是什么，以及忽略它会产生什么认知盲点
3. 知识错觉风险：用户有卡片但表述角度单一，可能产生「我懂了」的错觉
   - 概念名：当前卡片只覆盖了哪个角度，还缺少哪些角度才算真正理解

*** 易混淆点
用表格列出容易混淆的概念对及其决定性区别：
| 概念对 | 表面相似点 | 决定性区别 |
|--------+-----------+------------|

*** 结构风险
指出卡片现有组织方式中可能造成干扰或误解的地方，给出具体建议。

完成后只说「---请告诉我是否继续路径规划---」，等待用户回复，不要继续输出下一阶段。

【阶段二：诊断前置知识】
列出前置知识清单，按层次分组（基础层→中级层→高级层）。
每条用一句话说明「能做什么/理解什么」供用户自我评估。
如 context 中有用户已有笔记，标注哪些层级可能已掌握。
结尾只说「---请告诉我你的基础情况---」，等待用户回复，不要继续输出下一阶段。

【阶段三：规划学习路径】（基于用户回复的起点）
1. 推荐学习顺序（3-5 个阶段，每阶段 1-2 个核心任务）
2. 每个阶段的里程碑：完成后能做到什么
3. 每阶段推荐的资料类型（论文/教程/代码/视频）

约束：路径要和用户起点强相关；参考 context 避免规划已掌握的内容。"
  "System prompt for `org-sm-gptel-roadmap'."
  :type 'string :group 'org-sm-gptel)

(defcustom org-sm-gptel-system-topic
  "你是 SuperMemo 渐进阅读专家，处理阶段一（Topic）材料。任务：
1. 剥离废话、营销语气和过度修辞，保留核心事实。
2. 每段限定一个独立主题；消除代词歧义（替换为具体名词）。
3. 保持段落完整可读性——不要压缩成摘要，用户需要在阅读中理解后才制卡。
4. 集合式列举（「特点有：A、B、C」）改写为有因果逻辑的叙述。"
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
5. 删除「众所周知」等无信息量修辞。"
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
  "Return the current subtree as a string, starting from its heading."
  (save-excursion
    (org-back-to-heading t)
    (buffer-substring-no-properties
     (point)
     (save-excursion (org-end-of-subtree t t) (point)))))

(defun org-sm-gptel--open-chat (kind system intro)
  "Open or reuse a persistent gptel chat buffer for KIND on the current heading.

KIND   - string (\"explain\"/\"feynman\"/\"roadmap\"), used in the buffer name.
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
         (buf-name (org-sm-gptel--buf-name kind heading))
         (new-p    (not (get-buffer buf-name)))
         (chat-buf (gptel buf-name))
         (src-file (buffer-file-name)))
    (with-current-buffer chat-buf
      (setq-local gptel--system-prompt system)
      (when (and src-file (file-readable-p src-file))
        (require 'gptel-context)
        (gptel-context-add-file src-file))
      (when new-p
        (goto-char (point-max))
        (insert (format "%s\n\n当前 heading：%s\n\n#+begin_quote\n%s\n#+end_quote"
                        intro heading (org-sm-gptel--current-subtree)))
        (gptel-send)))
    (display-buffer chat-buf
                    '(display-buffer-in-side-window
                      (side . right) (window-width . 0.45)))))

(defun org-sm-gptel--init-topic ()
  "Set [#A] priority and mark current heading as an org-sm topic."
  (org-priority ?A)
  (org-sm-item-mark 'topic))

;;;; ---- Commands ------------------------------------------------------------

;;;###autoload
(defun org-sm-gptel-explain ()
  "Open a persistent explanation chat for the current heading.
AI explains TO you.  Use when you don't understand the content.
Buffer *org-sm-explain: <heading>* is reused on re-invoke."
  (interactive)
  (org-sm-gptel--open-chat
   "explain" org-sm-gptel-system-explain
   "我在阅读以下内容时遇到了困难，请帮我：
1. 快速评估这段材料的价值（值得深读/浅读/跳过，理由一句话）
2. 解释其中最难理解的部分（专业术语请逐一解释）
3. 若有抽象概念，从 2-3 个不同领域给出类比，让我选择最贴合的

（读完请合上窗口，用自己的话复述一遍再制卡。）"))

;;;###autoload
(defun org-sm-gptel-feynman ()
  "Open a persistent Feynman-technique chat for the current heading.
You explain TO the AI to test the depth of your understanding.
Buffer *org-sm-feynman: <heading>* is reused on re-invoke."
  (interactive)
  (org-sm-gptel--open-chat
   "feynman" org-sm-gptel-system-feynman
   "我认为自己已经理解了以下内容，请向我提问来检验我的理解。"))

;;;###autoload
(defun org-sm-gptel-roadmap ()
  "Open a persistent concept-map + learning-path chat for the current subtree.
Current org file added to context so the AI sees all existing notes.
Buffer *org-sm-roadmap: <heading>* is reused on re-invoke.

Flow (AI manages automatically):
  Phase 1 — Concept map: connections and confusion points.
  Phase 2 — Prerequisite checklist: you locate yourself.
  Phase 3 — Tailored learning path."
  (interactive)
  (org-sm-gptel--open-chat
   "roadmap" org-sm-gptel-system-roadmap
   "请先分析以下笔记/卡片中的知识关联，再帮我规划学习路径。"))

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
               "检查是否违反最小信息原则：若含多个知识点请拆分；\
保留 {{}} 关键词；上下文精简到唯一定位答案所需的最短长度。"
             "去除修辞废话，消除代词歧义（替换为具体名词），\
保持段落完整可读性——不要压缩成摘要，这是 Topic 阶段材料。")))
      (call-interactively #'gptel-rewrite))))

(provide 'org-sm-gptel)
;;; org-sm-gptel.el ends here
