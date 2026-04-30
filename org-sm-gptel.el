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
;; explain / feynman / knowledge-map share the same mechanism:
;;   - Current org file → buffer-local gptel context (refreshed each call).
;;   - Current subtree  → quoted in the opening message.
;;   - Persistent chat buffer *org-sm-<kind>: <heading>*; reused on re-invoke.
;; They differ only in system prompt (AI role) and a one-line intro.
;;
;; Commands:
;;   org-sm-gptel-explain        - AI explains TO you (use when stuck).
;;   org-sm-gptel-feynman        - Socratic dialogue + card-making guidance.
;;                                 Mode A (just finished reading): AI asks questions
;;                                 to draw out your understanding, then guides you
;;                                 to identify what's worth making into a card.
;;                                 Mode B (think you understand): You explain to AI,
;;                                 AI finds gaps without giving answers.
;;                                 Both modes end with Phase 3: card-making guidance
;;                                 (AI asks which word to blank, never generates cards).
;;   org-sm-gptel-knowledge-map  - Knowledge structure + learning-path planner.
;;   org-sm-gptel-refine         - Refine heading body in-place via gptel-rewrite.
;;                                 cloze: checks direction first (why-type vs what-type),
;;                                 then checks minimum-information principle.
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
  "你是费曼技巧学习教练。核心原则：

1. 永远不给答案。无论用户答得对不对，只用追问引导他自己找到答案。
2. 每次只问一个问题，等回答再继续。
3. 提问优先问「为什么」和「会导致什么」，而非「是什么」。
4. 用户表达清晰后，问他哪个词值得挖空——不输出 {{}} 格式，让他自己写。

根据用户开场话自然切换角色：
- 「刚读完/还没整理」→ 主动提问，把他的零散理解引出来
- 「我懂了/来检验我」→ 扮演不懂的初学者，找他解释里的漏洞
- 掌握稳定后 → 给一个新场景，问能否迁移

每隔 2-3 轮附一行：「📊 X/5，你自己感觉呢？」"
  "System prompt for `org-sm-gptel-feynman'."
  :type 'string :group 'org-sm-gptel)

(defcustom org-sm-gptel-system-knowledge-map
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
  "System prompt for `org-sm-gptel-knowledge-map'."
  :type 'string :group 'org-sm-gptel)

(defcustom org-sm-gptel-system-topic
  "你是 SuperMemo 渐进阅读专家，处理阶段一（Topic）材料。任务：
1. 剥离废话、营销语气和过度修辞，保留核心事实。
2. 每段限定一个独立主题；消除代词歧义（替换为具体名词）。
3. 保持段落完整可读性——不要压缩成摘要，用户需要在阅读中理解后才制卡。
4. 集合式列举（「特点有：A、B、C」）改写为有因果逻辑的叙述。
5. 严禁删除因果链。原文中的「因为」「导致」「所以」「才能」等连接词及其前后内容必须完整保留。
   这类材料的核心价值正是「为什么」，删掉因果链等于把活知识变成死记忆。
6. 如果原文只有结论没有说明原因，在段末加注：「【需补充】为什么是这个结论？」
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
         (subtree  (org-sm-gptel--current-subtree))
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
        (insert (format "%s\n\n#+begin_quote\n%s\n#+end_quote"
                        intro subtree))
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

重要：不要替我制卡、不要生成总结、不要出填空题。
我需要的是理解，不是现成的卡。读完请关闭窗口，
用自己的话尝试复述一遍，再自己动手制卡。"))

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
(defun org-sm-gptel-knowledge-map ()
  "Open a persistent knowledge-map chat for the current subtree.
Current org file added to context so the AI sees all existing notes.
Buffer *org-sm-knowledge-map: <heading>* is reused on re-invoke.

Flow (AI manages automatically):
  Phase 1 — Knowledge structure: hierarchy, gaps, confusion points.
  Phase 2 — Prerequisite checklist: you locate yourself.
  Phase 3 — Tailored learning path."
  (interactive)
  (org-sm-gptel--open-chat
   "knowledge-map" org-sm-gptel-system-knowledge-map
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
               "第一步：判断方向。先看这张卡的 {{}} 填的是「名称/定义」还是「机制/原因/结果」。\
如果是「名称/定义」型且原文有因果关系，先输出：「建议改为『因果型』：[XX导致...]，你要保留定义型还是改为因果型？」等用户决定再执行下一步。\
第二步：检查格式。检查是否违反最小信息原则：若含多个知识点请拆分；\
保留 {{}} 周围能唯一定位答案的最短上下文，其余删除。"
             "去除修辞废话，消除代词歧义（替换为具体名词），\
保持段落完整可读性——不要压缩成摘要，这是 Topic 阶段材料。")))
      (call-interactively #'gptel-rewrite))))

(provide 'org-sm-gptel)
;;; org-sm-gptel.el ends here
