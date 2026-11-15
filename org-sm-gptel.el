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
;; Commands:
;;   org-sm-gptel-suggest   - (x while reviewing a topic) the AI underlines up
;;                            to three passages worth extracting in the text
;;                            on screen or the region, each with a reason.
;;                            It quotes; it never writes a card.  RET on an
;;                            underline extracts it with `org-sm-extract',
;;                            DEL erases it, TAB moves to the next.
;;   org-sm-gptel-explain   - (?) explain the paragraph or region you are stuck
;;                            on, with the surrounding text, parent extract
;;                            origin, and card coordinates for on-demand lookup.
;;                            Persistent side-window chat.
;;   org-sm-gptel-card-ai   - (C-c C-a in a card workbench) rewrite the body
;;                            you are editing: refine, simplify, or a custom
;;                            instruction.  Cards you typed below are kept.
;;
;; Division of labour: the AI points at text and rewrites prose on request;
;; deciding what becomes a card, and cloze deletion, stay with the reader,
;; since generating the item is where encoding happens.  Capture never calls
;; AI.

;;; Code:

(require 'org-sm)
(require 'gptel)

(declare-function gptel-request "gptel")
(declare-function org-sm-set-body "org-sm")
(declare-function org-sm-capture-preamble-bounds "org-sm")
(declare-function org-sm-capture-set-preamble "org-sm")
(declare-function gptel--apply-preset "gptel")
(declare-function gptel-get-preset "gptel")
(declare-function gptel-agent-update "gptel-agent")
(defvar gptel-context)
(defvar gptel-use-context)
(defvar gptel-use-tools)
(defvar gptel-confirm-tool-calls)
(defvar gptel-include-reasoning)
(defvar gptel-post-response-functions)
(defvar gptel-system-prompt)
(declare-function gptel--fsm-live-p "gptel")

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

(defcustom org-sm-gptel-style
  "写法要求：
1. 动作用动词。写「分析日志」，不写「对日志进行分析」。
2. 一句只说一件事。不用分号或破折号把几件事连成一句。
3. 同一个概念全文只用一个名字。
4. 直接陈述。不写「不是 X 而是 Y」，除非读者确实持有 X 这个看法。
5. 不加收尾句。不用「这说明了……的重要性」之类的话重复上文。
6. 说重要性时写具体后果：不这样会怎样。不写「至关重要」「核心」。
7. 删掉套话和营销词。不凑三项排比，不给列表项加粗标签。
8. 保留语气强弱。原文写「可能」「通常」，照写，不升格成定论。
9. 因为、所以、导致、从而是内容，保留，不拆散因果链。
10. 不添加原文没有的事实、例子或数字。"
  "Writing rules appended to every prompt whose output a person reads.
Distilled from controlled technical Chinese (ASD-STE100: verbs over
nominalisations, one name per concept, no semicolon chains) and from the
catalogue of AI writing tells behind Wikipedia's \"Signs of AI writing\"
\(staged contrasts, closers, inflated importance).  One name per concept
matters twice in a card: synonyms rotated across reviews interfere in
memory.  Causal connectives are exempt from sentence splitting, the same
way the source rules treat modality as content.  Not applied to suggestion
quotes, which must stay verbatim to be located."
  :type 'string :group 'org-sm-gptel)

(defcustom org-sm-gptel-system-explain
  "你是学习导师。用户在渐进阅读中读到一段看不懂的文字，你帮他理解这一段。

按三层回答，每层一到两句。重点放在第二、三层，第一层一句带过：
① 是什么：一句话定义，需要时配一个类比。
② 为什么会这样：原文从 A 直接跳到 C 时，补出原文省略的中间步骤 B。
③ 不这样会怎样：它解决什么问题，没有它会出什么具体后果，它在更大的体系里接在哪里。
   这一层把孤立的知识接回主线，所以不能省。只停在定义，读者记住的是一个容易忘、也用不上的事实。

可以用工具查资料（若具备 Skill / Read / Bash）：
- 遇到领域知识，先读对应的书 skill（例如 ai-processor-architecture）核对，不凭空猜。
- 若需核对原书上下文或前后页推导：看到 `page:FILE::N` 链接时可运行 `mutool draw -q -F txt -o - FILE N` 读取对应页及相邻页；看到 `zotero://`、`#+reference:` 或仅有摘录原句时，可用 `zotero-skill`（如 `locate`）在 Zotero 全文索引中检索原书与页码。
- 只读不写。可以用 Skill、Read、Grep、只读命令查资料，不用 Edit、Write 改文件。

约束：
- 只解释。不制卡，不写填空，不写总结。
- 某一层不适用时（纯约定、纯事实），直说「这是约定，没有为什么」，不编造因果。
- 需要类比时，给两到三个不同领域的类比供用户挑选。有层次、流程或对比关系时，用 ASCII 图或 org 表格。
- 解释完问一句：「哪个词去掉了，这句话就不成立？」然后等用户回答，不给提示。
  用户回答后，用一句话确认或纠正，不展开。制卡留给用户。"
  "System prompt for `org-sm-gptel-explain'."
  :type 'string :group 'org-sm-gptel)

(defcustom org-sm-gptel-system-topic
  "你是渐进阅读编辑，整理一段阅读材料（topic），让用户重读时更容易理解。

1. 删掉废话、营销语气和过度修辞，保留全部事实。
2. 每段只讲一个主题。把有歧义的代词换成具体名词。
3. 保持段落完整可读，不压缩成摘要。用户读懂之后才会制卡。
4. 把罗列式写法（「特点有 A、B、C」）改成带因果的叙述。
5. 保留因果链。原文里的「因为」「导致」「所以」「才能」以及它们连接的内容，一个都不删。
   这类材料的价值就在「为什么」，删掉因果链，读者只能死记结论。
6. 原文隐含但没写明的因果，写成明文：补出省略的主语，补上「因为……所以……」，
   把「特点是 A、B」改成「A 导致 B」。这样用户重读时不必自己在脑子里重建逻辑。
   只能写明原文已有的逻辑，不引入原文没有的解释、事实或推导。补充解释是 explain 的工作。
7. 原文只有结论、没有原因，而且原文里也推不出原因时，在段末加注：「【需补充】为什么是这个结论？」
   让用户自己去找原因，你不代答。
8. 这是阅读卡（topic），不是填空卡：禁止在 body 里写 {{}} 挖空符号。值得重点摘录的句子放进 picks 数组。
9. 保留已有子卡标记：正文里形如 `⌈…[[id:xxx][<T>]]` 或 `[[id:xxx][<C>]]` 的标记代表该句已经摘成子卡。改写时必须原样保留 `⌈` 与 `[[id:xxx][<T>/<C>]]` 链接（ID 一个字符都不能改、不能删）。"
  "System prompt for topic refine operations."
  :type 'string :group 'org-sm-gptel)

(defcustom org-sm-gptel-system-cloze
  "你是 SuperMemo 完形填空（cloze）编辑，按最小信息原则帮用户精简一道填空卡。

核心不变量（挖空守恒）：
1. 原文已有 1 个或多个 `{{...}}` 时：原文有几个 `{{...}}`（无论是单空，还是递进链条/正反对比的多空），输出必须原样保留这几个 `{{...}}` 所测的考点——不删减任何一个已有的空，不把空换到别的词上，也不额外多挖新空（换考点会破坏已有的记忆稳定性记录）。若原有空带有 `{{答案::提示}}` 消歧线索，一并保留。
2. 原文还没有任何 `{{...}}` 时：在最核心的因果机制（为什么/怎么做，而非名称定义）处打上 1 个 `{{...}}`。
3. 括号内长度：如果某个 `{{...}}` 里的文字超过 5 个词，可以微调精简括号内的字词，但所测的意思不能变。
4. 题干瘦身与线索锚点：删掉与考点无关的背景铺垫、套话和多余分句，只保留能唯一定位 `{{...}}` 的最短因果或对比结构。如果是递进多空（如 `起点 → {{步骤1}} → {{步骤2}}`），必须保留明文的「起点」锚点，保证把空遮住后读者仍知道从哪里开始推导。
5. 句尾语义检索与按需消歧：单空卡片尽量调整为「前提/条件在前，`{{核心机制/结论}}` 在句尾」，迫使读者先读完整个前提再检索答案，防止靠前半句字面语感秒猜。默认不加 `::提示`；仅当遮住答案后存在多个不同维度或粒度的合法答案时，才在双冒号后补极短的答案形状/类别约束（如 `{{答案::填机制而非现象}}`），题干已能唯一锁死答案则不加，严禁在提示里写同义词或泄露字面线索。
6. 禁止代劳：题干只保留能触发用户自己推导的最小线索，不要在题干里把答案同义复述一遍。"
  "System prompt for cloze refine operations."
  :type 'string :group 'org-sm-gptel)

(defcustom org-sm-gptel-system-simplify
  "你把一段正文精简到只剩必须记住的核心，删得比 refine 更多。

1. 只保留结论、核心事实和关键因果链。删掉背景铺垫、重复的表述，以及不支撑结论的例子。
2. 可以合并同类项，可以删次要的限定语。因果链本身（为什么）必须保留，只删多余的解释文字。
3. 篇幅一般是原文的三分之一到一半。
4. 只删减与合并，不扩写，不引入原文没有的事实或结论。
5. 输出仍是完整可读的正文，不是关键词列表。
6. 正文里若已有 `⌈` 与 `[[id:xxx][<T>/<C>]]` 子卡回链标记，必须原样保留，不能删除或修改 ID。"
  "System prompt for the `simplify' `org-sm-gptel-card-ai' mode.
More aggressive than `org-sm-gptel-system-topic'/`-cloze': cuts length, not
just noise."
  :type 'string :group 'org-sm-gptel)

(defcustom org-sm-gptel-system-suggest
  "你是 SuperMemo 渐进阅读专家。用户正在读一段文字，想知道哪些句子最值得画线摘录成独立的子卡。

任务：从给定原文中挑出 1 到 3 段最值得单独摘录的核心句子（除非原文只有一句话且毫无信息量，否则至少挑 1 条）。
挑选优先级：可迁移的规律与因果机制（什么条件下 A 导致 B） > 关键概念边界 > 核心结论或定义。细节、铺垫不挑。

硬约束：
1. quote 必须逐字复制原文中连续的一段（一到三句），不改一个字、不加省略号、不拼接。
   程序会丢弃对不上原文的引文。
2. 跳过已经带有 `⌈…[[id:…][<T>/<C>]]` 标记的句子（那些已经摘成子卡了），只从未摘录的句子里挑。
3. why 用一句话说明它为什么值得单独复习，不复述原文。"
  "System prompt for `org-sm-gptel-suggest'.
Asks for verbatim quotes only, which are then located in the source: a
quote that cannot be found is dropped, so a suggestion can never point at
text that is not there."
  :type 'string :group 'org-sm-gptel)

(defconst org-sm-gptel--card-modes
  '(("refine" . refine)
    ("简化"   . simplify)
    ("自定义提示词"))   ; . custom, filled in with the typed prompt at call time
  "Choices for `org-sm-gptel-card-ai''s mode prompt.
Alist of (LABEL . SYMBOL); the last entry's cdr is left nil as a marker for
\='custom, whose actual system prompt is whatever the user types.")

(defun org-sm-gptel--card-system (type mode &optional custom)
  "Return the system prompt rewriting a body of TYPE in MODE.
TYPE is `topic', `cloze' or nil; MODE is `refine', `simplify' or `custom',
the last using CUSTOM as the task.  Every mode ends with
`org-sm-gptel-style', since every result becomes text the reader rereads."
  (concat (pcase mode
            ('refine (if (eq type 'cloze) org-sm-gptel-system-cloze
                       org-sm-gptel-system-topic))
            ('simplify org-sm-gptel-system-simplify)
            ('custom (string-trim custom)))
          "\n\n" org-sm-gptel-style
          (if (or (eq type 'cloze) (eq mode 'simplify))
              "\n\n输出结构：{body: 改写后的正文, picks: []}（picks 固定为空数组）。"
            (concat "\n\n重点画线规则（picks，1 到 3 条）："
                    "\n- 在改写后的 body 中，从未带有 `⌈…[[id:…][<T>/<C>]]` 标记的句子里挑出 1 到 3 处最核心的句子放进 picks 数组（程序会给它们画上波浪下划线，供用户摘成子卡）；已经带 `[[id:…]]` 标记的句子不要再挑。"
                    "\n- 挑选优先级：可迁移的规律与因果机制（什么条件下 A 导致 B） > 关键概念边界 > 核心结论或定义。"
                    "\n- quote 必须逐字复制你刚生成的 body 里连续的一段（一到两句），标点一致、一字不差，否则程序无法定位画线。"
                    "\n- why 用一句话说明为什么值得单独复习。"
                    "\n- 禁止在 body 中使用 {{}} 挖空语法（除非原本就是填空卡）。"))))

;;;; ---- Internal helpers ----------------------------------------------------

(defun org-sm-gptel--workbench-only ()
  "Signal a `user-error' unless this is an `org-sm-card-workbench' buffer.
The AI only ever acts on an existing card, i.e. while you read or review
it.  Plain `org-sm-capture' buffers deliberately hold raw material and
never call AI; `org-sm-capture-parent-marker' (non-nil only in a workbench)
is what tells the two apart."
  (unless (and (derived-mode-p 'org-sm-capture-mode)
               org-sm-capture-parent-marker)
    (user-error "AI needs a card workbench buffer (C-c s A on a card); capture stores raw text only")))

(defun org-sm-gptel--buf-name (kind heading)
  "Return a stable chat buffer name for KIND (string) and HEADING string."
  (format "*org-sm-%s: %s*"
          kind
          (truncate-string-to-width
           (replace-regexp-in-string "[^[:alnum:][:blank:]-]" "" heading)
           40 nil nil t)))

(defcustom org-sm-gptel-explain-context 1500
  "Characters of surrounding text sent with the passage being explained.
Enough for the argument the passage belongs to, short enough that a long
article does not drown the question."
  :type 'integer :group 'org-sm-gptel)

(defun org-sm-gptel--passage (&optional beg end)
  "Return (PASSAGE . CONTEXT) for BEG..END, or region/paragraph at point.
CONTEXT is up to `org-sm-gptel-explain-context' characters of the card body
around the span."
  (let* ((span (if (and beg end)
                   (cons beg end)
                 (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (org-sm-paragraph-bounds))))
         (body (org-sm-body-bounds))
         (from (max (car body) (- (car span) org-sm-gptel-explain-context)))
         (to   (min (cdr body) (+ (cdr span) org-sm-gptel-explain-context))))
    (cons (string-trim (buffer-substring-no-properties (car span) (cdr span)))
          (concat (buffer-substring-no-properties from (car span))
                  "【……】"
                  (buffer-substring-no-properties (cdr span) to)))))

(defun org-sm-gptel--draft-p ()
  "Return non-nil when the chat ends with text typed but not yet sent.
A new question is appended at the end of the chat, so it must not be glued
onto, and sent along with, half a sentence the user was still writing."
  (save-excursion
    (goto-char (point-max))
    (let* ((match (text-property-search-backward 'gptel 'response t))
           (tail (string-trim (buffer-substring-no-properties
                               (if match (prop-match-end match) (point-min))
                               (point-max)))))
      (not (member tail (list "" (string-trim (gptel-prompt-prefix-string))))))))

(defun org-sm-gptel--parse-json (response)
  "Parse a JSON object plist from RESPONSE (raw or fenced), or return nil."
  (when (stringp response)
    (or (ignore-errors
          (json-parse-string response :object-type 'plist :array-type 'list))
        (when-let* ((beg (string-search "{" response))
                    (end (string-match "}[^}]*\\'" response)))
          (ignore-errors
            (json-parse-string (substring response beg (1+ end))
                               :object-type 'plist :array-type 'list))))))

(defun org-sm-gptel--request (prompt system schema callback)
  "Send isolated JSON/text PROMPT to gptel and call CALLBACK with (TEXT ERR).
Disables external context, agent tools, and reasoning blocks so structured
single-turn requests stay fast and deterministic."
  (let ((gptel-use-context nil)
        (gptel-use-tools nil)
        (gptel-include-reasoning nil))
    (gptel-request prompt
      :system system
      :schema schema
      :transforms nil
      :callback
      (lambda (response info)
        (cond
         ((eq (car-safe response) 'reasoning) nil)
         ;; On Anthropic backends `:schema' is emulated via a `response_json'
         ;; tool call; if the model emits preamble text in the same message,
         ;; gptel fires the text callback before `gptel--handle-tool-use'
         ;; delivers the tool JSON.  Skip that non-JSON preamble while the
         ;; tool call is still pending.
         ((and schema
               (stringp response)
               (plist-get info :tool-use)
               (null (org-sm-gptel--parse-json response)))
          nil)
         ((stringp response)
          (funcall callback response nil))
         (t
          (funcall callback nil
                   (format "%s" (or (plist-get info :status)
                                    "AI request failed")))))))))

(defun org-sm-gptel--clean-chat-reply (beg end)
  "Return assistant reply in BEG..END without tool or reasoning blocks."
  (let ((raw (buffer-substring-no-properties beg end)))
    (string-trim
     (replace-regexp-in-string
      "\\(?:#\\+begin_\\(?:tool\\|reasoning\\)\\(?:.*\n\\)*?#\\+end_\\(?:tool\\|reasoning\\)\n?\\|``` *tool[^\n]*\n\\(?:.*\n\\)*?```\n?\\)"
      "" raw))))

(defun org-sm-gptel--open-chat (kind system message &optional callback)
  "Open the KIND chat for the current card and send MESSAGE to it.
SYSTEM is the system prompt.  One chat per card, reused across calls, so
a second question about another paragraph continues the conversation
instead of starting over.  When CALLBACK is non-nil, run without popping
up a window and call CALLBACK with (REPLY ERR) when the response finishes."
  (let* ((heading  (save-excursion
                     (org-back-to-heading t)
                     (org-get-heading t t t t)))
         (buf-name (org-sm-gptel--buf-name kind heading))
         (chat-buf (gptel buf-name)))
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
      ;; No attached files: the message already carries the passage, its
      ;; neighbourhood, its origin and coordinates for on-demand lookup.
      ;; Attaching the card's file would resend the whole note -- 200 KB for
      ;; the inbox -- every turn.
      (setq-local gptel-system-prompt system
                  gptel-context nil)
      (cond
       ((org-sm-gptel--draft-p)
        (if callback
            (funcall callback nil "unsent draft in chat buffer")
          (message "org-sm: 聊天里有未发送的草稿，先发送或删除它")))
       ((and (fboundp 'gptel--fsm-live-p) (gptel--fsm-live-p))
        (if callback
            (funcall callback nil "chat request already in progress")
          (message "org-sm: 聊天正在等待回复")))
       (t
        (goto-char (point-max))
        (let ((pos (point)))
          (unless (bolp) (insert "\n"))
          (insert message)
          (if (not callback)
              (gptel-send)
            (let ((gptel-confirm-tool-calls nil))
              (letrec ((done (lambda (beg end)
                               (remove-hook 'gptel-post-response-functions done t)
                               (let ((reply (and beg end (<= beg end)
                                                 (org-sm-gptel--clean-chat-reply beg end))))
                                 (if (org-string-nw-p reply)
                                     (funcall callback reply nil)
                                   (delete-region pos (point-max))
                                   (funcall callback nil "empty response"))))))
                (add-hook 'gptel-post-response-functions done nil t)
                (condition-case err
                    (gptel-send)
                  (error
                   (remove-hook 'gptel-post-response-functions done t)
                   (delete-region pos (point-max))
                   (signal (car err) (cdr err)))))))))))
    (unless callback
      (display-buffer chat-buf
                      '(display-buffer-in-side-window
                        (side . right) (window-width . 0.45))))))

;;;; ---- Commands ------------------------------------------------------------

(defun org-sm-gptel--origin-block (origin)
  "Return ORIGIN, from `org-sm-extract-origin', as a labelled message block.
nil when ORIGIN is nil, so callers can `concat' it unconditionally."
  (when origin
    (concat "#+begin_example\n" (car origin) "\n【……这张卡摘自这里……】\n"
            (cdr origin) "\n#+end_example")))

(defun org-sm-gptel--card-provenance ()
  "Resolve provenance for card at point: `:SOURCE:' (inherited) -> file header."
  (org-with-wide-buffer
    (save-excursion
      (when (org-sm--goto-enclosing-card)
        (or (org-string-nw-p (org-entry-get nil "SOURCE" t))
            (when-let* ((kw (org-collect-keywords
                             '("SOURCE" "SOURCE_URL" "REFERENCE")))
                        (val (cadr (car kw))))
              (org-string-nw-p (string-trim (string-remove-prefix ";" val)))))))))

(defun org-sm-gptel--card-coordinates ()
  "Return a one-line location and provenance hint for the card at point.
Gives the agent in `org-sm-gptel-explain' the file path, line number, card
ID, resolved provenance (from the card, its ancestors, or file keywords),
and vault directory so it can pull wider context on demand."
  (org-with-wide-buffer
    (save-excursion
      (when (org-sm--goto-enclosing-card)
        (let ((file (buffer-file-name))
              (line (line-number-at-pos))
              (id   (org-entry-get nil "ID"))
              (src  (org-sm-gptel--card-provenance))
              (dir  (and org-sm-directory (expand-file-name org-sm-directory))))
          (when (or file id src dir)
            (concat "【卡片坐标："
                    (string-join
                     (delq nil
                           (list (and file (format "文件 %s 第 %d 行" file line))
                                 (and id (format "ID %s" id))
                                 (and src (format "出处 %s" src))
                                 (and dir (format "笔记库 %s" dir))))
                     "，")
                    "。若以上上下文不够，可主动用 Read 读取该文件相邻部分、用 Grep 搜索笔记库，或按需查 PDF / Zotero 原书。】")))))))

(defun org-sm-gptel-explain-card (&optional beg end reply callback)
  "Explain BEG..END of card at point (or send follow-up REPLY) in its chat.
Uses the card's `*org-sm-explain: ...*' buffer with `org-sm-gptel-agent-preset'
tools enabled.  When CALLBACK is non-nil, call it with (PASSAGE ANSWER ERR);
otherwise display the chat side window."
  (let* ((followup (org-string-nw-p reply))
         (pair     (unless followup (org-sm-gptel--passage beg end)))
         (passage  (car pair))
         (msg      (if followup
                       (string-trim reply)
                     (when (and callback (string-blank-p (or passage "")))
                       (user-error "Nothing to explain"))
                     (let ((origin (org-sm-gptel--origin-block (org-sm-extract-origin)))
                           (coords (org-sm-gptel--card-coordinates)))
                       (concat
                        "我读到这一段卡住了，请按三层极简解释（重点②③，①一句带过，不要制卡、不要总结）：\n\n"
                        "#+begin_quote\n" passage "\n#+end_quote\n\n"
                        "【前后文，供理解，不必解释】\n#+begin_example\n" (cdr pair) "\n#+end_example"
                        (when origin (concat "\n\n【这张卡在原文里的位置】\n" origin))
                        (when coords (concat "\n\n" coords))))))
         (sys      (concat org-sm-gptel-system-explain "\n\n" org-sm-gptel-style)))
    (if callback
        (org-sm-gptel--open-chat
         "explain" sys msg
         (lambda (ans err)
           (funcall callback passage ans err)))
      (org-sm-gptel--open-chat "explain" sys msg))))

;;;###autoload
(defun org-sm-gptel-explain ()
  "Explain the paragraph at point, or the region, in this card's chat.
Sends the passage itself, the text around it, the parent text this card
was extracted from, and the card's file/line coordinates for on-demand tool
search.  Applies `org-sm-gptel-agent-preset' so the AI can consult book
skills, read PDF pages or query Zotero, and search the vault when the
immediate neighbourhood is not enough."
  (interactive)
  (org-sm-gptel-explain-card))

;;;; ---- Suggestions: the AI underlines, the reader extracts ------------------

(defconst org-sm-gptel--suggest-schema
  '(:type "object"
    :properties
    (:picks (:type "array"
             :items (:type "object"
                     :properties (:quote (:type "string") :why (:type "string"))
                     :required ["quote" "why"])))
    :required ["picks"])
  "JSON schema for `org-sm-gptel-suggest': verbatim quotes with reasons.")

(defun org-sm-gptel--locate (quote beg end)
  "Return (START . END) of QUOTE between BEG and END, or nil.
Exact text first, then with whitespace runs matched loosely, since models
often re-wrap lines.  Extract markers inside the source are ignored too,
so an already highlighted passage can still be found."
  (let ((quote (string-trim quote)))
    (when (>= (length quote) 4)
      (save-excursion
        (goto-char beg)
        (or (and (search-forward quote end t)
                 (cons (match-beginning 0) (match-end 0)))
            (progn
              (goto-char beg)
              (and (re-search-forward
                    (mapconcat (lambda (part) (regexp-quote part))
                               (split-string quote "[ \t\n]+" t)
                               "[ \t\n]*")
                    end t)
                   (cons (match-beginning 0) (match-end 0)))))))))

(defun org-sm-gptel--parse-picks (response)
  "Return the (QUOTE . WHY) pairs in RESPONSE, nil when it cannot be read."
  (when-let* ((data (org-sm-gptel--parse-json response)))
    (delq nil (mapcar (lambda (p)
                        (let ((q (plist-get p :quote)) (w (plist-get p :why)))
                          (and (stringp q) (stringp w) (cons q w))))
                      (plist-get data :picks)))))

(defun org-sm-gptel--resolve-picks (picks beg end)
  "Locate PICKS within BEG..END as 0-based (:start S :end E :quote Q :why W).
Drops any pick whose quote cannot be found or overlaps an already-extracted
`⌈...[[id:...][<T>/<C>]]' span."
  (let (out)
    (dolist (pick picks (nreverse out))
      (when-let* ((span (org-sm-gptel--locate (car pick) beg end))
                  (slice (buffer-substring-no-properties (car span) (cdr span))))
        (unless (string-match-p "⌈\\|\\[\\[id:[^]\n]+\\]\\[<[TC]>\\]\\]" slice)
          (push (list :start (- (car span) beg)
                      :end   (- (cdr span) beg)
                      :quote slice
                      :why   (cdr pick))
                out))))))

(defun org-sm-gptel--draw-resolved-picks (resolved beg end)
  "Draw already-resolved 0-based PICKS within BEG..END; return count drawn."
  (org-sm-pencils-clear beg end)
  (dolist (p resolved (length resolved))
    (org-sm-pencil-add (+ beg (plist-get p :start))
                       (+ beg (plist-get p :end))
                       (plist-get p :why))))

(defun org-sm-gptel--draw-picks (picks beg end)
  "Underline PICKS found between BEG and END; return how many were drawn.
A pick whose quote is not in the text, or that overlaps an already-extracted
`⌈...[[id:...][<T>/<C>]]' span, is dropped: a suggestion must point at
unextracted text the reader can see."
  (org-sm-gptel--draw-resolved-picks
   (org-sm-gptel--resolve-picks picks beg end) beg end))

(defun org-sm-gptel-request-suggest (text callback)
  "Ask the AI for verbatim extraction picks in TEXT and call CALLBACK.
Resolves quotes against TEXT at the seam; CALLBACK receives (PICKS ERR) where
each pick is a 0-based (:start S :end E :quote Q :why W) plist."
  (org-sm-gptel--request
   text org-sm-gptel-system-suggest org-sm-gptel--suggest-schema
   (lambda (response err)
     (if err
         (funcall callback nil err)
       (let ((raw (org-sm-gptel--parse-picks response)))
         (funcall callback
                  (with-temp-buffer
                    (insert text)
                    (org-sm-gptel--resolve-picks raw (point-min) (point-max)))
                  nil))))))

;;;###autoload
(defun org-sm-gptel-suggest ()
  "Underline passages worth extracting in the text on screen or the region.
The AI quotes; nothing is written.  Accept an underline with `M-z' to extract
it through `org-sm-extract', erase it with `M-k', move on with `M-n'.  The
underlines vanish with the buffer, so nothing half-decided reaches a file."
  (interactive)
  (unless (save-excursion (org-sm--goto-enclosing-card))
    (user-error "Not inside an org-sm card"))
  (let* ((span (org-sm-reading-bounds))
         (beg (copy-marker (car span)))
         (end (copy-marker (cdr span) t))
         (text (buffer-substring-no-properties beg end))
         (buf (current-buffer)))
    (when (string-blank-p text) (user-error "Nothing to read here"))
    (message "org-sm: 找值得摘的段落...")
    (org-sm-gptel-request-suggest
     text
     (lambda (picks err)
       (cond
        ((not (buffer-live-p buf)) nil)
        (err (message "org-sm AI failed: %s" err))
        (t
         (with-current-buffer buf
           (if (zerop (org-sm-gptel--draw-resolved-picks picks beg end))
               (message "org-sm: 这一屏没有值得单独摘的")
             (goto-char beg)
             (org-sm-pencil-next)))))))))

;;;; ---- Rewriting a body in the workbench -------------------------------------

(defvar-local org-sm-gptel--request-serial 0
  "Generation of the latest AI request in this workbench buffer.")

(defconst org-sm-gptel--card-schema
  '(:type "object"
    :properties
    (:body (:type "string")
     :picks (:type "array"
             :items (:type "object"
                     :properties (:quote (:type "string") :why (:type "string"))
                     :required ["quote" "why"])))
    :required ["body" "picks"])
  "JSON schema for `org-sm-gptel-card-ai': rewritten body plus verbatim picks.")

(defun org-sm-gptel--parse-card-response (response)
  "Return (BODY . PICKS) in RESPONSE, or nil when it cannot be read."
  (when-let* ((data (org-sm-gptel--parse-json response))
              (body (plist-get data :body))
              ((and (stringp body) (org-string-nw-p body))))
    (cons (string-trim body) (org-sm-gptel--parse-picks response))))

(defun org-sm-gptel--span-context (bounds full)
  "Return (BEFORE . AFTER) around BOUNDS within FULL bounds, or nil."
  (let ((beg (car bounds)) (end (cdr bounds))
        (full-beg (car full)) (full-end (cdr full)))
    (when (or (> beg full-beg) (< end full-end))
      (let ((before (string-trim
                     (buffer-substring-no-properties
                      (max full-beg (- beg org-sm-extract-origin-chars)) beg)))
            (after  (string-trim
                     (buffer-substring-no-properties
                      end (min full-end (+ end org-sm-extract-origin-chars))))))
        (when (or (org-string-nw-p before) (org-string-nw-p after))
          (cons (or before "") (or after "")))))))

(defun org-sm-gptel--card-request (text type mode custom callback)
  "Ask the AI to rewrite TEXT per MODE and call CALLBACK with the result.
TYPE and MODE/CUSTOM select the system prompt (see
`org-sm-gptel--card-system').  CALLBACK receives (BODY PICKS ERR)."
  (org-sm-gptel--request
   text (org-sm-gptel--card-system type mode custom) org-sm-gptel--card-schema
   (lambda (response err)
     (if err
         (funcall callback nil nil err)
       (let ((parsed (org-sm-gptel--parse-card-response response)))
         (cond
          ((null parsed)
           (funcall callback nil nil "parse failed"))
          ((and (eq type 'cloze)
                (not (string-match-p "{{[^}\n]+}}" (car parsed))))
           (funcall callback nil nil "AI dropped {{cloze}} markers; draft kept"))
          (t
           (funcall callback (car parsed) (cdr parsed) nil))))))))

(defun org-sm-gptel--rewrite-span (bounds full type origin mode custom callback)
  "Rewrite buffer BOUNDS within FULL bounds and call CALLBACK.
Assembles local surrounding context (when BOUNDS is a sub-span of FULL) plus
parent ORIGIN, then dispatches `org-sm-gptel--card-request'."
  (let* ((mode-sym (pcase mode
                     ((or 'refine "refine") 'refine)
                     ((or 'simplify "simplify") 'simplify)
                     ((or 'custom "custom") 'custom)
                     (_ (user-error "Unknown AI rewrite mode: %s" mode))))
         (raw      (string-trim (buffer-substring-no-properties
                                 (car bounds) (cdr bounds))))
         (local    (org-sm-gptel--span-context bounds full))
         (ctx      (string-join
                    (delq nil (list (org-sm-gptel--origin-block local)
                                    (org-sm-gptel--origin-block origin)))
                    "\n\n")))
    (when (and (eq mode-sym 'custom) (string-blank-p (or custom "")))
      (user-error "Custom mode needs a prompt"))
    (when (string-empty-p raw)
      (user-error "No body text to rewrite"))
    (org-sm-gptel--card-request
     (if (org-string-nw-p ctx)
         (concat raw "\n\n【出处上下文：只用来补全正文里的代词和省略的主语，"
                 "其中的其他内容不要写进正文】\n" ctx)
       raw)
     type mode-sym custom callback)))

(defun org-sm-gptel-request-rewrite (text mode custom start end callback)
  "Rewrite draft TEXT (or START..END in it) for card at point and call CALLBACK.
Resolves parent extract origin, splices region rewrites back into TEXT, and
resolves picks into 0-based (:start S :end E :quote Q :why W) offsets in the
returned full body.  CALLBACK receives (BODY PICKS ERR)."
  (unless (org-string-nw-p text)
    (user-error "No body text to rewrite"))
  (let* ((type   (org-sm-type))
         (origin (org-sm-extract-origin))
         (len    (length text))
         (sub-p  (and start end))
         (s      (or start 0))
         (e      (or end len)))
    (unless (and (<= 0 s e len) (< s e))
      (user-error "Selection out of range"))
    (with-temp-buffer
      (insert text)
      (org-sm-gptel--rewrite-span
       (cons (+ (point-min) s) (+ (point-min) e))
       (cons (point-min) (point-max))
       type origin mode custom
       (lambda (body picks err)
         (if err
             (funcall callback nil nil err)
           (let* ((final (if sub-p
                             (concat (substring text 0 s) body (substring text e))
                           body))
                  (resolved (with-temp-buffer
                              (insert body)
                              (mapcar
                               (lambda (p)
                                 (list :start (+ s (plist-get p :start))
                                       :end   (+ s (plist-get p :end))
                                       :quote (plist-get p :quote)
                                       :why   (plist-get p :why)))
                               (org-sm-gptel--resolve-picks
                                picks (point-min) (point-max))))))
             (funcall callback final resolved nil))))))))

;;;###autoload
(defun org-sm-gptel-card-ai ()
  "Rewrite the body (or active region) in this card workbench with the AI.
When a region is active, only that span is rewritten and underlined,
leaving surrounding text and underlines untouched; otherwise the whole
workbench body is rewritten.  Choose refine (clean up, keep every causal
link, underline 0-3 passages worth extracting), 简化 (cut to the core) or a
custom instruction.  An extract also sends the parent text around it, so
\"it\" and \"these neurons\" can be resolved from the source rather than
guessed.  Nothing reaches the source card until
\\<org-sm-capture-mode-map>\\[org-sm-capture-commit], which writes the body
back and extracts the surviving underlines in place."
  (interactive)
  (org-sm-gptel--workbench-only)
  (let* ((region (use-region-p))
         (full   (org-sm-capture-preamble-bounds))
         (bounds (if region (cons (region-beginning) (region-end)) full))
         (label  (completing-read (if region "AI 动作（选区）：" "AI 动作：")
                                  (mapcar #'car org-sm-gptel--card-modes)
                                  nil t))
         (mode   (or (cdr (assoc label org-sm-gptel--card-modes)) 'custom))
         (custom (and (eq mode 'custom) (read-string "提示词：")))
         (beg    (car bounds))
         (end    (cdr bounds))
         (origin (when-let* ((card (org-sm--capture-live-marker)))
                   (org-with-point-at card (org-sm-extract-origin))))
         (buf    (current-buffer))
         (tick   (buffer-chars-modified-tick))
         (serial (cl-incf org-sm-gptel--request-serial)))
    (when region (deactivate-mark))
    (message "org-sm: asking AI (%s)..." label)
    (org-sm-gptel--rewrite-span
     bounds full org-sm-capture-card-type origin mode custom
     (lambda (body picks err)
       (cond
        (err (message "org-sm AI failed: %s" err))
        ((not (buffer-live-p buf)) (message "org-sm AI: buffer gone"))
        ;; An async reply must never clobber what the user typed meanwhile.
        ((with-current-buffer buf
           (or (/= serial org-sm-gptel--request-serial)
               (/= tick (buffer-chars-modified-tick))))
         (message "org-sm AI: draft changed or newer request exists; result ignored"))
        (t
         (with-current-buffer buf
           (let (span-beg span-end)
             (if region
                 (progn
                   (org-sm-pencils-clear beg end)
                   (goto-char beg)
                   (delete-region beg end)
                   (insert (string-trim body))
                   (setq span-beg beg span-end (point)))
               (org-sm-capture-set-preamble body)
               (setq span-beg (point-min) span-end (point-max)))
             (let ((n (org-sm-gptel--draw-picks picks span-beg span-end)))
               (if (> n 0)
                   (progn (goto-char span-beg) (org-sm-pencil-next))
                 (goto-char span-beg)
                 (message "%s" (substitute-command-keys
                                (format "org-sm: AI %s done — edit, then \\<org-sm-capture-mode-map>\\[org-sm-capture-commit]"
                                        label)))))))))))))

;; Wire the AI actions into the workbench and the review repeat map.
(with-eval-after-load 'org-sm
  (keymap-set org-sm-capture-mode-map "C-c C-a" #'org-sm-gptel-card-ai)
  (keymap-set org-sm-review-repeat-map "?" #'org-sm-gptel-explain)
  (keymap-set org-sm-review-repeat-map "x" #'org-sm-gptel-suggest))

(provide 'org-sm-gptel)
;;; org-sm-gptel.el ends here
