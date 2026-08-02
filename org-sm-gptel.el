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
;;   org-sm-gptel-explain       - AI explains a heading TO you (why-before-what,
;;                                3 layers).  Persistent side-window chat.
;;   org-sm-gptel-capture-ai    - In an `org-sm-capture-mode' buffer (C-c C-a):
;;                                - plain `org-sm-capture': AI-split the
;;                                  content into independent topic cards.
;;                                - `org-sm-card-workbench': ONE AI call both
;;                                  refines the leading body text and proposes
;;                                  child cards (empty if none warranted);
;;                                  re-rendered as body + `**' headings.
;;                                Runs in place; C-c C-c commits whatever
;;                                is present (body and/or `**' cards).
;;                                Call again to re-run.
;;   org-sm-gptel-refine-card   - Inside an `org-sm-capture-mode' buffer
;;                                (C-c C-i): refine just the text at point
;;                                in-place -- a `**' card, or (in a
;;                                card-workbench) the leading body text --
;;                                without touching the rest.  Synchronous
;;                                feel, no workbench of its own: nothing here
;;                                is committed yet anyway.
;;   org-sm-gptel-split-text    - Synchronous text -> cards core (used by web).
;;   org-sm-gptel-rewrite-text  - Synchronous text -> refined text core (web).

;;; Code:

(require 'org-sm)
(require 'gptel)

(declare-function gptel-request "gptel")
(declare-function gptel-context-add-file "gptel-context")
(declare-function org-sm--capture "org-sm")
(declare-function org-sm-capture-fill-cards "org-sm")
(declare-function org-sm--set-body "org-sm")
(declare-function gptel--apply-preset "gptel")
(declare-function gptel-get-preset "gptel")
(declare-function gptel-agent-update "gptel-agent")
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

(defcustom org-sm-gptel-system-capture
  (concat
   "你是 SuperMemo 渐进阅读专家，把一段原始素材拆成一张或多张 Topic 卡片。\n\n"
   "【拆分】\n"
   "1. 一张卡只承载一个独立主题/一条完整因果链；多个独立主题拆成多张。\n"
   "2. 不过度拆：同一推理链、必须放一起才讲得通的，留在同一张。\n"
   "3. 原文本就不可分割时，只输出一张。\n\n"
   "【每张卡正文】同 topic refine 原则：剥离废话、消除代词歧义、保持段落完整不压缩成摘要、"
   "严禁删因果链、显式化隐含逻辑（但不引入新事实）。只有结论无原因时在末尾加「【需补充】为什么？」。\n"
   "【标题】一句概括该卡主题的短语，不含 [T] 前缀。\n\n"
   "只输出结构化结果。")
  "System prompt for `org-sm-gptel-capture-ai' (clean + split into Topic cards)."
  :type 'string :group 'org-sm-gptel)

(defcustom org-sm-gptel-system-split
  (concat
   "\n\n【子卡拆分（可选）】完成上面的正文优化后，再判断原文是否包含值得独立拆出的\n"
   "原子概念/论断——复用价值高、能被其他卡引用、能单独主动回忆的内容。\n\n"
   "拆分倒向（soft）：\n"
   "1. 优先按「一个概念 / 一条论断」原子化拆：这类卡复用价值最高，优先产出。\n"
   "2. 不强行概念化：内容本质是流程/叙事/并列事实时，按其自然结构拆，不硬套。\n"
   "3. 原子但不碎：一张只讲一件事，但要讲完整；同一推理链留在同一张子卡。\n"
   "4. 没有值得拆的内容就返回空数组 cards: []，不要为了拆而拆——大多数卡不需要子卡。\n\n"
   "子卡【标题 = API】优先用可被其他卡引用的句柄：首选完整论断，其次问句，\n"
   "仅核心术语用名词，不含 [T] 前缀。\n"
   "子卡【正文】遵循与正文相同的清理规则（剥离废话、消除代词歧义、不压缩成摘要、\n"
   "不删因果链、显式化隐含逻辑、不引入新事实）；拆出的内容不必从正文里删除，\n"
   "正文与子卡各自独立存在，允许重叠。")
  "Child-splitting instructions appended after the per-type refine prompt
\(`org-sm-gptel-system-topic' / `org-sm-gptel-system-cloze') to build the
combined `org-sm-card-workbench' system prompt -- see
`org-sm-gptel--card-system'.  Biases toward concept/claim atoms (high
memory-reuse value) but stays flexible, and explicitly allows an empty
result when nothing is worth splitting off."
  :type 'string :group 'org-sm-gptel)

(defun org-sm-gptel--card-system (type)
  "Return the combined refine+split system prompt for TYPE (`topic'/`cloze'/nil).
Concatenates the per-type refine prompt (`org-sm-gptel-system-cloze' /
`org-sm-gptel-system-topic') with the child-splitting addendum
\(`org-sm-gptel-system-split') and a final JSON-contract line, so a single
AI call both refines the body and proposes any child cards.  Used by
`org-sm-gptel--card-ai' for `org-sm-card-workbench' buffers."
  (concat (if (eq type 'cloze) org-sm-gptel-system-cloze org-sm-gptel-system-topic)
          org-sm-gptel-system-split
          "\n\n只输出结构化结果：{body: 优化后的正文, cards: 子卡数组（可为空）}。"))

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
(defun org-sm-gptel-refine-card (&optional arg)
  "Refine the text at point in-place using the AI, then replace it.

Works inside any `org-sm-capture-mode' buffer (plain capture or
`org-sm-card-workbench'), two ways:
- point in a `**' heading -> refines that one card's body.
- point before any `**' heading (a card-workbench's leading text, i.e.
  the source card's own body) -> refines that whole block, using the
  source card's original type (`org-sm-capture-card-type', set by
  `org-sm-card-workbench'; nil in plain capture, defaults to the topic
  prompt).

Nothing is committed here yet either way, so an undo-only in-place edit is
fine.  Card type determines the system prompt:
  cloze -> minimum-information principle, one {{}} per card.
  topic -> clean prose for incremental reading, preserve paragraphs.
With prefix ARG, prompt for a one-off extra instruction appended to the
text before sending.

Sends the text and waits for the AI asynchronously (Emacs is not
blocked), then replaces it with the result in place.  There is no
diff/accept step -- use `undo' to revert.  For a combined refine +
child-split in one AI call, use `org-sm-gptel-capture-ai' (\\`C-c C-a')
inside an `org-sm-card-workbench' instead."
  (interactive "P")
  (let* ((in-preamble (org-before-first-heading-p))
         (bounds (if in-preamble
                     (cons (point-min)
                           (save-excursion
                             (goto-char (point-min))
                             (if (re-search-forward "^\\*\\* " nil t)
                                 (match-beginning 0)
                               (point-max))))
                   (progn (org-back-to-heading t) (org-sm--body-bounds))))
         (type   (if in-preamble org-sm-capture-card-type (org-sm-type)))
         (body   (string-trim (buffer-substring-no-properties
                               (car bounds) (cdr bounds))))
         (extra  (and arg (read-string "额外指令：")))
         (start  (copy-marker (car bounds)))
         (end    (copy-marker (cdr bounds)))
         (buf    (current-buffer)))
    (when (string-empty-p body) (user-error "Nothing to refine"))
    (message "org-sm: asking AI to refine...")
    (org-sm-gptel--rewrite-request
     (if (org-string-nw-p extra)
         (concat body "\n\n【额外要求】" (string-trim extra))
       body)
     type
     (lambda (text err)
       (cond
        (err (message "org-sm refine failed: %s" err))
        ((not (buffer-live-p buf)) (message "org-sm refine: buffer gone"))
        (t
         (with-current-buffer buf
           (save-excursion
             (goto-char start)
             (delete-region start end)
             (insert text)
             (unless (bolp) (insert "\n"))))
         (message "org-sm: refined -- undo to revert")))
       (set-marker start nil) (set-marker end nil)))))

;;;; ---- AI capture (split into topic cards) ---------------------------------
;;
;; Adds one action to `org-sm-capture-mode' (from org-sm.el): `C-c C-a' hands
;; the buffer's current content to the AI, which splits/cleans it into one or
;; more topic cards.  The result replaces the buffer content as ** headings,
;; which you then edit freely and commit with `C-c C-c' (core command).  Press
;; `C-c C-a' again to re-split; `C-u C-c C-a' to add a one-off instruction.

(defconst org-sm-gptel--capture-schema
  '(:type "object"
    :properties
    (:cards
     (:type "array"
      :items
      (:type "object"
       :properties
       (:title (:type "string")
        :body  (:type "string"))
       :required ["title" "body"])))
    :required ["cards"])
  "JSON schema: model returns {cards:[{title,body}]}.")

(defun org-sm-gptel--parse-cards (response)
  "Parse RESPONSE JSON into a list of (TITLE . BODY); nil on failure."
  (when (stringp response)
    (let* ((data  (ignore-errors
                    (json-parse-string response :object-type 'plist
                                       :array-type 'list)))
           (cards (plist-get data :cards)))
      (delq nil
            (mapcar (lambda (c)
                      (let ((title (plist-get c :title))
                            (body  (plist-get c :body)))
                        (when (and (stringp body) (org-string-nw-p body))
                          (cons (and (stringp title) (string-trim title))
                                (string-trim body)))))
                    cards)))))

(defun org-sm-gptel--split-request (text extra callback &optional system)
  "Ask the AI to split TEXT into topic cards; call CALLBACK asynchronously.
EXTRA, when non-empty, is appended as a one-off instruction.  SYSTEM is the
system prompt (defaults to `org-sm-gptel-system-capture').  CALLBACK
receives two args: the parsed cards list (or nil) and an error string (or
nil).  The single owner of the schema and request flow for card splitting;
every entry point (capture, web, extract) goes through it."
  (let ((prompt (if (org-string-nw-p extra)
                    (concat text "\n\n【额外要求】" (string-trim extra))
                  text)))
    (gptel-request prompt
      :system (or system org-sm-gptel-system-capture)
      :schema org-sm-gptel--capture-schema
      :callback
      (lambda (response info)
        (if (stringp response)
            (funcall callback (org-sm-gptel--parse-cards response) nil)
          (funcall callback nil (format "%s" (plist-get info :status))))))))

(defun org-sm-gptel-split-text (text &optional extra timeout)
  "Synchronously split TEXT into a list of (TITLE . BODY) topic cards.
Block until the AI responds or TIMEOUT seconds elapse (default 60), then
return the cards list.  Signal an error on failure, timeout, or empty
result.  Shares its logic with `org-sm-gptel-capture-ai' via
`org-sm-gptel--split-request'.  Used where a synchronous result is needed
\(e.g. the web front-end servlet)."
  (let ((done nil) (result nil) (err nil)
        (deadline (+ (float-time) (or timeout 60))))
    (org-sm-gptel--split-request
     text extra
     (lambda (cards error) (setq result cards err error done t)))
    (while (and (not done) (< (float-time) deadline))
      (accept-process-output nil 0.2))
    (cond ((not done)     (error "AI split timed out"))
          (err            (error "AI split failed: %s" err))
          ((null result)  (error "AI split: no cards parsed"))
          (t result))))

(defun org-sm-gptel--rewrite-request (text type callback)
  "Ask the AI to refine TEXT; call CALLBACK asynchronously with the result.
TYPE (`cloze' or `topic') selects the system prompt (`org-sm-gptel-system-cloze'
/ `org-sm-gptel-system-topic').  CALLBACK receives two args: the rewritten
string (or nil) and an error string (or nil).  The single owner of the
rewrite request flow; `org-sm-gptel-refine-card' (async, in-buffer) and
`org-sm-gptel-rewrite-text' (sync, for the web) both go through it."
  (gptel-request text
    :system (if (eq type 'cloze) org-sm-gptel-system-cloze org-sm-gptel-system-topic)
    :callback
    (lambda (response info)
      (if (stringp response)
          (funcall callback (string-trim response) nil)
        (funcall callback nil (format "%s" (plist-get info :status)))))))

(defun org-sm-gptel-rewrite-text (text &optional type timeout)
  "Synchronously refine TEXT with the AI and return the rewritten string.
TYPE (`cloze' or `topic', default topic) selects the system prompt.  Blocks
up to TIMEOUT seconds (default 60); signals an error on failure, timeout,
or an empty result.  Shares its logic with `org-sm-gptel-refine-card' via
`org-sm-gptel--rewrite-request'.  Used by the web front-end."
  (let ((done nil) (result nil) (err nil)
        (deadline (+ (float-time) (or timeout 60))))
    (org-sm-gptel--rewrite-request
     text type
     (lambda (text error) (setq result text err error done t)))
    (while (and (not done) (< (float-time) deadline))
      (accept-process-output nil 0.2))
    (cond ((not done)            (error "AI refine timed out"))
          (err                   (error "AI refine failed: %s" err))
          ((not (org-string-nw-p result)) (error "AI refine: empty result"))
          (t (string-trim result)))))

;;;; ---- AI card action (refine + optional split, one call) ------------------

(defconst org-sm-gptel--card-schema
  '(:type "object"
    :properties
    (:body  (:type "string")
     :cards (:type "array"
             :items
             (:type "object"
              :properties
              (:title (:type "string")
               :body  (:type "string"))
              :required ["title" "body"])))
    :required ["body" "cards"])
  "JSON schema: model returns {body, cards:[{title,body}]} for the combined
refine+split action used by `org-sm-card-workbench'.")

(defun org-sm-gptel--parse-card-response (response)
  "Parse RESPONSE JSON into (BODY . CARDS); nil on failure or missing BODY.
CARDS uses the same (TITLE . BODY) format as `org-sm-gptel--parse-cards'."
  (when (stringp response)
    (let* ((data  (ignore-errors
                    (json-parse-string response :object-type 'plist
                                       :array-type 'list)))
           (body  (plist-get data :body))
           (cards (plist-get data :cards)))
      (when (and (stringp body) (org-string-nw-p body))
        (cons (string-trim body)
              (delq nil
                    (mapcar (lambda (c)
                              (let ((title (plist-get c :title))
                                    (b     (plist-get c :body)))
                                (when (and (stringp b) (org-string-nw-p b))
                                  (cons (and (stringp title) (string-trim title))
                                        (string-trim b)))))
                            cards)))))))

(defun org-sm-gptel--card-request (text type extra callback)
  "Ask the AI to refine TEXT and optionally split it into child cards.
TYPE (`topic'/`cloze'/nil) selects the refine half of the system prompt
\(see `org-sm-gptel--card-system'); EXTRA, when non-empty, is appended as a
one-off instruction.  CALLBACK receives three args: the refined body
string (or nil), the cards list (or nil, possibly empty), and an error
string (or nil).  The single owner of the schema and request flow for
`org-sm-card-workbench''s combined action."
  (let ((prompt (if (org-string-nw-p extra)
                    (concat text "\n\n【额外要求】" (string-trim extra))
                  text)))
    (gptel-request prompt
      :system (org-sm-gptel--card-system type)
      :schema org-sm-gptel--card-schema
      :callback
      (lambda (response info)
        (if (stringp response)
            (let ((parsed (org-sm-gptel--parse-card-response response)))
              (if parsed
                  (funcall callback (car parsed) (cdr parsed) nil)
                (funcall callback nil nil "parse failed")))
          (funcall callback nil nil (format "%s" (plist-get info :status))))))))

(defun org-sm-gptel--plain-capture-ai (arg)
  "AI action for plain `org-sm-capture' buffers: split into independent cards.
ARG, when non-nil, prompts for a one-off extra instruction.  See
`org-sm-gptel-capture-ai'."
  (let ((raw   (string-trim (buffer-substring-no-properties
                             (point-min) (point-max))))
        (extra (and arg (read-string "额外指令（如：拆得更细 / 合并成一张）：")))
        (buf   (current-buffer)))
    (when (string-empty-p raw) (user-error "Buffer is empty"))
    (message "org-sm: asking AI to split into topic cards...")
    (org-sm-gptel--split-request
     raw extra
     (lambda (cards err)
       (cond
        (err (message "org-sm AI capture failed: %s" err))
        ((null cards) (message "org-sm AI capture: no cards parsed"))
        ((buffer-live-p buf)
         (with-current-buffer buf (org-sm-capture-fill-cards cards))
         (message "org-sm: AI split into %d card%s — edit, then C-c C-c"
                  (length cards) (if (= (length cards) 1) "" "s"))))))))

(defun org-sm-gptel--card-ai (arg)
  "AI action for `org-sm-card-workbench' buffers: refine + optional split.
ARG, when non-nil, prompts for a one-off extra instruction.  See
`org-sm-gptel-capture-ai'."
  (let ((raw   (string-trim (buffer-substring-no-properties
                             (point-min) (point-max))))
        (extra (and arg (read-string "额外指令：")))
        (type  org-sm-capture-card-type)
        (buf   (current-buffer)))
    (when (string-empty-p raw) (user-error "Buffer is empty"))
    (message "org-sm: asking AI to refine + look for child cards...")
    (org-sm-gptel--card-request
     raw type extra
     (lambda (body cards err)
       (cond
        (err (message "org-sm AI failed: %s" err))
        ((not (buffer-live-p buf)) (message "org-sm AI: buffer gone"))
        (t
         (with-current-buffer buf (org-sm-capture-fill-body-and-cards body cards))
         (message "org-sm: AI refined%s — edit, then C-c C-c"
                  (if cards
                      (format " + split into %d card%s"
                              (length cards) (if (= (length cards) 1) "" "s"))
                    ""))))))))

;;;###autoload
(defun org-sm-gptel-capture-ai (&optional arg)
  "Ask the AI to act on the current `org-sm-capture-mode' buffer, in place.

Dispatches on the buffer-local `org-sm-capture-ai-system':
- plain `org-sm-capture' buffers (nil/`capture') -- split the content into
  independent topic cards (`org-sm-gptel-system-capture').
- `org-sm-card-workbench' buffers (`card') -- ONE AI call both refines the
  leading body text (per the source card's type) and proposes child cards
  \(empty array if none warranted; see `org-sm-gptel-system-split'),
  re-rendered as leading text + `**' headings.

Either way replaces the buffer content for you to edit and commit with
\\`C-c C-c' (writes back the leading text and/or the `**' cards, whichever
are present).  Runs asynchronously.  Call again to re-run; with prefix
ARG, prompt for a one-off extra instruction (e.g. split finer / merge into
one,
or a hint for the refine)."
  (interactive "P")
  (if (eq org-sm-capture-ai-system 'card)
      (org-sm-gptel--card-ai arg)
    (org-sm-gptel--plain-capture-ai arg)))

;; Wire the AI actions into the core capture workbenches when gptel is present.
(with-eval-after-load 'org-sm
  (when (boundp 'org-sm-capture-mode-map)
    (define-key org-sm-capture-mode-map (kbd "C-c C-a") #'org-sm-gptel-capture-ai)
    (define-key org-sm-capture-mode-map (kbd "C-c C-i") #'org-sm-gptel-refine-card)))

(provide 'org-sm-gptel)
;;; org-sm-gptel.el ends here
