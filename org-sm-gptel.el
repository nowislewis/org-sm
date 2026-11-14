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
;;                            on, with the surrounding text and the source
;;                            page as context.  Persistent side-window chat.
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
(defvar gptel-system-prompt)

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

可以用工具查资料（若具备 Skill / Read）：
- 遇到领域知识，先读对应的书 skill（例如 ai-processor-architecture）核对，不凭空猜。
- 只读不写。可以用 Skill、Read、Grep 查资料，不用 Edit、Write 改文件。

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
   让用户自己去找原因，你不代答。"
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

(defcustom org-sm-gptel-system-simplify
  "你把一段正文精简到只剩必须记住的核心，删得比 refine 更多。

1. 只保留结论、核心事实和关键因果链。删掉背景铺垫、重复的表述，以及不支撑结论的例子。
2. 可以合并同类项，可以删次要的限定语。因果链本身（为什么）必须保留，只删多余的解释文字。
3. 篇幅一般是原文的三分之一到一半。
4. 只删减与合并，不扩写，不引入原文没有的事实或结论。
5. 输出仍是完整可读的正文，不是关键词列表。"
  "System prompt for the `simplify' `org-sm-gptel-card-ai' mode.
More aggressive than `org-sm-gptel-system-topic'/`-cloze': cuts length, not
just noise."
  :type 'string :group 'org-sm-gptel)

(defcustom org-sm-gptel-system-suggest
  "你是 SuperMemo 渐进阅读专家。用户正在读一段较长的文字，想知道哪些地方值得摘录成独立的阅读卡。

任务：从给定原文中挑出 0 到 3 段最值得单独摘录的文字。
挑选优先级：作者的核心主张/结论 > 关键定义 > 关键因果链（为什么）。细节、例子、铺垫不挑。

硬约束：
1. quote 必须逐字复制原文中连续的一段（一到三句），不改一个字、不加省略号、不拼接。
   程序会丢弃对不上原文的引文。
2. why 用一句话说明它为什么值得单独复习，不复述原文。
3. 宁缺毋滥：没有值得摘的就返回空数组。"
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
            (concat "\n\n子卡标记规则（picks，0 到 3 条，宁缺毋滥）："
                    "\n- 如果改写后的 body 包含两层以上独立要点，把最值得单独摘成子卡的段落放进 picks。"
                    "\n- quote 必须逐字复制改写后 body 里连续的一段（一到三句），不改一个字。"
                    "\n- why 用一句话说明为什么值得单独复习。"
                    "\n- 如果整段只讲一件事，picks 返回空数组 []。"))))

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

(defun org-sm-gptel--passage ()
  "Return (PASSAGE . CONTEXT) for the text the reader is stuck on.
PASSAGE is the region, else the paragraph at point; CONTEXT is up to
`org-sm-gptel-explain-context' characters of the card body around it."
  (let* ((span (if (use-region-p)
                   (cons (region-beginning) (region-end))
                 (org-sm-paragraph-bounds)))
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

(defun org-sm-gptel--open-chat (kind system message)
  "Open the KIND chat for the current card and send MESSAGE to it.
SYSTEM is the system prompt.  One chat per card, reused across calls, so
a second question about another paragraph continues the conversation
instead of starting over."
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
      ;; neighbourhood, its origin and its page.  Attaching the card's file
      ;; would resend the whole note -- 200 KB for the inbox -- every turn.
      (setq-local gptel-system-prompt system
                  gptel-context nil)
      (if (org-sm-gptel--draft-p)
          (message "org-sm: 聊天里有未发送的草稿，先发送或删除它")
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert message)
        (gptel-send)))
    (display-buffer chat-buf
                    '(display-buffer-in-side-window
                      (side . right) (window-width . 0.45)))))

;;;; ---- Commands ------------------------------------------------------------

(defun org-sm-gptel--source-quote ()
  "Return the page text behind the `page:' link in the current card, or nil.
A card extracted from a PDF keeps only the sentence that mattered; the
reasoning around it stayed on the page."
  (when-let* ((link (org-sm-source-page-link (org-sm-body-text)))
              (text (org-sm-source-page-text (car link) (cdr link))))
    (truncate-string-to-width
     (replace-regexp-in-string "\n\\{3,\\}" "\n\n" text) 4000 nil nil "…")))

(defun org-sm-gptel--origin-block (origin)
  "Return ORIGIN, from `org-sm-extract-origin', as a labelled message block.
nil when ORIGIN is nil, so callers can `concat' it unconditionally."
  (when origin
    (concat "#+begin_example\n" (car origin) "\n【……这张卡摘自这里……】\n"
            (cdr origin) "\n#+end_example")))

;;;###autoload
(defun org-sm-gptel-explain ()
  "Explain the paragraph at point, or the region, in this card's chat.
Sends the passage itself, the text around it, the parent text this card
was extracted from and, when the card has a `page:' link, that page of the
source: the step the passage skips is usually in the neighbourhood, not in
the passage.  Applies
`org-sm-gptel-agent-preset' so the AI can consult book skills."
  (interactive)
  (pcase-let ((`(,passage . ,context) (org-sm-gptel--passage))
              (origin (org-sm-gptel--origin-block (org-sm-extract-origin)))
              (page (ignore-errors (org-sm-gptel--source-quote))))
    (org-sm-gptel--open-chat
     "explain" (concat org-sm-gptel-system-explain "\n\n" org-sm-gptel-style)
     (concat
      "我读到这一段卡住了，请按三层极简解释（重点②③，①一句带过，不要制卡、不要总结）：\n\n"
      "#+begin_quote\n" passage "\n#+end_quote\n\n"
      "【前后文，供理解，不必解释】\n#+begin_example\n" context "\n#+end_example"
      (when origin (concat "\n\n【这张卡在原文里的位置】\n" origin))
      (when page
        (concat "\n\n【原书那一页】\n#+begin_example\n" page "\n#+end_example"))))))

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
  (when (stringp response)
    (let ((data (ignore-errors
                  (json-parse-string response :object-type 'plist
                                     :array-type 'list))))
      (delq nil (mapcar (lambda (p)
                          (let ((q (plist-get p :quote)) (w (plist-get p :why)))
                            (and (stringp q) (stringp w) (cons q w))))
                        (plist-get data :picks))))))

(defun org-sm-gptel--draw-picks (picks beg end)
  "Underline PICKS found between BEG and END; return how many were drawn.
A pick whose quote is not in the text is dropped: a suggestion must point
at something the reader can see."
  (org-sm-pencils-clear beg end)
  (let ((n 0))
    (dolist (pick picks)
      (when-let* ((span (org-sm-gptel--locate (car pick) beg end)))
        (org-sm-pencil-add (car span) (cdr span) (cdr pick))
        (setq n (1+ n))))
    n))

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
    (let ((gptel-use-context nil)
          (gptel-use-tools nil))
      (gptel-request text
        :system org-sm-gptel-system-suggest
        :schema org-sm-gptel--suggest-schema
        :transforms nil
        :callback
        (lambda (response info)
          (cond
           ((not (buffer-live-p buf)) nil)
           ((not (stringp response))
            (message "org-sm AI failed: %s" (plist-get info :status)))
           (t
            (with-current-buffer buf
              (let ((n (org-sm-gptel--draw-picks
                        (org-sm-gptel--parse-picks response) beg end)))
                (if (zerop n)
                    (message "org-sm: 这一屏没有值得单独摘的")
                  (goto-char beg)
                  (org-sm-pencil-next)))))))))))

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
  (when (stringp response)
    (let* ((data (ignore-errors
                   (json-parse-string response :object-type 'plist
                                      :array-type 'list)))
           (body (plist-get data :body)))
      (when (and (stringp body) (org-string-nw-p body))
        (cons (string-trim body) (org-sm-gptel--parse-picks response))))))

(defun org-sm-gptel--card-request (text type mode custom callback)
  "Ask the AI to rewrite TEXT per MODE and call CALLBACK with the result.
TYPE and MODE/CUSTOM select the system prompt (see
`org-sm-gptel--card-system').  CALLBACK receives (BODY PICKS ERR).

Context is bound here rather than inherited: with added context and agent
tools off and no prompt transforms, this stays one plain text-in, JSON-out
request that never picks up whatever the user attached elsewhere."
  (let ((gptel-use-context nil)
        (gptel-use-tools nil))
    (gptel-request text
      :system (org-sm-gptel--card-system type mode custom)
      :schema org-sm-gptel--card-schema
      :transforms nil
      :callback
      (lambda (response info)
        (if-let* ((parsed (org-sm-gptel--parse-card-response response)))
            (funcall callback (car parsed) (cdr parsed) nil)
          (funcall callback nil nil (if (stringp response) "parse failed"
                                      (format "%s" (plist-get info :status)))))))))

;;;###autoload
(defun org-sm-gptel-card-ai ()
  "Rewrite the body in this card workbench with the AI, underlining child picks.
Choose refine (clean up, keep every causal link, underline 0-3 passages worth
extracting), 简化 (cut to the core) or a custom instruction.  An extract also
sends the parent text around it, so \"it\" and \"these neurons\" can be
resolved from the source rather than guessed.  Nothing reaches the source
card until \\<org-sm-capture-mode-map>\\[org-sm-capture-commit], which writes
the body back and extracts the surviving underlines in place."
  (interactive)
  (org-sm-gptel--workbench-only)
  (let* ((label  (completing-read "AI 动作：" (mapcar #'car org-sm-gptel--card-modes)
                                  nil t))
         (mode   (or (cdr (assoc label org-sm-gptel--card-modes)) 'custom))
         (custom (and (eq mode 'custom) (read-string "提示词："))))
    (when (and (eq mode 'custom) (string-blank-p (or custom "")))
      (user-error "Custom mode needs a prompt"))
    (let* ((bounds (org-sm-capture-preamble-bounds))
           (raw    (string-trim (buffer-substring-no-properties
                                 (car bounds) (cdr bounds))))
           (origin (org-sm-gptel--origin-block
                    (when-let* ((card (org-sm--capture-live-marker)))
                      (org-with-point-at card (org-sm-extract-origin)))))
           (buf    (current-buffer))
           (tick   (buffer-chars-modified-tick))
           (serial (cl-incf org-sm-gptel--request-serial)))
      (when (string-empty-p raw) (user-error "No body text to rewrite"))
      (message "org-sm: asking AI (%s)..." label)
      (org-sm-gptel--card-request
       (if origin
           (concat raw "\n\n【出处上下文：只用来补全正文里的代词和省略的主语，"
                   "其中的其他内容不要写进正文】\n" origin)
         raw)
       org-sm-capture-card-type mode custom
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
             (org-sm-capture-set-preamble body)
             (let ((n (org-sm-gptel--draw-picks picks (point-min) (point-max))))
               (if (> n 0)
                   (progn (goto-char (point-min)) (org-sm-pencil-next))
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
