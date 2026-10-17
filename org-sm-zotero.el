;;; org-sm-zotero.el --- Zotero annotations as org-sm topics -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-only client for Zotero 10's built-in Local HTTP API
;; (`http://127.0.0.1:23119/api').  Imports PDF/EPUB highlights and commented
;; image/area selections into `org-sm' topic cards, filed per book and ordered
;; by Zotero's reading `annotationSortIndex'.
;;
;; Requires Zotero desktop running with Settings -> Advanced ->
;; "Allow other applications on this computer to communicate with Zotero".

;;; Code:

(require 'org-sm)
(require 'ol)
(require 'url)
(require 'url-http)
(require 'url-util)
(require 'json)
(require 'cl-lib)
(require 'subr-x)

(defvar url-http-response-status)
(defvar url-http-end-of-headers)

;;;###autoload
(defun org-sm-zotero-open-link (path _)
  "Open a `zotero:' link PATH in the desktop Zotero client without fuzzy fallback."
  (let ((url (concat "zotero:" path)))
    (if (executable-find "xdg-open")
        (start-process "zotero-open" nil "xdg-open" url)
      (browse-url url))))

;;;###autoload
(with-eval-after-load 'ol
  (org-link-set-parameters "zotero" :follow #'org-sm-zotero-open-link))

;;;; ---- Customization -------------------------------------------------------

(defcustom org-sm-zotero-api-url "http://127.0.0.1:23119/api"
  "Base URL of Zotero's local HTTP API."
  :type 'string
  :group 'org-sm)

(defcustom org-sm-zotero-user-prefix "/users/0"
  "Library path prefix for Zotero Local API requests (`/users/0' is personal)."
  :type 'string
  :group 'org-sm)

(defcustom org-sm-zotero-group 'book
  "How cards are filed below `org-sm-capture-olp'.
book  /sm-cards/<title>
none  straight into `org-sm-capture-olp'"
  :type '(choice (const :tag "Inbox only" none)
                 (const :tag "Per book" book))
  :group 'org-sm)

(defcustom org-sm-zotero-min-highlight-length 3
  "Ignore text highlights shorter than this many characters."
  :type 'integer
  :group 'org-sm)

(defcustom org-sm-zotero-color-rules
  '(("#ff6666" . ?A)
    ("#ff0000" . ?A)
    ("#aaaaaa" . ignore))
  "Alist mapping Zotero annotation hex colors (lowercase) to import actions.
Each value is one of:
  `ignore'          — keep in PDF/EPUB only; skip during `org-sm-zotero-import'
  ?A / ?B / ?C      — import as topic card and set that Org priority cookie
  nil (or unmapped) — import as a topic card with default priority."
  :type '(alist :key-type string
                :value-type (choice (const :tag "Default priority" nil)
                                    (const :tag "Priority [#A]" ?A)
                                    (const :tag "Priority [#B]" ?B)
                                    (const :tag "Priority [#C]" ?C)
                                    (const :tag "Ignore (mark only)" ignore)))
  :group 'org-sm)

(defun org-sm-zotero--color-action (color)
  "Return configured import action (`ignore', ?A/?B/?C, or nil) for hex COLOR."
  (when (stringp color)
    (cdr (assoc (downcase (string-trim color)) org-sm-zotero-color-rules))))

;;;; ---- Local HTTP API client -----------------------------------------------

(defun org-sm-zotero--get (path)
  "Perform GET on PATH under `org-sm-zotero-api-url' and return parsed JSON."
  (let* ((url (concat (string-remove-suffix "/" org-sm-zotero-api-url) path))
         (url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (buf (condition-case err
                  (url-retrieve-synchronously url t t 60)
                (error (user-error "无法连接 Zotero 本地 API (%s)，请确认 Zotero 已启动: %s"
                                   org-sm-zotero-api-url (error-message-string err))))))
    (unless buf
      (user-error "Zotero 本地 API 请求超时 (%s)" url))
    (unwind-protect
        (with-current-buffer buf
          (set-buffer-multibyte t)
          (let ((status (or url-http-response-status 0)))
            (cond
             ((= status 403)
              (user-error "Zotero 本地 API 返回 403：请在 Zotero 设置 -> 高级 中勾选“允许其他应用程序与此计算机通信”"))
             ((not (<= 200 status 299))
              (error "Zotero GET %s failed (HTTP %s)" path status))
             (url-http-end-of-headers
              (goto-char (1+ url-http-end-of-headers))
              (json-parse-buffer :object-type 'plist
                                 :array-type 'list
                                 :null-object nil
                                 :false-object nil)))))
      (kill-buffer buf))))

(defun org-sm-zotero--get-by-keys (keys)
  "Fetch Zotero item objects for KEYS in batches of 50; return key -> object hash."
  (let ((table (make-hash-table :test #'equal))
        (rem (delete-dups (copy-sequence keys)))
        (prefix org-sm-zotero-user-prefix))
    (while rem
      (let ((batch (seq-take rem 50)))
        (setq rem (seq-drop rem 50))
        (dolist (obj (org-sm-zotero--get
                      (concat prefix "/items?itemKey=" (string-join batch ","))))
          (puthash (plist-get obj :key) obj table))))
    table))

;;;; ---- Text & API object helpers -------------------------------------------

(defun org-sm-zotero--one-line (text)
  "Collapse TEXT to one trimmed line safe for Org headings; nil when blank."
  (and (stringp text)
       (org-string-nw-p
        (replace-regexp-in-string "\\`[ \t*]+\\|[ \t:]+\\'" ""
         (replace-regexp-in-string "[ \t\n\r]+" " " text)))))

(defun org-sm-zotero--safe (text)
  "Escape Org structural line-starts in remote TEXT without rewriting prose."
  (replace-regexp-in-string "^\\(\\*+ \\|#+\\|:[[:alnum:]_]+:\\|[[<]\\)" ",\\1" (or text "")))

(defun org-sm-zotero--extract-authors (data meta)
  "Extract author string from item DATA `:creators' or META `:creatorSummary'."
  (let* ((creators (plist-get data :creators))
         (authors
          (cl-loop for c in creators
                   when (equal (plist-get c :creatorType) "author")
                   collect (if-let* ((name (org-sm-zotero--one-line (plist-get c :name))))
                               name
                             (string-trim
                              (concat (or (plist-get c :firstName) "") " "
                                      (or (plist-get c :lastName) "")))))))
    (or (org-sm-zotero--one-line (string-join (delq nil authors) "; "))
        (org-sm-zotero--one-line (plist-get meta :creatorSummary)))))

(defun org-sm-zotero--items ()
  "Fetch annotated books on demand without scanning unannotated library items."
  (when-let* ((raw-annos (org-sm-zotero--get
                          (concat org-sm-zotero-user-prefix "/items?itemType=annotation"))))
    (let* ((att-keys (cl-loop for a in raw-annos
                              for k = (plist-get (plist-get a :data) :parentItem)
                              when (stringp k) collect k))
           (atts (org-sm-zotero--get-by-keys att-keys))
           (book-keys (cl-loop for a in raw-annos
                               for att-k = (plist-get (plist-get a :data) :parentItem)
                               for att = (and att-k (gethash att-k atts))
                               for bk = (or (and att (plist-get (plist-get att :data) :parentItem))
                                            att-k)
                               when (stringp bk) collect bk))
           (raw-books (org-sm-zotero--get-by-keys book-keys))
           (books (make-hash-table :test #'equal))
           result)
      (dolist (a raw-annos)
        (let* ((adata (plist-get a :data))
               (att-key (plist-get adata :parentItem))
               (att-obj (and att-key (gethash att-key atts)))
               (bkey (or (and att-obj (plist-get (plist-get att-obj :data) :parentItem))
                         att-key))
               (bobj (and bkey (or (gethash bkey raw-books) att-obj))))
          (when bobj
            (let ((book (or (gethash bkey books)
                            (let ((bd (plist-get bobj :data))
                                  (bm (plist-get bobj :meta)))
                              (puthash bkey
                                       (list :key bkey
                                             :title (plist-get bd :title)
                                             :author (org-sm-zotero--extract-authors bd bm)
                                             :annos 0
                                             :latest ""
                                             :sources nil)
                                       books))))
                  (ts (or (plist-get adata :dateModified)
                          (plist-get adata :dateAdded) "")))
              (plist-put book :annos (1+ (plist-get book :annos)))
              (when (string< (plist-get book :latest) ts)
                (plist-put book :latest ts))
              (plist-put book :sources
                         (cons (list :key (plist-get a :key)
                                     :type (plist-get adata :annotationType)
                                     :attachment att-key
                                     :text (plist-get adata :annotationText)
                                     :comment (org-sm-zotero--one-line
                                               (plist-get adata :annotationComment))
                                     :color (plist-get adata :annotationColor)
                                     :page (org-sm-zotero--one-line
                                            (plist-get adata :annotationPageLabel))
                                     :sort-index (or (plist-get adata :annotationSortIndex) ""))
                               (plist-get book :sources)))))))
      (maphash
       (lambda (_k book)
         (plist-put book :sources
                    (sort (plist-get book :sources)
                          (lambda (x y)
                            (string< (plist-get x :sort-index)
                                     (plist-get y :sort-index)))))
         (push book result))
       books)
      (sort result (lambda (a b)
                     (string> (plist-get a :latest) (plist-get b :latest)))))))

;;;; ---- Rendering -----------------------------------------------------------

(defun org-sm-zotero--olp (item)
  "Outline path for ITEM, per `org-sm-zotero-group'."
  (append org-sm-capture-olp
          (unless (eq org-sm-zotero-group 'none)
            (list (or (org-sm-zotero--one-line (plist-get item :title)) "未命名")))))

(defun org-sm-zotero--link (source)
  "Build a `zotero://' deep link for annotation SOURCE, or nil."
  (when-let* ((att (org-sm-zotero--one-line (plist-get source :attachment)))
              (key (org-sm-zotero--one-line (plist-get source :key))))
    (if-let* ((page (org-sm-zotero--one-line (plist-get source :page))))
        (format "zotero://open-pdf/library/items/%s?page=%s&annotation=%s"
                att (url-hexify-string page) key)
      (format "zotero://open-pdf/library/items/%s?annotation=%s" att key))))

(defun org-sm-zotero--footer (item author &optional source)
  "Minimal provenance trailer for ITEM (AUTHOR) and SOURCE."
  (let ((page (and source (org-sm-zotero--one-line (plist-get source :page))))
        (link (and source (org-sm-zotero--link source))))
    (concat "\n\n来源：" (org-sm-zotero--safe (or (plist-get item :title) "未命名"))
            (when (org-string-nw-p author) (concat " / " (org-sm-zotero--safe author)))
            (when page (concat "，第 " (org-sm-zotero--safe page) " 页"))
            (when link (concat "\n[[" link "][在 Zotero 中打开]]")))))

(defun org-sm-zotero--body (item author source)
  "Card body for SOURCE on ITEM with AUTHOR; nil when unusable.
Supports both text highlights and commented image/area annotations."
  (let* ((raw-text (string-trim (or (plist-get source :text) "")))
         (comment (org-sm-zotero--one-line (plist-get source :comment)))
         (image-p (equal (plist-get source :type) "image")))
    (cond
     ((>= (length raw-text) org-sm-zotero-min-highlight-length)
      (concat (org-sm-zotero--safe raw-text)
              (when comment (concat "\n\n批注：" comment))
              (org-sm-zotero--footer item author source)))
     ((and image-p comment)
      (concat "[图片批注] " comment
              (org-sm-zotero--footer item author source))))))

;;;; ---- Dedup and card creation ---------------------------------------------

(defun org-sm-zotero--key (_item source)
  "Stable source id for SOURCE: annotation key alone (immune to book merges)."
  (plist-get source :key))

(defun org-sm-zotero--index ()
  "Map existing `ZOTERO_KEY' values -> heading markers across inbox and vault.
Normalizes legacy `anno/<bookKey>/<annoKey>' values to `<annoKey>'."
  (let ((index (make-hash-table :test #'equal))
        (files (delete-dups
                (cons (org-sm-capture-target-file)
                      (when (and org-sm-directory (file-directory-p org-sm-directory))
                        (directory-files-recursively org-sm-directory "\\.org\\'"))))))
    (dolist (file files)
      (when (or (file-exists-p file) (get-file-buffer file))
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward org-heading-regexp nil t)
             (when-let* ((k (org-entry-get nil "ZOTERO_KEY"))
                         (norm (car (last (split-string k "/" t)))))
               (puthash norm (copy-marker (line-beginning-position)) index)))))))
    index))

(defun org-sm-zotero--create (body key index olp &optional has-comment priority)
  "Capture BODY under OLP as a topic card, stamp KEY, and update INDEX."
  (save-current-buffer
    (let* ((id (org-sm-capture-card 'topic body nil olp))
           (pos (or (org-find-entry-with-id id)
                    (error "org-sm-zotero: captured card %s vanished" id))))
      (org-with-wide-buffer
       (goto-char pos)
       (when (characterp priority)
         (org-priority priority)
         (goto-char pos))
       (org-entry-put nil "ZOTERO_KEY" key)
       (when has-comment (org-entry-put nil "ZOTERO_COMMENT" "t"))
       (save-buffer)
       (puthash key (copy-marker pos) index)))))

(defun org-sm-zotero--append-comment (marker comment)
  "Append late Zotero COMMENT to existing card at MARKER without rescheduling."
  (when-let* ((text (org-sm-zotero--one-line comment)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (let ((bounds (org-sm-body-bounds)))
         (unless (or (org-entry-get nil "ZOTERO_COMMENT")
                     (save-excursion
                       (goto-char (car bounds))
                       (re-search-forward "^批注：" (cdr bounds) t)))
           (goto-char (car bounds))
           (if (re-search-forward "\n\n来源：" (cdr bounds) t)
               (progn
                 (goto-char (match-beginning 0))
                 (insert "\n\n批注：" text))
             (goto-char (cdr bounds))
             (insert "\n\n批注：" text "\n"))
           (goto-char marker)
           (org-entry-put nil "ZOTERO_COMMENT" "t")
           (save-buffer)
           t))))))

;;;; ---- Command -------------------------------------------------------------

;;;###autoload
(defun org-sm-zotero-import ()
  "Pick books and import their new Zotero annotations as `org-sm' topic cards."
  (interactive)
  (let* ((items (org-sm-zotero--items))
         (table (mapcar (lambda (it)
                          (cons (format "%-52s [%s] %d标注"
                                        (truncate-string-to-width
                                         (or (plist-get it :title) "") 52)
                                        (plist-get it :key) (plist-get it :annos))
                                it))
                        items))
         (chosen (cond ((null items) (user-error "Zotero 中没有含标注的条目"))
                       ((= (length items) 1) items)
                       (t (let ((sel (completing-read-multiple
                                      "Zotero 条目（可多选）: "
                                      (cons "ALL" (mapcar #'car table)) nil t)))
                            (if (member "ALL" sel) items
                              (delq nil (mapcar (lambda (s)
                                                  (cdr (cl-assoc s table :test #'equal)))
                                                sel)))))))
         (index (org-sm-zotero--index))
         (created 0) (appended 0) (seen 0))
    (dolist (item chosen)
      (let ((author (plist-get item :author))
            (olp (org-sm-zotero--olp item)))
        (dolist (src (plist-get item :sources))
          (cl-incf seen)
          (let* ((key (org-sm-zotero--key item src))
                 (marker (gethash key index))
                 (comment (plist-get src :comment))
                 (action (org-sm-zotero--color-action (plist-get src :color))))
            (if marker
                (when (and comment (org-sm-zotero--append-comment marker comment))
                  (cl-incf appended))
              (unless (eq action 'ignore)
                (when-let* ((body (org-sm-zotero--body item author src)))
                  (org-sm-zotero--create body key index olp
                                         (or comment (equal (plist-get src :type) "image"))
                                         (and (characterp action) action))
                  (cl-incf created))))))))
    (message "Zotero: 新增 %d 张卡片，追加 %d 条批注（扫描 %d 个标注 / %d 本书）"
             created appended seen (length chosen))))

(provide 'org-sm-zotero)
;;; org-sm-zotero.el ends here
