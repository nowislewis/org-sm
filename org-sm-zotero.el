;;; org-sm-zotero.el --- Zotero annotations as org-sm topics -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin read-only client for Zotero 10's built-in Local HTTP API
;; (`http://127.0.0.1:23119/api').  Converts PDF/EPUB highlights and commented
;; image annotations into highlight plists and delegates deduplication,
;; `:SOURCE:' provenance, `[#C]' priority, and due-date spreading to
;; `org-sm-import-highlights'.

;;; Code:

(require 'org-sm)
(require 'org-sm-import)
(require 'ol)
(require 'url)
(require 'url-http)
(require 'url-util)
(require 'json)
(require 'cl-lib)
(require 'subr-x)

(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defcustom org-sm-zotero-api-url "http://127.0.0.1:23119/api"
  "Base URL of Zotero's local HTTP API."
  :type 'string
  :group 'org-sm)

(defcustom org-sm-zotero-user-prefix "/users/0"
  "Library path prefix for Zotero Local API requests (`/users/0' is personal)."
  :type 'string
  :group 'org-sm)

(defcustom org-sm-zotero-group 'book
  "How cards are filed below `org-sm-capture-olp'."
  :type '(choice (const :tag "Inbox only" none)
                 (const :tag "Per book" book))
  :group 'org-sm)

(defcustom org-sm-zotero-min-highlight-length 3
  "Ignore text highlights shorter than this many characters."
  :type 'integer
  :group 'org-sm)

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

(defun org-sm-zotero--authors (data meta)
  "Extract author string from item DATA `:creators' or META `:creatorSummary'."
  (let* ((creators (plist-get data :creators))
         (authors
          (cl-loop for c in creators
                   when (equal (plist-get c :creatorType) "author")
                   collect (or (org-sm-import-heading (plist-get c :name))
                               (org-sm-import-heading
                                (concat (or (plist-get c :firstName) "") " "
                                        (or (plist-get c :lastName) "")))))))
    (or (org-sm-import-heading (string-join (delq nil authors) "; "))
        (org-sm-import-heading (plist-get meta :creatorSummary)))))

(defun org-sm-zotero--books ()
  "Fetch annotated books on demand, ordered by latest annotation timestamp."
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
                                             :author (org-sm-zotero--authors bd bm)
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
                                     :comment (org-sm-import-heading
                                               (plist-get adata :annotationComment))
                                     :page (org-sm-import-heading
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

(defun org-sm-zotero--source (book src)
  "Build `:SOURCE:' link for BOOK and annotation SRC."
  (let* ((att (or (org-sm-import-heading (plist-get src :attachment))
                  (plist-get book :key)))
         (key (org-sm-import-heading (plist-get src :key)))
         (page (org-sm-import-heading (plist-get src :page)))
         (title (or (org-sm-import-heading (plist-get book :title)) "未命名"))
         (author (org-sm-import-heading (plist-get book :author)))
         (link (if page
                   (format "zotero://open-pdf/library/items/%s?page=%s&annotation=%s"
                           att (url-hexify-string page) key)
                 (format "zotero://open-pdf/library/items/%s?annotation=%s" att key)))
         (label (string-join
                 (delq nil (list (format "《%s》" title)
                                 (and author (concat "/ " author))
                                 (and page (format "p.%s" page))))
                 " ")))
    (org-link-make-string link label)))

(defun org-sm-zotero--book-items (book)
  "Convert BOOK's annotations into highlight plists for `org-sm-import-highlights'."
  (let ((olp (append org-sm-capture-olp
                     (unless (eq org-sm-zotero-group 'none)
                       (list (or (org-sm-import-heading (plist-get book :title))
                                 "未命名"))))))
    (delq nil
          (mapcar (lambda (src)
                    (let* ((raw (string-trim (or (plist-get src :text) "")))
                           (comment (plist-get src :comment))
                           (text (cond
                                  ((>= (length raw) org-sm-zotero-min-highlight-length)
                                   raw)
                                  ((and (equal (plist-get src :type) "image") comment)
                                   (concat "[图片批注] " comment)))))
                      (when text
                        (list :key (plist-get src :key)
                              :text text
                              :olp olp
                              :source (org-sm-zotero--source book src)))))
                  (plist-get book :sources)))))

;;;###autoload
(defun org-sm-zotero-import ()
  "Pick books and import their new Zotero annotations as `[#C]' topic cards."
  (interactive)
  (let* ((books (org-sm-zotero--books))
         (table (mapcar (lambda (it)
                          (cons (format "%-52s [%s] %d标注"
                                        (truncate-string-to-width
                                         (or (plist-get it :title) "") 52)
                                        (plist-get it :key) (plist-get it :annos))
                                it))
                        books))
         (chosen (cond ((null books) (user-error "Zotero 中没有含标注的条目"))
                       ((= (length books) 1) books)
                       (t (let ((sel (completing-read-multiple
                                      "Zotero 条目（可多选）: "
                                      (cons "ALL" (mapcar #'car table)) nil t)))
                            (if (member "ALL" sel) books
                              (delq nil (mapcar (lambda (s)
                                                  (cdr (cl-assoc s table :test #'equal)))
                                                sel)))))))
         (items (mapcan #'org-sm-zotero--book-items chosen))
         (created (org-sm-import-highlights items)))
    (message "Zotero: 新增 %d 张卡片（共扫描 %d 条标注，已有 :SOURCE: 跳过）"
             created (length items))))

(provide 'org-sm-zotero)
;;; org-sm-zotero.el ends here
