;;; org-sm-weread.el --- WeRead highlights as org-sm topics -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin adapter over WeRead's agent gateway.  Converts bookmarks into
;; highlight plists and delegates deduplication, `:SOURCE:' provenance,
;; `[#C]' priority, and due-date spreading to `org-sm-import-highlights'.

;;; Code:
(require 'org-sm)
(require 'org-sm-import)
(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)
(require 'subr-x)
(require 'auth-source)

(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defcustom org-sm-weread-group 'book+chapter
  "Where imported notes are filed under `org-sm-capture-olp'.
Book and chapter headings are created on first use and reused afterwards,
so they appear in import order rather than in the book's own order."
  :type '(choice (const :tag "One flat inbox" none)
                 (const :tag "Per book" book)
                 (const :tag "Per book and chapter" book+chapter))
  :group 'org-sm)

(defun org-sm-weread--api-key ()
  "Read the WeRead API key from auth-source."
  (let ((secret (plist-get
                 (car (auth-source-search
                       :host "i.weread.qq.com"
                       :user "apikey"
                       :require '(:secret)))
                 :secret)))
    (cond
     ((functionp secret) (funcall secret))
     ((stringp secret) secret)
     (t (user-error "No WeRead API key found in auth-source")))))

(defun org-sm-weread--request (api &rest params)
  "Call gateway API with flat PARAMS; reject errors before returning data."
  (let* ((key (org-sm-weread--api-key))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " key))
            ("Content-Type" . "application/json")))
         (url-request-data
          (encode-coding-string
           (json-encode (append `((api_name . ,api) (skill_version . "1.0.4"))
                                params)) 'utf-8))
         buffer)
    (unless (org-string-nw-p key) (user-error "Set WEREAD_API_KEY in Emacs"))
    (setq buffer (url-retrieve-synchronously
                  "https://i.weread.qq.com/api/agent/gateway" t t 60))
    (unless buffer (error "WeRead request timed out"))
    (unwind-protect
        (with-current-buffer buffer
          (unless (and (integerp url-http-response-status)
                       (<= 200 url-http-response-status 299))
            (error "WeRead HTTP error: %s" url-http-response-status))
          (goto-char (or url-http-end-of-headers
                         (error "WeRead response has no headers")))
          (let ((data (json-parse-buffer :object-type 'alist :array-type 'list
                                         :null-object nil :false-object nil)))
            (when-let* ((upgrade (alist-get 'upgrade_info data)))
              (error "WeRead upgrade required: %s" (alist-get 'message upgrade)))
            (unless (member (alist-get 'errcode data) '(nil 0))
              (error "WeRead API error %s: %s" (alist-get 'errcode data)
                     (alist-get 'errmsg data)))
            data))
      (kill-buffer buffer))))

(defun org-sm-weread--pages (api field cursor-field &rest params)
  "Collect FIELD from API pages; reject non-advancing CURSOR-FIELD."
  (let (cursor seen items done)
    (while (not done)
      (let* ((page (apply #'org-sm-weread--request api
                          (append params '((count . 100))
                                  (when cursor (list (cons cursor-field cursor))))))
             (rows (alist-get field page)))
        (setq items (append items rows))
        (if (not (member (alist-get 'hasMore page) '(1 t)))
            (setq done t)
          (let ((next (if (eq cursor-field 'lastSort)
                          (alist-get 'sort (car (last rows)))
                        (alist-get cursor-field page))))
            (when (or (null rows) (null next) (equal next cursor) (member next seen))
              (error "WeRead pagination stalled: %s" api))
            (push next seen)
            (setq cursor next)))))
    items))

(defun org-sm-weread--key (book-id bookmark-id)
  "Build a stable bookmark key, refusing missing IDs."
  (unless (and (org-string-nw-p (format "%s" (or book-id "")))
               (org-string-nw-p (format "%s" (or bookmark-id ""))))
    (error "WeRead bookmark has no ID"))
  (format "bookmark/%s/%s" book-id bookmark-id))

(defun org-sm-weread--chapter (note chapters)
  "Return NOTE's chapter title from CHAPTERS, or nil for a book-level note."
  (let ((chapter (cl-find (alist-get 'chapterUid note) chapters
                          :key (lambda (c) (alist-get 'chapterUid c))
                          :test #'equal)))
    (or (alist-get 'title chapter) (alist-get 'chapterName note))))

(defun org-sm-weread--olp (book chapter)
  "Return the capture outline path for BOOK and CHAPTER."
  (append org-sm-capture-olp
          (when (memq org-sm-weread-group '(book book+chapter))
            (list (or (org-sm-import-heading (alist-get 'title book)) "未知书籍")))
          (when (eq org-sm-weread-group 'book+chapter)
            (when-let* ((title (org-sm-import-heading chapter))) (list title)))))

(defun org-sm-weread--source (book note chapter key)
  "Build `:SOURCE:' link carrying KEY as its fragment or target for dedupe."
  (let* ((title (org-sm-import-heading (alist-get 'title book)))
         (author (org-sm-import-heading (alist-get 'author book)))
         (chap (org-sm-import-heading chapter))
         (deep (or (org-string-nw-p (alist-get 'deepLink note))
                   (org-string-nw-p (alist-get 'deepLink book))))
         (link (if deep (concat deep "#" key) (concat "weread:" key)))
         (label (string-join
                 (delq nil (list (or title "微信读书")
                                 (and author (concat "/ " author))
                                 (and chap (concat "§ " chap))))
                 " ")))
    (org-link-make-string link label)))

(defun org-sm-weread--book-items (entry)
  "Fetch highlight plists for book ENTRY."
  (let* ((book (or (alist-get 'book entry) entry))
         (book-id (or (alist-get 'bookId entry) (alist-get 'bookId book)))
         (data (org-sm-weread--request "/book/bookmarklist" (cons 'bookId book-id)))
         (marks (alist-get 'updated data))
         (chapters (alist-get 'chapters data)))
    (mapcar (lambda (mark)
              (let ((key (org-sm-weread--key book-id (alist-get 'bookmarkId mark)))
                    (chapter (org-sm-weread--chapter mark chapters)))
                (list :key key
                      :text (alist-get 'markText mark)
                      :olp (org-sm-weread--olp book chapter)
                      :source (org-sm-weread--source book mark chapter key))))
            marks)))

;;;###autoload
(defun org-sm-weread-import ()
  "Choose one book or all notebooks and import new highlights into the inbox."
  (interactive)
  (let* ((books (org-sm-weread--pages "/user/notebooks" 'books 'lastSort))
         (choices (mapcar (lambda (book)
                            (cons (format "%s [%s]"
                                          (alist-get 'title (alist-get 'book book))
                                          (alist-get 'bookId book))
                                  book))
                          books))
         (selection (completing-read "WeRead: "
                                     (cons "ALL_BOOK" (mapcar #'car choices)) nil t))
         (chosen (if (equal selection "ALL_BOOK") books
                   (list (cdr (assoc selection choices)))))
         (items (mapcan #'org-sm-weread--book-items chosen))
         (created (org-sm-import-highlights items)))
    (message "WeRead: 新增 %d 张卡片（共扫描 %d 条划线，已有 :SOURCE: 跳过）"
             created (length items))))

(provide 'org-sm-weread)
;;; org-sm-weread.el ends here
