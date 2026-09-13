;;; org-sm-weread.el --- WeRead notes as org-sm topics -*- lexical-binding: t; -*-

;;; Commentary:
;; Import only: source IDs live on cards, never compare or replace their text.
;; Credentials are read from auth-source.  See README.org for scope and limitations.

;;; Code:
(require 'org-sm)
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

(defun org-sm-weread--key (kind book id)
  "Build a source key, refusing missing IDs rather than deduplicating by text."
  (unless (and id (not (equal id ""))) (error "WeRead %s has no ID" kind))
  (format "%s/%s/%s" kind book id))

(defun org-sm-weread--index ()
  "Index source IDs across the vault and inbox, including dismissed cards.
Use live buffers so unsaved edits and refiles are respected."
  (let ((index (make-hash-table :test #'equal))
        (files (delete-dups
                (cons (org-sm--capture-target-file)
                      (when (and org-sm-directory (file-directory-p org-sm-directory))
                        (directory-files-recursively org-sm-directory "\\.org\\'"))))))
    (dolist (file files)
      (when (or (file-exists-p file) (get-file-buffer file))
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward org-heading-regexp nil t)
             (dolist (property '("WEREAD_KEY" "WEREAD_REVIEW_KEY"))
               (when-let* ((key (org-entry-get nil property)))
                 (puthash key (copy-marker (line-beginning-position)) index))))))))
    index))

(defun org-sm-weread--text (text)
  "Escape Org structural lines in remote TEXT without rewriting its prose."
  (replace-regexp-in-string
   "^\\(\\*+ \\|#\\+\\|:[[:alnum:]_]+:\\)" ",\\1" (or text "")))

(defun org-sm-weread--heading (text)
  "Return TEXT as a single-line Org heading, or nil when it is blank.
Remote titles may wrap or end in a `:tag:'-shaped run, either of which would
change the outline structure `org-find-olp' matches against."
  (org-string-nw-p
   (replace-regexp-in-string
    "[ \t:]+\\'" ""
    (replace-regexp-in-string
     "\\`[ \t*]+" ""
     (replace-regexp-in-string "[ \t\n\r]+" " " (or text ""))))))

(defun org-sm-weread--chapter (note chapters)
  "Return NOTE's chapter title from CHAPTERS, or nil for a book-level note."
  (let ((chapter (cl-find (alist-get 'chapterUid note) chapters
                          :key (lambda (c) (alist-get 'chapterUid c))
                          :test #'equal)))
    (or (alist-get 'title chapter) (alist-get 'chapterName note))))

(defun org-sm-weread--olp (book chapter)
  "Return the capture outline path for BOOK and CHAPTER.
Book-level notes and unusable titles simply yield a shorter path instead of
an invented placeholder heading."
  (append org-sm-capture-olp
          (when (memq org-sm-weread-group '(book book+chapter))
            (list (or (org-sm-weread--heading (alist-get 'title book)) "未知书籍")))
          (when (eq org-sm-weread-group 'book+chapter)
            (when-let* ((title (org-sm-weread--heading chapter))) (list title)))))

(defun org-sm-weread--location-equal-p (mark review)
  "Match a review only when explicit chapter and range are both present."
  (and (alist-get 'chapterUid mark) (org-string-nw-p (alist-get 'range mark))
       (equal (alist-get 'chapterUid mark) (alist-get 'chapterUid review))
       (equal (alist-get 'range mark) (alist-get 'range review))))

(defun org-sm-weread--body (text book note chapter)
  "Attach source metadata to TEXT using NOTE location and CHAPTER title.
The card keeps its own provenance: review shows one card at a time, without
the book/chapter headings it is filed under.

A link is emitted only when the API sends a `deepLink': the gateway contract
forbids hand-built weread:// URLs, and Linux has no handler for them anyway.
Today neither notes endpoint returns the field, so cards carry no link."
  (let ((link (or (alist-get 'deepLink note) (alist-get 'deepLink book))))
    (concat (org-sm-weread--text text) "\n\n来源："
            (org-sm-weread--text (alist-get 'title book))
            (when-let* ((author (alist-get 'author book))) (concat " / " author))
            (when chapter (concat "\n章节：" (org-sm-weread--text chapter)))
            (when (org-string-nw-p link)
              (concat "\n[[" link "][在微信读书中打开]]")))))

(defun org-sm-weread--create (body key index olp &optional review-key)
  "Capture BODY under OLP and record KEY and optional REVIEW-KEY in INDEX."
  (save-current-buffer
    ;; Locate the card by the id capture returns: capture restores point
    ;; (`org-with-wide-buffer'), so it does *not* leave it on the new heading.
    ;; Searching by id instead of moving relative to point is what keeps the
    ;; key on the card rather than on whatever heading point happened to sit.
    (let* ((id (org-sm--capture 'topic body nil olp))
           (pos (or (org-find-entry-with-id id)
                    (error "org-sm-weread: captured card %s vanished" id))))
      (org-with-wide-buffer
       (goto-char pos)
       (org-entry-put nil "WEREAD_KEY" key)
       (when review-key (org-entry-put nil "WEREAD_REVIEW_KEY" review-key))
       (save-buffer)
       (puthash key (copy-marker pos) index)
       (when review-key (puthash review-key (copy-marker pos) index))))))

(defun org-sm-weread--append-review (marker text key index)
  "Append unseen thought TEXT at MARKER without resetting the card's schedule."
  (with-current-buffer (marker-buffer marker)
    (org-with-wide-buffer
     (goto-char marker)
     (unless (org-entry-get nil "WEREAD_REVIEW_KEY")
       (let ((end (cdr (org-sm--body-bounds))))
         (goto-char end)
         (insert "\n想法：\n\n" (org-sm-weread--text text) "\n"))
       (goto-char marker)
       (org-entry-put nil "WEREAD_REVIEW_KEY" key)
       (save-buffer)
       (puthash key (copy-marker marker) index)
       t))))

(defun org-sm-weread--import-book (entry index)
  "Import ENTRY using source INDEX; return (CREATED . APPENDED) counts."
  (let* ((book (or (alist-get 'book entry) entry))
         (book-id (or (alist-get 'bookId entry) (alist-get 'bookId book)))
         (data (org-sm-weread--request "/book/bookmarklist" (cons 'bookId book-id)))
         (marks (alist-get 'updated data))
         (chapters (alist-get 'chapters data))
         (reviews (mapcar (lambda (r) (alist-get 'review r))
                          (org-sm-weread--pages "/review/list/mine" 'reviews 'synckey
                                                (cons 'bookid book-id))))
         (created 0) (appended 0))
    ;; Validate IDs before modifying this book's cards.
    (dolist (mark marks) (org-sm-weread--key "bookmark" book-id (alist-get 'bookmarkId mark)))
    (dolist (review reviews) (org-sm-weread--key "review" book-id (alist-get 'reviewId review)))
    (dolist (mark marks)
      (let* ((key (org-sm-weread--key "bookmark" book-id (alist-get 'bookmarkId mark)))
             (marker (gethash key index))
             ;; Ambiguous locations must not attach a thought to the wrong card.
             (matches (cl-remove-if-not
                       (lambda (r) (org-sm-weread--location-equal-p mark r)) reviews))
             (review (when (and (= (length matches) 1)
                                (= 1 (cl-count-if
                                      (lambda (m) (org-sm-weread--location-equal-p m (car matches))) marks)))
                       (car matches)))
             (rkey (when review (org-sm-weread--key "review" book-id (alist-get 'reviewId review))))
             (new-review (and rkey (not (gethash rkey index))))
             (chapter (org-sm-weread--chapter mark chapters)))
        (if marker
            (when (and new-review
                       (org-sm-weread--append-review marker (alist-get 'content review) rkey index))
              (cl-incf appended))
          (org-sm-weread--create
           (concat (org-sm-weread--body (alist-get 'markText mark) book mark chapter)
                   (when new-review
                     (concat "\n\n想法：\n\n" (org-sm-weread--text (alist-get 'content review)))))
           key index (org-sm-weread--olp book chapter) (when new-review rkey))
          (cl-incf created))))
    (dolist (review reviews)
      (let ((key (org-sm-weread--key "review" book-id (alist-get 'reviewId review)))
            (chapter (org-sm-weread--chapter review chapters)))
        (unless (gethash key index)
          (org-sm-weread--create
           (org-sm-weread--body (alist-get 'content review) book review chapter)
           key index (org-sm-weread--olp book chapter))
          (cl-incf created))))
    (cons created appended)))

;;;###autoload
(defun org-sm-weread-import ()
  "Choose one book or all notebooks and import new notes into the org-sm inbox.
Synchronous network requests can block Emacs (60 seconds per request).
Existing text is never replaced.  A new associated thought is appended once."
  (interactive)
  (unless org-sm-directory (user-error "Set org-sm-directory for vault-wide deduplication"))
  (let* ((books (org-sm-weread--pages "/user/notebooks" 'books 'lastSort))
         (choices (mapcar (lambda (book)
                           (cons (format "%s [%s]" (alist-get 'title (alist-get 'book book))
                                         (alist-get 'bookId book)) book)) books))
         (selection (completing-read "WeRead: " (cons "ALL_BOOK" (mapcar #'car choices)) nil t))
         (index (org-sm-weread--index))
         (created 0) (appended 0))
    (unwind-protect
        (dolist (book (if (equal selection "ALL_BOOK") books
                       (list (cdr (assoc selection choices)))))
          (let ((counts (org-sm-weread--import-book book index)))
            (cl-incf created (car counts)) (cl-incf appended (cdr counts))))
      (maphash (lambda (_ marker) (set-marker marker nil)) index))
    (message "WeRead: 新增 %d 张卡片，补充 %d 条想法；已有 ID 跳过" created appended)))

(provide 'org-sm-weread)
;;; org-sm-weread.el ends here
