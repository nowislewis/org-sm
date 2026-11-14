;;; org-sm-source.el --- Provenance for captures from PDF, EPUB and web -*- lexical-binding: t -*-

;; Author: Lewis Liu
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: org, spaced-repetition, incremental-reading

;;; Commentary:
;;
;; An extract that forgets where it came from is orphaned the moment the
;; book is closed.  This module answers three questions at capture time,
;; with no AI and no rewriting:
;;
;;   What am I reading?   `org-sm-source-context' -- one plist for a PDF
;;                        (emureader), an EPUB (nov), a web page (eww) or a
;;                        plain file.  Org buffers return nil: inside notes,
;;                        extraction already links child to parent.
;;
;;   Where does it go?    `org-sm-source-target-file' -- the vault file that
;;                        already holds cards from this document, else the
;;                        Denote note whose `#+reference:' names it.  The
;;                        inbox stops being the default home of every book.
;;
;;   What does the page say?
;;                        `org-sm-source-page-text' -- PDF pages are images
;;                        in emureader, so text comes from `mutool draw -F
;;                        txt', falling back to tesseract for scans.  OCR
;;                        takes seconds, so it runs as a background process
;;                        and caches under `org-sm-source-cache-directory'.
;;
;; `page:FILE::N' links open the document at page N; EPUB positions reuse
;; nov.el's own `nov:' links.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'org)
(require 'ol)

(defvar nov-file-name)
(defvar nov-metadata)
(defvar nov-documents-index)
(defvar eww-data)
(declare-function emureader-current-doc-pagenumber "emureader")
(declare-function emureader-goto-page "emureader")
(declare-function doc-view-goto-page "doc-view")
(declare-function doc-view-current-page "doc-view")

(defgroup org-sm-source nil
  "Provenance and page text for org-sm captures."
  :group 'org-sm
  :prefix "org-sm-source-")

(defcustom org-sm-source-cache-directory
  (expand-file-name "org-sm/pages" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Directory caching extracted PDF page text, OCR results included."
  :type 'directory
  :group 'org-sm-source)

(defcustom org-sm-source-ocr-languages "chi_sim+eng"
  "Tesseract languages used for PDF pages without a text layer."
  :type 'string
  :group 'org-sm-source)

(defcustom org-sm-source-tessdata-directory
  (let ((dir (expand-file-name "~/.local/share/tessdata")))
    (and (file-directory-p dir) dir))
  "Tesseract data directory, or nil for tesseract's built-in default.
Language packs installed outside the package manager (for example
tessdata_best `chi_sim') live here."
  :type '(choice (const :tag "Tesseract default" nil) directory)
  :group 'org-sm-source)

(defconst org-sm-source-ocr-min-chars 40
  "Text-layer output shorter than this triggers OCR instead.
A scanned page still yields a page number or a stray glyph, so the test is
volume rather than emptiness.")

(defconst org-sm-image-extensions '("png" "jpg" "jpeg" "gif" "svg" "webp")
  "File extensions org-sm treats as images, lower case without the dot.
Shared by import, which copies them next to a note, and the web reader,
which serves them; the two must agree on what an image is.")

;;;; ---- What am I reading? -------------------------------------------------

(defun org-sm-source--short (title)
  "Return TITLE trimmed of bracketed noise and capped at 40 columns.
Download names carry publisher and mirror tags in parentheses; the footer
only needs enough to recognise the book."
  (truncate-string-to-width
   (string-trim (replace-regexp-in-string "[(（][^)）]*[)）]" "" (or title "")))
   40 nil nil "…"))

(defun org-sm-source-context ()
  "Return a plist describing the document being read, or nil.
Keys: `:kind' (`pdf', `epub', `web' or `file'), `:file', `:page', `:url',
`:link' and `:label'.  Reads buffer-local state, so call it while the
reader buffer is current.  Org buffers return nil on purpose."
  (cond
   ((derived-mode-p 'emureader-mode 'doc-view-mode)
    (let ((file (abbreviate-file-name (buffer-file-name)))
          (page (if (derived-mode-p 'emureader-mode)
                    (emureader-current-doc-pagenumber)
                  (doc-view-current-page))))
      (list :kind 'pdf :file file :page page
            :link (format "page:%s::%d" file page)
            :label (format "《%s》p.%d"
                           (org-sm-source--short (file-name-base file)) page))))
   ((derived-mode-p 'nov-mode)
    (let ((file (abbreviate-file-name nov-file-name))
          (pos (if (use-region-p) (region-beginning) (point))))
      (list :kind 'epub :file file
            :link (format "nov:%s::%d:%d" file (or nov-documents-index 0) pos)
            :label (format "《%s》§%d"
                           (org-sm-source--short
                            (or (cdr (assq 'title nov-metadata))
                                (file-name-base file)))
                           (1+ (or nov-documents-index 0))))))
   ((derived-mode-p 'eww-mode)
    (let ((url (plist-get eww-data :url)))
      (list :kind 'web :url url :link url
            :label (org-sm-source--short
                    (or (org-string-nw-p (plist-get eww-data :title)) url)))))
   ((and (buffer-file-name) (not (derived-mode-p 'org-mode)))
    (let ((file (abbreviate-file-name (buffer-file-name)))
          (line (line-number-at-pos (if (use-region-p) (region-beginning) (point)))))
      (list :kind 'file :file file
            :link (format "file:%s::%d" file line)
            :label (format "%s:%d" (file-name-nondirectory file) line))))))

(defun org-sm-source-footer (context)
  "Return the provenance line for CONTEXT, or nil when CONTEXT is nil.
Built with `org-link-make-string', which escapes brackets: book titles
such as \"[美] 作者\" are common and must not break the link."
  (when context
    (concat "来源：" (org-link-make-string (plist-get context :link)
                                        (plist-get context :label)))))

;;;; ---- Where does it go? --------------------------------------------------

(defun org-sm-source--file-citing (document exclude)
  "Return the vault file that already links DOCUMENT, other than EXCLUDE.
The file holding the most such links wins: that is where this book's
cards have been living."
  (let* ((hits (org-sm-rg "-c" "--glob" "*.org" "-F" document))
         (counts (delq nil
                       (mapcar (lambda (line)
                                 (when (string-match "\\`\\(.*\\):\\([0-9]+\\)\\'" line)
                                   (cons (match-string 1 line)
                                         (string-to-number (match-string 2 line)))))
                               hits)))
         (counts (seq-remove (lambda (c) (and exclude (file-equal-p (car c) exclude)))
                             counts)))
    (car (car (sort counts (lambda (a b) (> (cdr a) (cdr b))))))))

(defun org-sm-source--file-by-reference (key)
  "Return the vault note whose `#+reference:' keyword equals KEY."
  (seq-some (lambda (line)
              (when (string-match "\\`\\(.*\\.org\\):[0-9]+:#\\+reference:[ \t]*\\(.*\\)\\'" line)
                (let ((value (string-trim (match-string 2 line))))
                  (and (equal (string-remove-prefix ";" value) key)
                       (match-string 1 line)))))
            (org-sm-rg "-n" "--glob" "*.org" "-i" "^#\\+reference:")))

(defun org-sm-source-target-file (context &optional exclude)
  "Return the file that should receive a capture from CONTEXT, or nil.
EXCLUDE is the inbox: a book with cards only there has no home yet.  Tries
the file already citing the document, then a note whose `#+reference:'
matches the document's base name (citar-managed files are named by key)."
  (when-let* ((document (plist-get context :file)))
    (or (org-sm-source--file-citing document exclude)
        (org-sm-source--file-by-reference (file-name-base document)))))

;;;; ---- What does the page say? --------------------------------------------

(defun org-sm-source--cache-file (file page)
  "Return the cache path for FILE's PAGE, keyed by path and modification time."
  (let* ((path (expand-file-name file))
         (mtime (file-attribute-modification-time (file-attributes path))))
    (expand-file-name
     (format "%s-%d.txt" (secure-hash 'sha1 (format "%s|%S" path mtime)) page)
     org-sm-source-cache-directory)))

(defun org-sm-source--cached (file page)
  "Return cached text for FILE's PAGE, or nil."
  (let ((cache (org-sm-source--cache-file file page)))
    (when (file-readable-p cache)
      (with-temp-buffer
        (insert-file-contents cache)
        (buffer-string)))))

(defun org-sm-source--store (file page text)
  "Cache TEXT for FILE's PAGE when it has content; return TEXT."
  (when (org-string-nw-p text)
    (let ((cache (org-sm-source--cache-file file page)))
      (make-directory (file-name-directory cache) t)
      (with-temp-file cache (insert text))))
  text)

(defun org-sm-source--run (program args then)
  "Run PROGRAM with ARGS in the background; call THEN with stdout or nil.
stderr is discarded so tool chatter never leaks into card text."
  (let ((out (generate-new-buffer " *org-sm-source*"))
        (err (generate-new-buffer " *org-sm-source-err*")))
    (make-process
     :name "org-sm-source" :buffer out :stderr err
     :command (cons program args) :noquery t
     :coding 'utf-8
     :sentinel
     (lambda (proc _event)
       (unless (process-live-p proc)
         (let ((text (and (zerop (process-exit-status proc))
                          (with-current-buffer out (buffer-string)))))
           (kill-buffer out)
           (when (buffer-live-p err) (kill-buffer err))
           (funcall then text)))))))

(defun org-sm-source--clean (text)
  "Trim TEXT and drop the form feeds `mutool' puts between pages."
  (string-trim (replace-regexp-in-string "\f" "" (or text ""))))

(defun org-sm-source--ocr (file page then)
  "OCR FILE's PAGE in the background and call THEN with the text or nil."
  (let ((png (make-temp-file "org-sm-page-" nil ".png")))
    (org-sm-source--run
     "mutool" (list "draw" "-q" "-r" "200" "-o" png
                    (expand-file-name file) (number-to-string page))
     (lambda (_)
       (org-sm-source--run
        "tesseract"
        (append (and org-sm-source-tessdata-directory
                     (list "--tessdata-dir" org-sm-source-tessdata-directory))
                (list png "-" "-l" org-sm-source-ocr-languages "--psm" "6"
                      "-c" "preserve_interword_spaces=1"))
        (lambda (text)
          (ignore-errors (delete-file png))
          (funcall then (org-sm-source--clean text))))))))

(defun org-sm-source-page-text (file page callback)
  "Call CALLBACK with the text of FILE's PAGE, or nil when none is found.
Order: cache, then the text layer via mutool, then OCR.  Never blocks: the
first two are fast, OCR can take ten seconds on a dense page."
  (if-let* ((cached (org-sm-source--cached file page)))
      (funcall callback cached)
    (org-sm-source--run
     "mutool" (list "draw" "-q" "-F" "txt" "-o" "-"
                    (expand-file-name file) (number-to-string page))
     (lambda (raw)
       (let ((text (org-sm-source--clean raw)))
         (if (>= (length text) org-sm-source-ocr-min-chars)
             (funcall callback (org-sm-source--store file page text))
           (org-sm-source--ocr
            file page
            (lambda (ocr)
              (funcall callback (org-sm-source--store file page ocr))))))))))

(defun org-sm-source-page-text-now (file page)
  "Return FILE's PAGE text if it is available without waiting, else nil.
Cache first, then the text layer, which takes tens of milliseconds.  A
scanned page starts OCR in the background instead, so callers on an
interactive path never stall and the next request finds it cached."
  (or (org-sm-source--cached file page)
      (let* ((coding-system-for-read 'utf-8)
             (text (org-sm-source--clean
                    (with-temp-buffer
                      (call-process "mutool" nil '(t nil) nil
                                    "draw" "-q" "-F" "txt" "-o" "-"
                                    (expand-file-name file) (number-to-string page))
                      (buffer-string)))))
        (if (>= (length text) org-sm-source-ocr-min-chars)
            (org-sm-source--store file page text)
          (org-sm-source--ocr file page
                              (lambda (ocr) (org-sm-source--store file page ocr)))
          nil))))

(defun org-sm-source-page-link (text)
  "Return (FILE . PAGE) for the first `page:' link in TEXT, or nil."
  (when (and text (string-match "\\[\\[page:\\([^]]+?\\)::\\([0-9]+\\)\\]" text))
    (cons (match-string 1 text) (string-to-number (match-string 2 text)))))

;;;; ---- page: links ---------------------------------------------------------

(defun org-sm-source-open-page (path _)
  "Open the document in PATH (FILE::PAGE) at that page."
  (let* ((parts (split-string path "::"))
         (page (and (cadr parts) (string-to-number (cadr parts)))))
    (find-file (car parts))
    (when (and page (> page 0))
      (cond ((derived-mode-p 'emureader-mode) (emureader-goto-page page))
            ((derived-mode-p 'doc-view-mode) (doc-view-goto-page page))))))

(defun org-sm-source-store-page ()
  "Store a `page:' link for the PDF page currently shown."
  (when-let* ((context (and (derived-mode-p 'emureader-mode 'doc-view-mode)
                            (org-sm-source-context))))
    (org-link-store-props :type "page"
                          :link (plist-get context :link)
                          :description (plist-get context :label))))

(org-link-set-parameters "page"
                         :follow #'org-sm-source-open-page
                         :store #'org-sm-source-store-page)

(provide 'org-sm-source)
;;; org-sm-source.el ends here
