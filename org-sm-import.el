;;; org-sm-import.el --- Turn long material into topic cards -*- lexical-binding: t -*-

;; Author: Lewis Liu
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: org, spaced-repetition, incremental-reading

;;; Commentary:
;;
;; One invariant keeps org-sm simple: inside a card, every heading is a card.
;; A card's body ends at its first child heading, the capture workbench reads
;; `**' as "new card", and child counts drive stall detection and neural
;; review.  An article's own section headings would break each of those, so
;; they are flattened into `§ TITLE' lines once, where material enters, keeping
;; the whole article intact as one long-form topic card:
;;
;;   `org-sm-sectionize'       pure: Org text -> flattened text with `§ TITLE'
;;   `org-sm-sectionize-card'  flattens plain subheadings in the card at point
;;   `org-sm-import-file'      HTML, Markdown, TXT, DOCX or Org file -> note
;;   `org-sm-import-url'       fetch a static page -> note
;;   `org-sm-import-highlights' shared highlight importer for WeRead & Zotero
;;
;; Linked images are copied next to the note so the note survives link rot and
;; the web reader can show them.  Import never calls AI and never makes clozes.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'ol)
(require 'dom)
(require 'org-sm)

(defvar url-http-end-of-headers)
(declare-function shr-dom-to-xml "shr")
(declare-function eww-readable-dom "eww")
(declare-function denote-sluggify-title "denote")

(defcustom org-sm-import-priority ?C
  "Priority cookie for imported articles and highlights, or nil.
New material always feels urgent -- SuperMemo calls it priority bias --
and an import arrives a whole article or book at a time.  At [#C] it waits
behind what has already proved itself; an extract from it drops the C (see
`org-sm-extract'), and `org-sm-cycle-priority' raises a card by hand."
  :type '(choice (const :tag "None" nil) character)
  :group 'org-sm)

(defcustom org-sm-import-topics-per-day 1
  "How many highlight cards from one batch import come due per day.
Importing dozens of highlights must not dump all of them into tomorrow."
  :type 'integer
  :group 'org-sm)

(defconst org-sm-import--chrome-tags '(nav header footer aside script style form)
  "Tags removed from a fetched page before readability picks the article.")

(defconst org-sm-import--lazy-image-attrs '(data-src data-original data-actualsrc)
  "Attributes that lazy-loading pages use for the real image URL.")

;;;; ---- Sectionize: flatten internal headings into `§ TITLE' ------------------

(defun org-sm-sectionize (text)
  "Rewrite plain Org headings in TEXT to `§ TITLE' lines and strip noise.
org-sm assumes every heading inside a card is itself a card: keeping an
imported article or note as one long-form topic card therefore means
turning its internal section headings into `§ TITLE' lines."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*<<[^>\n]*>>[ \t]*\n" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward
            "^[ \t]*:PROPERTIES:[ \t]*\n\\(?:.*\n\\)*?[ \t]*:END:[ \t]*\n" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "^\\*+ +\\(.*\\)$" nil t)
      (let ((title (string-trim (match-string 1))))
        (replace-match (concat "\n§ " title "\n") t t)))
    (goto-char (point-min))
    (while (re-search-forward "\n[ \t]*\n\\(?:[ \t]*\n\\)+" nil t)
      (replace-match "\n\n"))
    (string-trim (buffer-string))))

;;;###autoload
(defun org-sm-sectionize-card ()
  "Flatten plain section headings inside the card at point into `§ TITLE' lines.
Existing child cards are left alone.  Return the number of headings flattened."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (unless (org-sm-type) (user-error "Not on an org-sm card"))
    (let* ((n (org-sm-section-count))
           (start (car (org-sm-body-bounds)))
           (end (save-excursion
                  (let ((limit (save-excursion (org-end-of-subtree t t))))
                    (while (and (outline-next-heading) (< (point) limit)
                                (not (org-sm-type))))
                    (min (point) limit))))
           (body (org-sm-sectionize (buffer-substring-no-properties start end))))
      (delete-region start end)
      (goto-char start)
      (when (org-string-nw-p body) (insert body "\n"))
      (org-sm--card-changed 'sectionize)
      (when (called-interactively-p 'interactive)
        (message "org-sm: flattened %d section heading%s into § lines"
                 n (if (= n 1) "" "s")))
      n)))

;;;; ---- Images ----------------------------------------------------------------

(defun org-sm-import--localize-images (text from-dir media)
  "Copy or download every image TEXT links to into MEDIA; return new TEXT.
Relative paths resolve against FROM-DIR; http(s) images are downloaded.
MEDIA is relative to `org-sm-directory', so links stay relative and the
web reader can serve them.  An image that cannot be fetched keeps its
original link rather than failing the import."
  (let ((dir (expand-file-name media org-sm-directory))
        (n 0))
    (replace-regexp-in-string
     org-link-bracket-re
     (lambda (link)
       ;; `replace-regexp-in-string' reuses the match data after this returns.
       (save-match-data
         (string-match org-link-bracket-re link)
         (let* ((target (match-string 1 link))
                (desc (match-string 2 link))
                (path (string-remove-prefix "file:" target))
                (ext (downcase (or (file-name-extension
                                    (car (split-string path "[?#]")))
                                   ""))))
           (if (not (member ext org-sm-image-extensions))
               link
             (condition-case nil
                 (let ((dest (expand-file-name
                              (format "img-%d.%s" (cl-incf n) ext) dir)))
                   (make-directory dir t)
                   (if (string-match-p "\\`https?://" path)
                       (url-copy-file path dest t)
                     (copy-file (expand-file-name path from-dir) dest t))
                   (org-link-make-string
                    (concat "file:" (file-relative-name dest org-sm-directory))
                    desc))
               (error link))))))
     text t t)))

;;;; ---- Writing a note ----------------------------------------------------------

(defun org-sm-import--clean-title (title)
  "Return TITLE on one line, without invisible characters.
Page metadata carries zero-width spaces and BOMs more often than expected
\(Zhihu appends U+200B to author names), and they break file names."
  (string-trim
   (replace-regexp-in-string "[[:space:]\u200b-\u200f\ufeff]+" " " (or title ""))))

(defun org-sm-import--file-name (title)
  "Return a Denote-style file name for a note titled TITLE.
The slug is capped at 120 bytes: file systems allow 255, and CJK titles
take three bytes a character."
  (let ((slug (if (fboundp 'denote-sluggify-title)
                  (denote-sluggify-title title)
                (string-trim (downcase (replace-regexp-in-string
                                        "[^[:alnum:]]+" "-" title))
                             "-+" "-+"))))
    (while (> (string-bytes slug) 120)
      (setq slug (substring slug 0 -1)))
    (format "%s--%s__import.org" (format-time-string "%Y%m%dT%H%M%S") slug)))

(defun org-sm-import--existing (source)
  "Return the vault note already imported from SOURCE, or nil.
Matches our `#+source:' and the `#+SOURCE_URL:' Copy-as-Org exporters write,
so a page saved from the browser and the same URL fetched here agree."
  (car (org-sm-rg "-l" "-i" "-F" "--glob" "*.org"
                  "-e" (concat "#+source: " source)
                  "-e" (concat "#+source_url: " source))))

(defun org-sm-import--write (name title source text)
  "Write TEXT as note NAME whose single topic card is TITLE, from SOURCE.
Internal headings in TEXT are flattened to `§ TITLE' lines via
`org-sm-sectionize-card'.  Return FILE."
  (let ((file (expand-file-name name org-sm-directory)))
    (with-temp-file file
      (insert (format "#+title:      %s\n#+date:       %s\n#+filetags:   :import:\n#+source: %s\n\n* %s\n%s\n"
                      title (format-time-string "[%F %a %H:%M]") source title
                      text)))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (re-search-forward "^\\* ")
      (org-sm-item-mark 'topic)
      (org-set-property "SOURCE"
                        (org-sm-source-link
                         (list :link source :label (format "《%s》" title))))
      (when org-sm-import-priority (org-priority org-sm-import-priority))
      (org-sm-sectionize-card)
      (let ((save-silently t)) (save-buffer))
      file)))

(defun org-sm-import--media (name)
  "Return the image directory, relative to the vault, for note NAME."
  (concat (file-name-base name) "-media"))

(defun org-sm-import--finish (file)
  "Visit the imported note FILE and report."
  (find-file file)
  (message "org-sm: imported as one topic card"))

;;;; ---- Sources ------------------------------------------------------------------

(defun org-sm-import--pandoc (file from)
  "Convert FILE in pandoc format FROM to Org, headings demoted one level.
Media embedded in the container (EPUB, DOCX, ODT) is extracted to a
temporary directory for `org-sm-import--localize-images' to copy.  Other
formats are not given `--extract-media': pandoc would then download remote
images itself, ignoring Emacs's proxy, and silently drop any it cannot
fetch.  Return (TEXT . TMP-DIR)."
  (let* ((tmp (make-temp-file "org-sm-pandoc-" t))
         (default-directory (file-name-as-directory tmp))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8))
    (with-temp-buffer
      (unless (eq 0 (apply #'call-process "pandoc" nil '(t nil) nil
                           (expand-file-name file) "-f" from "-t" "org"
                           "--wrap=none" "--shift-heading-level-by=1"
                           (and (member from '("epub" "docx" "odt"))
                                (list "--extract-media=."))))
        (error "org-sm-import: pandoc failed on %s" file))
      (cons (buffer-string) tmp))))

(defun org-sm-import--pandoc-title (file)
  "Return the title in FILE's metadata, or nil."
  (let ((template (make-temp-file "org-sm-title-" nil ".txt" "$title$")))
    (unwind-protect
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8))
            (when (eq 0 (call-process "pandoc" nil '(t nil) nil
                                      (expand-file-name file) "-t" "plain"
                                      (concat "--template=" template)))
              (org-string-nw-p (string-trim (buffer-string))))))
      (delete-file template))))

(defun org-sm-import--org-keyword (name)
  "Return the value of the first `#+NAME:' keyword in the buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward (format "^#\\+%s:[ \t]*\\(.*\\)$" name) nil t)
        (org-string-nw-p (string-trim (match-string 1)))))))

(defun org-sm-import--org-file (file)
  "Return (TITLE SOURCE TEXT) from an Org FILE, its keyword header removed.
Headings come back demoted one level, ready to sit under the note's card."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((title (org-sm-import--org-keyword "title"))
          (source (or (org-sm-import--org-keyword "source_url")
                      (org-sm-import--org-keyword "source"))))
      (goto-char (point-min))
      (while (looking-at "^\\(?:#\\+[[:alnum:]_]+:.*\\)?\n")
        (replace-match ""))
      (list title source
            (replace-regexp-in-string "^\\(\\*+ \\)" "*\\1" (buffer-string))))))

;;;###autoload
(defun org-sm-import-file (file)
  "Import FILE (HTML, Markdown, DOCX, TXT, EPUB or Org) as one topic card.
Internal headings are rewritten to `§ TITLE' lines so the whole article stays
one long-form topic card.  An Org file saved by a Copy-as-Org browser
exporter keeps its `#+TITLE' and `#+SOURCE_URL'.  Refuses a second import of
the same source: the existing note is where its card already lives."
  (interactive (list (read-file-name "Import into org-sm: " nil nil t)))
  (unless org-sm-directory (user-error "Set `org-sm-directory' first"))
  (let* ((file (abbreviate-file-name (expand-file-name file)))
         (ext (downcase (or (file-name-extension file) "")))
         (org-p (equal ext "org"))
         (txt-p (equal ext "txt"))
         (parsed (and org-p (org-sm-import--org-file file)))
         (source (or (nth 1 parsed) file))
         (title (org-sm-import--clean-title
                 (or (nth 0 parsed)
                     (and (not txt-p) (org-sm-import--pandoc-title file))
                     (file-name-base file))))
         (name (org-sm-import--file-name title)))
    (when-let* ((done (org-sm-import--existing source)))
      (user-error "Already imported in %s" (file-name-nondirectory done)))
    (pcase-let* ((`(,text . ,tmp)
                  (cond
                   (org-p (cons (nth 2 parsed) nil))
                   (txt-p (cons (with-temp-buffer
                                  (insert-file-contents file)
                                  (org-sm-import-escape (buffer-string)))
                                nil))
                   (t (org-sm-import--pandoc
                       file (pcase ext
                              ("md" "markdown") ("htm" "html") (_ ext)))))))
      (unwind-protect
          (org-sm-import--finish
           (org-sm-import--write
            name title source
            (org-sm-import--localize-images
             text (or tmp (file-name-directory (expand-file-name file)))
             (org-sm-import--media name))))
        (when tmp (delete-directory tmp t))))))

(defun org-sm-import--fetch (url)
  "Return the HTML at URL as a decoded string."
  (let ((buf (url-retrieve-synchronously url t t 30)))
    (unless buf (user-error "Could not fetch %s" url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (let ((charset (save-excursion
                           (and (re-search-forward
                                 "charset=[\"']?\\([-_[:alnum:]]+\\)"
                                 (min (point-max) 4096) t)
                                (intern-soft (downcase (match-string 1)))))))
            (goto-char (or url-http-end-of-headers (point-min)))
            (decode-coding-string
             (buffer-substring-no-properties (point) (point-max))
             (if (and charset (coding-system-p charset)) charset 'utf-8))))
      (kill-buffer buf))))

(defun org-sm-import--keep-images (article)
  "Wrap bare IMG nodes in ARTICLE with a paragraph.
Pandoc drops an `<img>' that is not inside a block element, which is
exactly where hand-written pages put figures."
  (dolist (img (dom-by-tag article 'img))
    (let ((parent (dom-parent article img)))
      (when (and parent
                 (not (memq (dom-tag parent)
                            '(p figure div li td th blockquote a section))))
        (when-let* ((cell (member img (cdr parent))))
          (setcar cell (dom-node 'p nil img)))))))

(defun org-sm-import--absolutize (article url)
  "Make every link and image URL in ARTICLE absolute against URL.
Fragment-only links stay as they are: they are meant to scroll.  A
lazy-loading image (`data-src') has a placeholder `src', so the real
attribute wins."
  (dolist (img (dom-by-tag article 'img))
    (when-let* ((real (seq-some (lambda (a) (dom-attr img a))
                                org-sm-import--lazy-image-attrs)))
      (dom-set-attribute img 'src real)))
  (dolist (node (dom-search article (lambda (n) (or (dom-attr n 'src)
                                                    (dom-attr n 'href)))))
    (dolist (attr '(src href))
      (when-let* ((v (dom-attr node attr)))
        (unless (string-prefix-p "#" v)
          (dom-set-attribute node attr (url-expand-file-name v url))))))
  article)

(defun org-sm-import--readable (html url)
  "Return (TITLE . HTML) for the article in HTML fetched from URL.
Chrome is stripped before `eww-readable-dom' picks the article (it counts
words by spaces and gives up on most Chinese pages, so the stripped page
is the fallback).  Lazy images, relative links and a leading heading that
repeats the title are all fixed up here, before pandoc sees the HTML."
  (require 'eww)
  (require 'shr)
  (let* ((dom (with-temp-buffer
                (insert html)
                (libxml-parse-html-region (point-min) (point-max))))
         (title (org-sm-import--clean-title
                 (dom-inner-text (car (dom-by-tag dom 'title))))))
    (dolist (tag org-sm-import--chrome-tags)
      (dolist (node (dom-by-tag dom tag)) (dom-remove-node dom node)))
    (let ((article (or (ignore-errors (eww-readable-dom dom)) dom)))
      (when-let* ((first (car (dom-search article (lambda (n) (memq (dom-tag n)
                                                                  '(h1 h2))))))
                  ((equal (org-sm-import--clean-title (dom-inner-text first)) title)))
        (dom-remove-node article first))
      (org-sm-import--keep-images article)
      (cons title (shr-dom-to-xml (org-sm-import--absolutize article url))))))

;;;###autoload
(defun org-sm-import-url (url)
  "Fetch the static page at URL and import it as one long-form topic card.
Internal headings are rewritten to `§ TITLE' lines.  Pages that need
JavaScript or a login extract better in the browser: save them with a
Copy-as-Org exporter and use `org-sm-import-file'."
  (interactive (list (read-string "URL: " (or (thing-at-point 'url) ""))))
  (unless org-sm-directory (user-error "Set `org-sm-directory' first"))
  (when-let* ((done (org-sm-import--existing url)))
    (user-error "Already imported in %s" (file-name-nondirectory done)))
  (pcase-let* ((`(,title . ,html) (org-sm-import--readable
                                   (org-sm-import--fetch url) url))
               (title (or (org-string-nw-p title) url))
               (name (org-sm-import--file-name title))
               (tmp (make-temp-file "org-sm-import-" nil ".html")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (set-buffer-file-coding-system 'utf-8)
            (insert html))
          (pcase-let ((`(,text . ,dir) (org-sm-import--pandoc tmp "html")))
            (unwind-protect
                (org-sm-import--finish
                 (org-sm-import--write
                  name title url
                  (org-sm-import--localize-images
                   text dir (org-sm-import--media name))))
              (delete-directory dir t))))
      (delete-file tmp))))

;;;; ---- External highlight importers (WeRead, Zotero) ------------------------

(defun org-sm-import-heading (text)
  "Return TEXT as a single-line Org heading safe for `org-find-olp', or nil.
Remote book and chapter titles may wrap or end in a `:tag:'-shaped run,
either of which would change the outline structure `org-find-olp' matches."
  (and (stringp text)
       (org-string-nw-p
        (replace-regexp-in-string
         "\\`[ \t*]+\\|[ \t:]+\\'" ""
         (replace-regexp-in-string "[ \t\n\r]+" " " text)))))

(defun org-sm-import-escape (text)
  "Escape Org structural line-starts in remote TEXT without rewriting prose."
  (replace-regexp-in-string
   "^\\(\\*+ \\|#\\+\\|:[[:alnum:]_]+:\\)" ",\\1" (or text "")))

(defun org-sm-import--seen-keys ()
  "Index highlight keys from `:SOURCE:' across the vault and inbox.
Reads live buffers when open so unsaved edits and refiles are respected."
  (let ((seen (make-hash-table :test #'equal))
        (files (delete-dups
                (append (list (org-sm-capture-target-file))
                        (org-sm-rg "-l" "--glob" "*.org" ":SOURCE:")
                        (delq nil
                              (mapcar (lambda (b)
                                        (let ((f (buffer-file-name b)))
                                          (and f org-sm-directory
                                               (string-suffix-p ".org" f t)
                                               (file-in-directory-p f org-sm-directory)
                                               f)))
                                      (buffer-list)))))))
    (dolist (file files)
      (when (or (file-exists-p file) (get-file-buffer file))
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward "^[ \t]*:SOURCE:[ \t]+\\(.+\\)$" nil t)
             (let ((src (string-trim (match-string 1))))
               (puthash src t seen)
               (when (string-match "\\(?:#\\|weread:\\)\\([^]\n ]+\\)" src)
                 (puthash (match-string 1 src) t seen))
               (when (string-match "[?&]annotation=\\([^]&\n ]+\\)" src)
                 (puthash (match-string 1 src) t seen))))))))
    seen))

(defun org-sm-import-highlights (items)
  "Import highlight ITEMS into the inbox; return count of created cards.
Each element of ITEMS is `(:key KEY :text TEXT :olp OLP :source SOURCE)'.
Items whose KEY already appears in a `:SOURCE:' property anywhere in the
vault or inbox are skipped (including dismissed cards).  New cards receive
`org-sm-import-priority' (`[#C]') and spread their due dates by
`org-sm-import-topics-per-day'."
  (unless org-sm-directory
    (user-error "Set `org-sm-directory' for vault-wide deduplication"))
  (let ((seen (org-sm-import--seen-keys))
        (per-day (max 1 org-sm-import-topics-per-day))
        (created 0)
        touched)
    (dolist (item items)
      (let ((key (plist-get item :key))
            (text (plist-get item :text))
            (olp (plist-get item :olp))
            (source (plist-get item :source)))
        (unless (and (org-string-nw-p key) (org-string-nw-p source))
          (error "Highlight import requires non-empty :key and :source"))
        (when (and (org-string-nw-p text) (not (gethash key seen)))
          (save-current-buffer
            (let ((days (1+ (/ created per-day))))
              (org-sm-capture-card 'topic (org-sm-import-escape text)
                                   nil olp nil source days
                                   org-sm-import-priority)
              (cl-pushnew (current-buffer) touched)))
          (puthash key t seen)
          (cl-incf created))))
    (dolist (buf touched)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((save-silently t)) (save-buffer)))))
    created))

(provide 'org-sm-import)
;;; org-sm-import.el ends here
