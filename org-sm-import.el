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
;; they are resolved once, where material enters, by one primitive:
;;
;;   `org-sm-sectionize'       pure: Org text -> (INTRO . CARDS)
;;   `org-sm-sectionize-card'  applies it to the card at point
;;   `org-sm-import-file'      EPUB, HTML, Markdown or Org file -> note
;;   `org-sm-import-url'       fetch a static page -> note
;;
;; Long sections become child topics, due one per day in reading order;
;; short ones become `§ TITLE' lines; shallow container headings (book parts)
;; prefix the titles beneath them.  Linked images are copied next to the note
;; so the note survives link rot and the web reader can show them.
;;
;; Web pages are best extracted in the browser, which sees the rendered page
;; (JavaScript, logins, lazy images).  A page saved by a Copy-as-Org exporter
;; imports as a .org file directly, keeping its `#+SOURCE_URL'.
;; `org-sm-import-url' remains as a fallback for static pages.  Import never
;; calls AI and never makes clozes.

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

(defcustom org-sm-import-min-chars 200
  "Sections with less text than this are folded into their neighbour.
Title pages, copyright notices and one-paragraph sections are not worth a
review slot of their own."
  :type 'integer
  :group 'org-sm)

(defcustom org-sm-import-topics-per-day 1
  "How many section cards from one import come due per day.
Importing a long article must not dump all its sections into tomorrow."
  :type 'integer
  :group 'org-sm)

(defconst org-sm-import--chrome-tags '(nav header footer aside script style form)
  "Tags removed from a fetched page before readability picks the article.")

(defconst org-sm-import--lazy-image-attrs '(data-src data-original data-actualsrc)
  "Attributes that lazy-loading pages use for the real image URL.")

;;;; ---- Sectionize: the one structural primitive ------------------------------

(defun org-sm-import--clean (text)
  "Strip pandoc's anchor targets and property drawers from Org TEXT.
They serve cross-references inside the original document and would only
show up as noise in a card body."
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
    (while (re-search-forward "\n\\{3,\\}" nil t)
      (replace-match "\n\n"))
    (string-trim (buffer-string))))

(defun org-sm-import--segments (text)
  "Split Org TEXT into (LEVEL TITLE BODY) segments, one per heading.
Text before the first heading becomes a segment with LEVEL 0."
  (let (segments (level 0) title (start 1))
    (with-temp-buffer
      (insert text "\n")
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*+\\) +\\(.*\\)$" nil t)
        ;; Capture the match before `string-trim', which clobbers match data.
        (let ((next-level (length (match-string 1)))
              (next-title (match-string 2))
              (head (match-beginning 0))
              (after (match-end 0)))
          (push (list level title
                      (string-trim (buffer-substring-no-properties start head)))
                segments)
          (setq level next-level
                title (string-trim next-title)
                start (min (point-max) (1+ after)))))
      (push (list level title
                  (string-trim (buffer-substring-no-properties start (point-max))))
            segments))
    (nreverse segments)))

(defun org-sm-import--substantial-p (segment)
  "Return non-nil when SEGMENT has enough text to be its own card."
  (>= (length (nth 2 segment)) org-sm-import-min-chars))

(defun org-sm-import--split-level (segments)
  "Return the heading level whose sections become cards, or nil.
The shallowest level where at least two headings, and at least half of
them, are substantial.  Parts and title pages fail the test; chapters pass.
Nil means nothing deserves a card of its own."
  (seq-find (lambda (lvl)
              (let* ((at (seq-filter (lambda (s) (= (car s) lvl)) segments))
                     (big (seq-count #'org-sm-import--substantial-p at)))
                (and (>= big 2) (>= (* 2 big) (length at)))))
            (sort (seq-uniq (seq-remove #'zerop (mapcar #'car segments))) #'<)))

(defun org-sm-import--units (segments split)
  "Merge SEGMENTS deeper than SPLIT into the section above them.
Return (LEVEL TITLE BODY) units; deeper headings become `§ TITLE' lines."
  (let (units)
    (dolist (seg segments)
      (pcase-let ((`(,level ,title ,body) seg))
        (if (and units (> level split) (>= (car (car units)) split))
            (setf (nth 2 (car units))
                  (string-join (delq nil (list (org-string-nw-p (nth 2 (car units)))
                                               (concat "§ " title)
                                               (org-string-nw-p body)))
                               "\n\n"))
          (push (list level title body) units))))
    (nreverse units)))

(defun org-sm-sectionize (text)
  "Plan how Org TEXT becomes one topic card and its child cards.
Return (INTRO . CARDS), CARDS being (TITLE . BODY) pairs in reading order.
Substantial sections at the chapter level become cards, titled with the
container above them (\"Part One › Chapter A\") when there is one.  Every
other heading becomes a `§ TITLE' line appended to whatever precedes it,
so reading order survives and no plain heading is left inside a card."
  (let* ((segments (org-sm-import--segments (org-sm-import--clean text)))
         (split (org-sm-import--split-level segments))
         intro cards prefix)
    (cl-flet ((add (s)
                (when (org-string-nw-p s)
                  (if cards
                      (setcdr (car cards) (concat (cdar cards) "\n\n" s))
                    (push s intro))))
              (section (title body)
                (string-join (delq nil (list (concat "§ " title)
                                             (org-string-nw-p body)))
                             "\n\n")))
      (dolist (unit (if split (org-sm-import--units segments split) segments))
        (pcase-let ((`(,level ,title ,body) unit))
          (cond
           ((zerop level) (add body))
           ((and split (< level split))
            (setq prefix title)
            (when (org-string-nw-p body) (add (section title body))))
           ((and split (= level split) (org-sm-import--substantial-p unit))
            (push (cons (if prefix (concat prefix " › " title) title) body)
                  cards))
           (t (add (section title body)))))))
    (cons (string-join (nreverse intro) "\n\n") (nreverse cards))))

;;;###autoload
(defun org-sm-sectionize-card ()
  "Resolve the plain section headings inside the card at point.
Substantial sections become child topics, due one per day in reading order;
the rest fold into the text above them (see `org-sm-sectionize').  Existing
child cards are left alone.  Return the number of cards created."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (unless (org-sm-type) (user-error "Not on an org-sm card"))
    ;; Pin the card: inserting children moves point, and `org-sm--insert-child'
    ;; reads the priority from whichever heading point is on.
    (let* ((card (copy-marker (point)))
           (level (org-current-level))
           (start (car (org-sm-body-bounds)))
           (end (save-excursion
                  (let ((limit (save-excursion (org-end-of-subtree t t))))
                    (while (and (outline-next-heading) (< (point) limit)
                                (not (org-sm-type))))
                    (min (point) limit))))
           (plan (org-sm-sectionize (buffer-substring-no-properties start end)))
           (per-day (max 1 org-sm-import-topics-per-day))
           (n 0))
      (unwind-protect
          (progn
            (delete-region start end)
            (goto-char start)
            (when (org-string-nw-p (car plan)) (insert (car plan) "\n"))
            (dolist (unit (cdr plan))
              (save-excursion
                (goto-char card)
                (org-sm--insert-child level 'topic
                                      (concat org-sm-topic-prefix (car unit))
                                      (cdr unit) (org-id-new)
                                      (1+ (/ n per-day))))
              (setq n (1+ n)))
            (org-sm--card-changed 'sectionize)
            (when (called-interactively-p 'interactive)
              (message "org-sm: %d section card%s, the rest folded into text"
                       n (if (= n 1) "" "s")))
            n)
        (set-marker card nil)))))

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
  "Write TEXT as note NAME whose topic card is TITLE, imported from SOURCE.
TEXT must already have its headings one level below the card.  The card
is marked and sectionized by the same commands used on any card, so an
imported note obeys the same invariant.  Return (FILE . CARD-COUNT)."
  (let ((file (expand-file-name name org-sm-directory)))
    (with-temp-file file
      (insert (format "#+title:      %s\n#+date:       %s\n#+filetags:   :import:\n#+source: %s\n\n* %s\n%s\n\n%s\n"
                      title (format-time-string "[%F %a %H:%M]") source title
                      text
                      (org-sm-source-footer
                       (list :link source :label (format "《%s》" title))))))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (re-search-forward "^\\* ")
      (org-sm-item-mark 'topic)
      (let ((n (org-sm-sectionize-card)))
        (let ((save-silently t)) (save-buffer))
        (cons file n)))))

(defun org-sm-import--media (name)
  "Return the image directory, relative to the vault, for note NAME."
  (concat (file-name-base name) "-media"))

(defun org-sm-import--finish (result)
  "Visit the imported note in RESULT and report how it was split."
  (find-file (car result))
  (message "org-sm: imported as one topic with %d section card%s"
           (cdr result) (if (= (cdr result) 1) "" "s")))

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
  "Import FILE (EPUB, HTML, Markdown, DOCX or Org) as a note of topic cards.
An Org file saved by a Copy-as-Org browser exporter keeps its `#+TITLE' and
`#+SOURCE_URL'.  Refuses a second import of the same source: the existing
note is where its cards already live."
  (interactive (list (read-file-name "Import into org-sm: " nil nil t)))
  (unless org-sm-directory (user-error "Set `org-sm-directory' first"))
  (let* ((file (abbreviate-file-name (expand-file-name file)))
         (org-p (string-suffix-p ".org" file t))
         (parsed (and org-p (org-sm-import--org-file file)))
         (source (or (nth 1 parsed) file))
         (title (org-sm-import--clean-title
                 (or (nth 0 parsed) (org-sm-import--pandoc-title file)
                     (file-name-base file))))
         (name (org-sm-import--file-name title)))
    (when-let* ((done (org-sm-import--existing source)))
      (user-error "Already imported in %s" (file-name-nondirectory done)))
    (pcase-let* ((`(,text . ,tmp)
                  (if org-p
                      (cons (nth 2 parsed) nil)
                    (org-sm-import--pandoc
                     file (pcase (downcase (file-name-extension file))
                            ("md" "markdown") ("htm" "html") (ext ext))))))
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
  "Fetch the static page at URL and import it as a note of topic cards.
Pages that need JavaScript or a login extract better in the browser: save
them with a Copy-as-Org exporter and use `org-sm-import-file'."
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

(provide 'org-sm-import)
;;; org-sm-import.el ends here
