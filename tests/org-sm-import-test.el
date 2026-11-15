;;; org-sm-import-test.el --- Sectionize and import -*- lexical-binding: t; -*-
(require 'ert)
(require 'org-sm)
(require 'org-sm-import)

(defvar org-sm-import-test--fixtures
  (or (getenv "ORG_SM_FIXTURES")
      (expand-file-name "fixtures" (file-name-directory load-file-name))))

(defmacro org-sm-import-test--with-vault (&rest body)
  "Run BODY with an empty temp vault bound to `org-sm-directory'."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "org-sm-import-" t))
          (org-sm-directory dir)
          (org-id-locations-file (expand-file-name "ids" dir)))
     (unwind-protect
         (cl-letf (((symbol-function 'find-file) #'ignore))
           ,@body)
       (dolist (buf (buffer-list))
         (when (and (buffer-file-name buf)
                    (file-in-directory-p (buffer-file-name buf) dir))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory dir t))))

(defun org-sm-import-test--note (dir)
  "Return the single imported note under DIR."
  (car (directory-files dir t "__import\\.org\\'")))

(defun org-sm-import-test--text (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun org-sm-import-test--count (regexp text)
  "Count matches of REGEXP in TEXT."
  (with-temp-buffer (insert text) (goto-char (point-min)) (how-many regexp)))

(defun org-sm-import-test--no-plain-heading-in-cards (file)
  "Assert the invariant: every heading inside a card is itself a card."
  (with-current-buffer (find-file-noselect file)
    (org-map-entries
     (lambda () (when (org-sm-type) (should (= (org-sm-section-count) 0)))))))

;;;; ---- The pure primitive -------------------------------------------------

(ert-deftest org-sm-sectionize-flattens-headings-into-section-lines ()
  (should (equal (org-sm-sectionize
                  (concat "Intro.\n<<anchor>>\n"
                          ":PROPERTIES:\n:CUSTOM_ID: x\n:END:\n"
                          "* Part One\n** Chapter A\nBody A.\n*** Section A1\nDeep.\n"))
                 "Intro.\n\n§ Part One\n\n§ Chapter A\n\nBody A.\n\n§ Section A1\n\nDeep.")))

;;;; ---- Applying it to a card --------------------------------------------------

(ert-deftest org-sm-sectionize-card-restores-the-invariant ()
  (org-sm-import-test--with-vault
    (let ((file (expand-file-name "a.org" dir)))
      (with-temp-file file
        (insert "* [#A] [T] Article\n:PROPERTIES:\n:ID: art\n:SRS_TYPE: topic\n:END:\n"
                "Intro.\n** One\nBody 1.\n** Two\nBody 2.\n"
                "** [C] Existing child\n:PROPERTIES:\n:SRS_TYPE: cloze\n:END:\n{{c}}\n"))
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (should (= (org-sm-section-count) 2))
        (should (= (org-sm-sectionize-card) 2))
        (goto-char (point-min))
        (should (= (org-sm-section-count) 0))
        (let ((card (org-sm-card-at-point)))
          ;; No child topics split off; the existing cloze child stays untouched.
          (should (= (plist-get card :child-topics) 0))
          (should (= (plist-get card :child-clozes) 1)))
        (should (equal (org-sm-body-text)
                       "Intro.\n\n§ One\n\nBody 1.\n\n§ Two\n\nBody 2."))
        (set-buffer-modified-p nil)))))

;;;; ---- File import ---------------------------------------------------------------

(ert-deftest org-sm-import-epub-follows-the-invariant ()
  (skip-unless (executable-find "pandoc"))
  (org-sm-import-test--with-vault
    (org-sm-import-file (expand-file-name "nested.epub" org-sm-import-test--fixtures))
    (let* ((note (org-sm-import-test--note dir))
           (text (org-sm-import-test--text note))
           (titles (mapcar (lambda (c) (plist-get c :title)) (org-sm-cards-in-file note))))
      ;; Imported as one single topic card; internal headings become `§ TITLE'.
      (should (equal titles '("[T] Nested Book")))
      (org-sm-import-test--no-plain-heading-in-cards note)
      (should (string-match-p "§ Copyright\n\nAll rights reserved." text))
      (should (string-match-p "§ Section A1\n\nDeep text inside chapter A." text))
      ;; Provenance once, in `:SOURCE:' on the book card; `#+source:' for routing.
      (should (= (org-sm-import-test--count "^:SOURCE:[ \t]+\\[\\[" text) 1))
      (should-not (string-match-p "来源：" text))
      (should (string-match-p "^#\\+source: " text))
      ;; The figure was copied next to the note and linked relatively.
      (should (string-match "\\[\\[file:\\([^]]+\\.png\\)\\]" text))
      (should (file-exists-p (expand-file-name (match-string 1 text) dir)))
      (should-not (string-match-p "<<\\|CUSTOM_ID" text)))))

(ert-deftest org-sm-import-org-file-from-a-browser-exporter ()
  ;; Copy-as-Org writes #+TITLE / #+SOURCE_URL and images under ./images/.
  (org-sm-import-test--with-vault
    (let* ((src (make-temp-file "org-sm-saved-" t))
           (saved (expand-file-name "page.org" src))
           (long (make-string 300 ?z)))
      (make-directory (expand-file-name "images" src))
      (with-temp-file (expand-file-name "images/pic.png" src) (insert "png"))
      (with-temp-file saved
        (insert "#+TITLE: 一篇\u200b文章\n#+DATE: 2026-11-14T00:00:00Z\n"
                "#+SOURCE_URL: https://example.org/post\n\n"
                "导语。\n\n[[./images/pic.png]]\n\n* 第一节\n" long "\n* 第二节\n" long "\n"))
      (unwind-protect
          (progn
            (org-sm-import-file saved)
            (let* ((note (org-sm-import-test--note dir))
                   (text (org-sm-import-test--text note)))
              ;; Title cleaned of the zero-width space; source kept for dedupe.
              (should (string-match-p "^#\\+title: +一篇 文章$" text))
              (should (string-match-p "^#\\+source: https://example.org/post$" text))
              ;; Kept intact as one long-form [#C] topic card with `§' headings.
              (should (equal (mapcar (lambda (c) (plist-get c :title))
                                     (org-sm-cards-in-file note))
                             '("[T] 一篇 文章")))
              (org-sm-import-test--no-plain-heading-in-cards note)
              (should (string-match-p "§ 第一节" text))
              (should (string-match-p "§ 第二节" text))
              (should (equal (mapcar (lambda (c) (plist-get c :priority))
                                     (org-sm-cards-in-file note))
                             '(?C)))
              (should (string-match "\\[\\[file:\\([^]]+\\.png\\)\\]" text))
              (should (file-exists-p (expand-file-name (match-string 1 text) dir))))
            ;; The same URL fetched later is recognised as already imported.
            (should-error (org-sm-import-url "https://example.org/post")
                          :type 'user-error))
        (delete-directory src t)))))

(ert-deftest org-sm-import-routes-later-captures-to-the-book ()
  (skip-unless (executable-find "pandoc"))
  (org-sm-import-test--with-vault
    (let* ((epub (expand-file-name "nested.epub" org-sm-import-test--fixtures))
           (inbox (expand-file-name "inbox.org" dir)))
      (with-temp-file inbox (insert "* inbox\n"))
      (org-sm-import-file epub)
      (should (equal (org-sm-source-target-file
                      (list :kind 'epub :file (abbreviate-file-name epub)) inbox)
                     (org-sm-import-test--note dir))))))

(ert-deftest org-sm-import-refuses-duplicates ()
  (skip-unless (executable-find "pandoc"))
  (org-sm-import-test--with-vault
    (let ((epub (expand-file-name "nested.epub" org-sm-import-test--fixtures)))
      (org-sm-import-file epub)
      (should-error (org-sm-import-file epub) :type 'user-error)
      (should (= (length (directory-files dir nil "__import\\.org\\'")) 1)))))

(ert-deftest org-sm-import-file-name-fits-the-file-system ()
  (let ((name (org-sm-import--file-name
               (apply #'concat (make-list 40 "一个非常长的中文标题")))))
    (should (<= (string-bytes name) 255))
    (should (string-suffix-p "__import.org" name))))

;;;; ---- Web import ---------------------------------------------------------------

(ert-deftest org-sm-import-url-cleans-the-page ()
  (skip-unless (executable-find "pandoc"))
  (org-sm-import-test--with-vault
    (let* ((para (mapconcat (lambda (i) (format "word%d" i)) (number-sequence 1 80) " "))
           (html (format "<html><head><title>An Article</title></head><body>
<nav>Home About Login</nav><article><h1>An Article</h1>
<p>%s <a href=\"/rel/page\">rel</a></p>
<img src=\"data:image/gif;base64,R0l\" data-src=\"/img/a.png\">
<h2>Intro</h2><p>%s</p><h2>Body</h2><p>%s</p></article>
<footer>copyright footer</footer></body></html>" para para para)))
      (cl-letf (((symbol-function 'org-sm-import--fetch) (lambda (_) html))
                ;; No network: pretend the image downloads.
                ((symbol-function 'url-copy-file)
                 (lambda (_url dest &rest _) (with-temp-file dest (insert "png")))))
        (org-sm-import-url "https://example.org/a"))
      (let ((text (org-sm-import-test--text (org-sm-import-test--note dir))))
        (should (string-match-p "^#\\+title: +An Article" text))
        ;; The heading repeating the title is dropped; sections become `§'.
        (should (= (org-sm-import-test--count "^\\* \\[#C\\] \\[T\\]" text) 1))
        (should (string-match-p "§ Intro" text))
        (should (string-match-p "§ Body" text))
        (should-not (string-match-p "Login\\|copyright footer" text))
        ;; Relative link made absolute; lazy image fetched and localised.
        (should (string-match-p "\\[\\[https://example.org/rel/page\\]" text))
        (should-not (string-match-p "data:image" text))
        (should (string-match-p "\\[\\[file:[^]]+-media/img-1\\.png\\]\\]" text))))))

(ert-deftest org-sm-import-txt-and-org-note ()
  (org-sm-import-test--with-vault
    (let ((txt (expand-file-name "plain.txt" dir))
          (org (expand-file-name "notes.org" dir))
          (long (make-string 300 ?k)))
      (with-temp-file txt (insert "Plain paragraph.\n* Fake heading\n"))
      (org-sm-import-file txt)
      (let* ((note (org-sm-import-test--note dir))
             (text (org-sm-import-test--text note)))
        (should (equal (mapcar (lambda (c) (plist-get c :title))
                               (org-sm-cards-in-file note))
                       '("[T] plain")))
        (should (string-match-p "^,\\* Fake heading$" text))
        (delete-file note))
      ;; An Org file with multiple sections imports as one long card with its
      ;; subheadings rewritten to `§ TITLE'.
      (with-temp-file org
        (insert "#+TITLE: Long Note\n* Sec 1\n" long "\n* Sec 2\n" long "\n"))
      (org-sm-import-file org)
      (let* ((note (org-sm-import-test--note dir))
             (text (org-sm-import-test--text note)))
        (should (equal (mapcar (lambda (c) (plist-get c :title))
                               (org-sm-cards-in-file note))
                       '("[T] Long Note")))
        (should (string-match-p "§ Sec 1" text))
        (should (string-match-p "§ Sec 2" text))
        (org-sm-import-test--no-plain-heading-in-cards note)))))

(ert-deftest org-sm-import-highlights-dedupes-prioritizes-and-spreads ()
  (org-sm-import-test--with-vault
    (let* ((org-sm-capture-file (expand-file-name "inbox.org" dir))
           (org-sm-capture-olp '("sm-cards"))
           (items
            (list
             (list :key "bookmark/b1/m1"
                   :text "第一条微信读书划线"
                   :olp '("sm-cards" "书A" "第一章")
                   :source "[[weread:bookmark/b1/m1][书A / 作者 § 第一章]]")
             (list :key "ANNO1234"
                   :text "第一条 Zotero 高亮"
                   :olp '("sm-cards" "BookB")
                   :source "[[zotero://open-pdf/library/items/ATT1?page=5&annotation=ANNO1234][《BookB》 p.5]]"))))
      (should (= (org-sm-import-highlights items) 2))
      ;; Re-importing the same highlights is a no-op; adding one new highlight
      ;; imports only the new one.
      (should (= (org-sm-import-highlights
                  (append items
                          (list (list :key "ANNO5678"
                                      :text "第二条 Zotero 高亮"
                                      :olp '("sm-cards" "BookB")
                                      :source "[[zotero://open-pdf/library/items/ATT1?annotation=ANNO5678][《BookB》]]"))))
                 1))
      (let ((cards (org-sm-cards-in-file org-sm-capture-file))
            dates)
        (should (= (length cards) 3))
        (should (equal (mapcar (lambda (c) (plist-get c :priority)) cards)
                       '(?C ?C ?C)))
        (with-current-buffer (find-file-noselect org-sm-capture-file)
          (org-map-entries
           (lambda ()
             (when (org-sm-type)
               (push (org-sm-sched-day-delta) dates)))))
        ;; First batch spread across days 1 and 2; second batch has 1 card at day 1.
        (should (equal (sort dates #'<) '(1 1 2)))))))

;;; org-sm-import-test.el ends here
