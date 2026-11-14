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

(ert-deftest org-sm-import-split-level-skips-empty-containers ()
  (let* ((long (make-string 300 ?x))
         (segs `((1 "Copyright" "short") (1 "Part" "") (2 "Ch A" ,long)
                 (3 "Sec" "deep") (2 "Ch B" ,long) (1 "Part 2" "") (2 "Ch C" ,long))))
    (should (= (org-sm-import--split-level segs) 2)))
  (let ((long (make-string 300 ?x)))
    (should (= (org-sm-import--split-level `((1 "A" ,long) (1 "B" ,long))) 1))
    ;; One long section is not worth splitting off.
    (should-not (org-sm-import--split-level `((1 "A" ,long) (1 "B" "short"))))))

(ert-deftest org-sm-sectionize-plans-cards-and-folds-the-rest ()
  (let* ((long (make-string 300 ?x))
         (plan (org-sm-sectionize
                (concat "Intro.\n* Copyright\nAll rights reserved.\n* Part One\n"
                        "** Chapter A\n" long "\n*** Section A1\nDeep.\n"
                        "** Short\ntiny\n** Chapter B\n" long "\n"))))
    ;; Intro keeps the text before any card, plus short material in order.
    (should (equal (car plan) "Intro.\n\n§ Copyright\n\nAll rights reserved."))
    (should (equal (mapcar #'car (cdr plan))
                   '("Part One › Chapter A" "Part One › Chapter B")))
    ;; Deeper headings and short siblings fold into the preceding card.
    (should (string-match-p "§ Section A1\n\nDeep.\n\n§ Short\n\ntiny\\'"
                            (cdr (nth 1 plan)))))
  ;; Nothing substantial: everything folds into one body, no cards.
  (let ((plan (org-sm-sectionize "Intro\n* A\nshort\n* B\nshort")))
    (should (null (cdr plan)))
    (should (equal (car plan) "Intro\n\n§ A\n\nshort\n\n§ B\n\nshort"))))

;;;; ---- Applying it to a card --------------------------------------------------

(ert-deftest org-sm-sectionize-card-restores-the-invariant ()
  (org-sm-import-test--with-vault
    (let ((file (expand-file-name "a.org" dir))
          (long (make-string 300 ?y)))
      (with-temp-file file
        (insert "* [#A] [T] Article\n:PROPERTIES:\n:ID: art\n:SRS_TYPE: topic\n:END:\n"
                "Intro.\n** One\n" long "\n** Two\n" long "\n** Tiny\nx\n"
                "** [C] Existing child\n:PROPERTIES:\n:SRS_TYPE: cloze\n:END:\n{{c}}\n"))
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (should (= (org-sm-section-count) 3))
        (should (= (org-sm-sectionize-card) 2))
        (goto-char (point-min))
        (should (= (org-sm-section-count) 0))
        (let ((card (org-sm-card-at-point)))
          ;; Two new topics plus the cloze that was already there.
          (should (= (plist-get card :child-topics) 2))
          (should (= (plist-get card :child-clozes) 1)))
        (should (string-match-p "\\`Intro.\\'" (org-sm-body-text)))
        ;; Children inherit priority and come due one per day.
        (let ((dates nil))
          (org-map-entries
           (lambda ()
             (when (equal (org-sm-type) 'topic)
               (unless (equal (org-entry-get nil "ID") "art")
                 (should (eq (plist-get (org-sm-card-at-point) :priority) ?A))
                 (push (org-sm-sched-day-delta) dates)))))
          (should (equal (sort dates #'<) '(1 2))))
        (should (string-match-p "§ Tiny\n\nx" (buffer-string)))
        (set-buffer-modified-p nil)))))

;;;; ---- File import ---------------------------------------------------------------

(ert-deftest org-sm-import-epub-follows-the-invariant ()
  (skip-unless (executable-find "pandoc"))
  (org-sm-import-test--with-vault
    (org-sm-import-file (expand-file-name "nested.epub" org-sm-import-test--fixtures))
    (let* ((note (org-sm-import-test--note dir))
           (text (org-sm-import-test--text note))
           (titles (mapcar (lambda (c) (plist-get c :title)) (org-sm-cards-in-file note))))
      ;; One topic for the book; chapters below it, titled with their part.
      (should (equal titles '("[T] Nested Book"
                              "[T] Part One › Chapter A"
                              "[T] Part One › Chapter B"
                              "[T] Part Two › Chapter C")))
      (org-sm-import-test--no-plain-heading-in-cards note)
      ;; Front matter and the short copyright page land in the book's body.
      (should (string-match-p "§ Copyright\n\nAll rights reserved." text))
      (should (string-match-p "§ Section A1\n\nDeep text inside chapter A." text))
      ;; Provenance once, on the book card; source keyword for routing.
      (should (= (org-sm-import-test--count "来源：\\[\\[" text) 1))
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
              (should (equal (mapcar (lambda (c) (plist-get c :title))
                                     (org-sm-cards-in-file note))
                             '("[T] 一篇 文章" "[T] 第一节" "[T] 第二节")))
              (org-sm-import-test--no-plain-heading-in-cards note)
              ;; Imports start unproven: the article and every section at [#C].
              (should (equal (mapcar (lambda (c) (plist-get c :priority))
                                     (org-sm-cards-in-file note))
                             '(?C ?C ?C)))
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
        ;; The heading repeating the title is dropped: two section cards.
        (should (= (org-sm-import-test--count "^\\*\\* \\[#C\\] \\[T\\]" text) 2))
        (should-not (string-match-p "Login\\|copyright footer" text))
        ;; Relative link made absolute; lazy image fetched and localised.
        (should (string-match-p "\\[\\[https://example.org/rel/page\\]" text))
        (should-not (string-match-p "data:image" text))
        (should (string-match-p "\\[\\[file:[^]]+-media/img-1\\.png\\]\\]" text))))))

;;; org-sm-import-test.el ends here
