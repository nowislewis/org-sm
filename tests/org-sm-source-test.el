;;; org-sm-source-test.el --- Provenance, routing, page text -*- lexical-binding: t; -*-
(require 'ert)
(require 'org-sm)

(defvar org-sm-source-test--fixtures
  (or (getenv "ORG_SM_FIXTURES")
      (expand-file-name "fixtures" (file-name-directory load-file-name)))
  "Directory holding text.pdf / book.epub / scan.pdf fixtures.")

(defun org-sm-source-test--fixture (name)
  (expand-file-name name org-sm-source-test--fixtures))

(defmacro org-sm-source-test--with-vault (files &rest body)
  "Run BODY with a temp vault containing FILES, an alist of (NAME . TEXT)."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "org-sm-source-" t))
          (org-sm-directory dir)
          (org-sm-capture-file (expand-file-name "inbox.org" dir))
          (org-sm-capture-olp '("inbox")))
     (unwind-protect
         (progn
           (dolist (cell ,files)
             (with-temp-file (expand-file-name (car cell) dir)
               (insert (cadr cell))))
           ,@body)
       (dolist (buf (buffer-list))
         (when (and (buffer-file-name buf)
                    (file-in-directory-p (buffer-file-name buf) dir))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory dir t))))

;;;; ---- Context detection ---------------------------------------------------

(ert-deftest org-sm-source-context-org-buffer-is-nil ()
  ;; Inside an org note, extraction links child to parent already; a page
  ;; footer would be noise.
  (with-temp-buffer
    (org-mode)
    (insert "#+title: 笔记\n* Card\n")
    (should-not (org-sm-source-context))))

(ert-deftest org-sm-source-context-file ()
  (with-temp-buffer
    (insert "plain text\nsecond line\n")
    (setq buffer-file-name "/tmp/notes.txt")
    (goto-char (point-min))
    (forward-line 1)
    (let ((ctx (org-sm-source-context)))
      (should (eq (plist-get ctx :kind) 'file))
      (should (string-match-p "notes.txt:2" (plist-get ctx :label)))
      (should (string-match-p "\\`file:/tmp/notes.txt::2\\'" (plist-get ctx :link)))
      (should (string-match-p "notes.txt:2" (org-sm-source-link ctx))))))

(ert-deftest org-sm-source-context-web ()
  (with-temp-buffer
    (setq-local major-mode 'eww-mode)
    (setq-local eww-data (list :url "https://example.org/a" :title "A Page"))
    (let ((ctx (org-sm-source-context)))
      (should (eq (plist-get ctx :kind) 'web))
      (should (equal (plist-get ctx :link) "https://example.org/a"))
      (should (string-match-p "A Page" (plist-get ctx :label))))))

(ert-deftest org-sm-source-context-epub ()
  (with-temp-buffer
    (setq-local major-mode 'nov-mode)
    (setq-local nov-file-name "/books/x.epub"
                nov-documents-index 3
                nov-metadata (list (cons 'title "一本书"))
                buffer-file-name "/books/x.epub")
    (goto-char (point-min))
    (insert "text")
    (let ((ctx (org-sm-source-context)))
      (should (eq (plist-get ctx :kind) 'epub))
      ;; nov's own link form: file::document:point
      (should (string-match-p "\\`nov:/books/x.epub::3:[0-9]+\\'" (plist-get ctx :link)))
      (should (string-match-p "一本书" (plist-get ctx :label))))))

;;;; ---- Routing --------------------------------------------------------------

(ert-deftest org-sm-source-routes-to-the-file-already-citing-the-document ()
  (org-sm-source-test--with-vault
      '(("inbox.org" "* inbox\n")
        ("book.org" "* 书\n见 [[page:/books/x.pdf::12][p12]]\n")
        ("other.org" "* 别的书\n"))
    (let* ((ctx (list :kind 'pdf :file "/books/x.pdf" :page 12))
           (target (org-sm-source-target-file ctx (org-sm-capture-target-file))))
      (should (equal (file-name-nondirectory target) "book.org")))))

(ert-deftest org-sm-source-routes-by-bibliography-reference ()
  (org-sm-source-test--with-vault
      '(("inbox.org" "* inbox\n")
        ("20260101--book__bib.org" "#+title: book\n#+reference: knuth_1997_taocp\n* 笔记\n"))
    (let* ((ctx (list :kind 'pdf :file "/books/knuth_1997_taocp.pdf"))
           (target (org-sm-source-target-file ctx (org-sm-capture-target-file))))
      ;; citar-managed PDFs are named by citekey, so the note is found by
      ;; #+reference: even though no card links the file yet.
      (should (equal (file-name-nondirectory target) "20260101--book__bib.org")))))

(ert-deftest org-sm-source-never-routes-into-the-inbox ()
  (org-sm-source-test--with-vault
      '(("inbox.org" "* inbox\n见 [[page:/books/x.pdf::1][p1]]\n"))
    (let ((ctx (list :kind 'pdf :file "/books/x.pdf" :page 1)))
      ;; The inbox cites the document, but it is an inbox: no home yet.
      (should-not (org-sm-source-target-file ctx (org-sm-capture-target-file))))))

;;;; ---- Page text ------------------------------------------------------------

(ert-deftest org-sm-source-page-text-reads-the-text-layer ()
  (skip-unless (executable-find "mutool"))
  (skip-unless (file-exists-p (org-sm-source-test--fixture "text.pdf")))
  (let ((text (org-sm-source-page-text (org-sm-source-test--fixture "text.pdf") 1)))
    (should (string-match-p "动态规划" text))
    (should-not (string-match-p "\f" text))))

(ert-deftest org-sm-source-page-text-is-nil-on-a-scan ()
  ;; No OCR: a scanned page has no text layer, so there is nothing to offer.
  (skip-unless (executable-find "mutool"))
  (skip-unless (file-exists-p (org-sm-source-test--fixture "scan.pdf")))
  (should-not (org-sm-source-page-text (org-sm-source-test--fixture "scan.pdf") 1)))

(ert-deftest org-sm-source-capture-stores-source-property-and-routes ()
  (org-sm-source-test--with-vault
      '(("inbox.org" "* inbox\n")
        ("book.org" "* 书\n见 [[page:/books/x.pdf::3][p3]]\n"))
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'org-sm-source-context)
               (lambda () (list :kind 'pdf :file "/books/x.pdf" :page 3))))
      (let* ((ctx (list :kind 'pdf :file "/books/x.pdf" :page 3
                        :link "page:/books/x.pdf::3" :label "《x》p.3"))
             (buf (org-sm--capture-buffer "正文" nil nil ctx)))
        (unwind-protect
            (with-current-buffer buf
              ;; Body stays pure; provenance lives in `org-sm-capture-source'
              ;; and is written to `:SOURCE:' on commit.
              (should (equal (buffer-string) "正文"))
              (should (equal org-sm-capture-source "[[page:/books/x.pdf::3][《x》p.3]]"))
              (should (equal (file-name-nondirectory org-sm-capture-file) "book.org"))
              (org-sm-capture-commit))
          (when (buffer-live-p buf) (kill-buffer buf)))
        (with-current-buffer (find-file-noselect (expand-file-name "book.org" dir))
          (goto-char (point-min))
          (re-search-forward ":SRS_TYPE: topic")
          (should (equal (org-entry-get nil "SOURCE")
                         "[[page:/books/x.pdf::3][《x》p.3]]"))
          (should (equal (org-sm-body-text) "正文")))
        ;; Routing is buffer-local: the global target is untouched.
        (should (equal (file-name-nondirectory (org-sm-capture-target-file))
                       "inbox.org"))))))

;;; org-sm-source-test.el ends here
