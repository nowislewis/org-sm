;;; org-sm-web-test.el --- Regression tests for mobile writes -*- lexical-binding: t; -*-

(require 'ert)
;; Load sources explicitly: local .elc files can lag during development.
(let ((root (file-name-directory (or load-file-name buffer-file-name))))
  (load-file (expand-file-name "../org-sm.el" root))
  (load-file (expand-file-name "../org-sm-web.el" root)))

(defmacro org-sm-web-test--with-card (&rest body)
  "Run BODY on an in-memory scheduled topic without touching real notes."
  (declare (indent 0))
  `(with-temp-buffer
     (org-mode)
     (insert "* [T] Example\nSCHEDULED: <2026-09-24 Thu>\n:PROPERTIES:\n:SRS_TYPE: topic\n:END:\n😀alpha beta\n")
     (goto-char (point-min))
     ,@body))

(ert-deftest org-sm-web-test-postpone-parses-days ()
  (org-sm-web-test--with-card
    (let ((event (org-sm-web--apply-review nil "postpone" "7")))
      (should (= (plist-get event :postpone-days) 7))
      (should (plist-get event :new-days)))
    (should-error (org-sm-web--apply-review nil "postpone" "7garbage")
                  :type 'user-error)
    (should-error (org-sm-web--apply-review nil "postpone" "0")
                  :type 'user-error)))

(ert-deftest org-sm-web-test-revision-detects-text-title-and-schedule ()
  (org-sm-web-test--with-card
    (let ((original (org-sm-web--revision)))
      (org-sm-web--check-revision original)
      (org-sm-set-body "new body")
      (should-error (org-sm-web--check-revision original)
                    :type 'org-sm-web-conflict))
    (let ((original (org-sm-web--revision)))
      (org-edit-headline "Renamed")
      (should-error (org-sm-web--check-revision original)
                    :type 'org-sm-web-conflict))
    (goto-char (point-min))
    (let ((original (org-sm-web--revision)))
      (org-sm-web--apply-review nil "reschedule" nil)
      (should-error (org-sm-web--check-revision original)
                    :type 'org-sm-web-conflict))))

(ert-deftest org-sm-web-test-existing-draft-is-not-erased ()
  (let ((name "*org-sm test draft*"))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create name) (insert "unsaved draft"))
          (should-error (org-sm--capture-buffer "replacement" name)
                        :type 'user-error)
          (with-current-buffer name
            (should (equal (buffer-string) "unsaved draft"))))
      (when (get-buffer name) (kill-buffer name)))))

(ert-deftest org-sm-web-test-git-commit-does-not-include-other-staged-paths ()
  (let* ((root (make-temp-file "org-sm-git-test-" t))
         (notes (expand-file-name "notes" root))
         (org-sm-directory notes))
    (unwind-protect
        (progn
          (make-directory notes)
          (org-sm-web--git-run root "init" "-q")
          (org-sm-web--git-run root "config" "user.name" "Test")
          (org-sm-web--git-run root "config" "user.email" "test@example.org")
          (with-temp-file (expand-file-name "card.org" notes) (insert "before\n"))
          (with-temp-file (expand-file-name "other.txt" root) (insert "before\n"))
          (org-sm-web--git-run root "add" "-A")
          (org-sm-web--git-run root "commit" "-qm" "initial")
          (with-temp-file (expand-file-name "card.org" notes) (insert "after\n"))
          (with-temp-file (expand-file-name "new.org" notes) (insert "new card\n"))
          (with-temp-file (expand-file-name "other.txt" root) (insert "after\n"))
          (org-sm-web--git-run root "add" "other.txt")
          (should (string-match-p "card.org" (plist-get (org-sm-web--git-diff) :diff)))
          ;; No remote: push must report failure, but the commit already exists.
          (should-error (org-sm-web--git-push) :type 'error)
          (let ((files (org-sm-web--git-run root "show" "--pretty=format:" "--name-only" "HEAD")))
            (should (equal files "notes/card.org\nnotes/new.org")))
          (should (equal "other.txt"
                         (org-sm-web--git-run root "diff" "--cached" "--name-only"))))
      (delete-directory root t))))

(provide 'org-sm-web-test)
;;; org-sm-web-test.el ends here

;;;; ---- Browser reading: read point, images, priority ----------------------

(ert-deftest org-sm-web-test-readpoint-roundtrip ()
  (org-sm-web-test--with-card
    (org-entry-put nil "ID" "card-rp")
    (let* ((before (org-sm-body-text))
           (offset (string-search "beta" before)))
      ;; No anchor yet: the snapshot must say so rather than guess.
      (should-not (org-sm-web--readpoint-offset))
      (org-sm-web--set-readpoint offset)
      ;; Offset is measured on the cleaned body the browser was given.
      (should (= (org-sm-web--readpoint-offset) offset))
      (should (string-match-p "\\[\\[rp:" (org-sm--readpoint-anchor (org-id-get))))
      ;; Moving the anchor must not leave the old one behind.
      (org-sm-web--set-readpoint 0)
      (should (= (org-sm-web--readpoint-offset) 0))
      (should (= (count-matches "\\[\\[rp:" (point-min) (point-max)) 1))
      ;; The anchor stays in the body (the browser hides it at render time
      ;; and reports its offset), so the two views keep one text.
      (should (string-match-p "\\[\\[rp:" (org-sm-body-text))))))

(ert-deftest org-sm-web-test-image-path-is-confined ()
  (let* ((root (make-temp-file "org-sm-img-" t))
         (notes (expand-file-name "notes" root))
         (shared (expand-file-name "pics" root))
         (org-sm-directory notes)
         (org-sm-web-image-roots (list shared))
         (in (expand-file-name "a.png" notes))
         (out (expand-file-name "b.png" root))
         (allowed (expand-file-name "c.png" shared)))
    (make-directory notes) (make-directory shared)
    (unwind-protect
        (progn
          (dolist (f (list in out allowed))
            (with-temp-file f (insert "x")))
          (should (equal (org-sm-web--image-path in) in))
          (should (equal (org-sm-web--image-path allowed) allowed))
          ;; Outside every root, or not an image: refused.
          (should-error (org-sm-web--image-path out) :type 'user-error)
          (should-error (org-sm-web--image-path "/etc/passwd") :type 'user-error)
          (with-temp-file (expand-file-name "notes.txt" notes) (insert "x"))
          (should-error (org-sm-web--image-path (expand-file-name "notes.txt" notes))
                        :type 'user-error))
      (delete-directory root t))))

(ert-deftest org-sm-web-test-card-snapshot-carries-reading-state ()
  (let* ((dir (make-temp-file "org-sm-snap-" t))
         (org-sm-directory dir)
         (org-sm-web-image-roots nil)
         (file (expand-file-name "n.org" dir))
         (img (expand-file-name "pic.png" dir)))
    (unwind-protect
        (progn
          (with-temp-file img (insert "png"))
          (with-temp-file file
            (insert "* [T] Read me\n:PROPERTIES:\n:ID: card1\n:SRS_TYPE: topic\n"
                    ":END:\n首段\n[[file:pic.png][图示]]\nsecond\n"))
          (with-current-buffer (find-file-noselect file)
            (goto-char (point-min))
            (org-sm-web--set-readpoint 0)
            (let ((card (org-sm-web--card-at-point)))
              (should (= (plist-get card :readpoint) 0))
              (let ((image (aref (plist-get card :images) 0)))
                (should (= (length (plist-get card :images)) 1))
                (should (equal (plist-get image :alt) "图示"))
                (should (string-match-p "\\`/api/image\\?path=" (plist-get image :url)))))
            (set-buffer-modified-p nil))
        (when-let* ((b (get-file-buffer file))) (kill-buffer b))
        (delete-directory dir t)))))

(ert-deftest org-sm-web-test-priority-endpoint ()
  (org-sm-web-test--with-card
    (should-not (plist-get (org-sm-card-at-point) :priority))
    (let ((letter (aref "A" 0)))
      (org-priority letter))
    (should (eq (plist-get (org-sm-card-at-point) :priority) ?A))
    (should-error (org-priority (aref "Z" 0)) :type 'error)))

(ert-deftest org-sm-web-test-readpoint-moves-forward-past-itself ()
  ;; The browser's offset counts the old anchor; moving forward must still
  ;; land on the tapped character, not one anchor-length too far.
  (org-sm-web-test--with-card
    (org-entry-put nil "ID" "card-fw")
    (org-sm-web--set-readpoint 0)
    (let* ((body (org-sm-body-text))
           (target (string-search "beta" body)))
      (org-sm-web--set-readpoint target)
      (should (string-match-p "alpha \\[\\[rp:card-fw\\]\\[📖\\]\\]beta"
                              (org-sm-body-text))))))
