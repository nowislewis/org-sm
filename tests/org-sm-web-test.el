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
    (should (= (plist-get (org-sm-web--apply-review nil "postpone" "7")
                          :interval-days) 7))
    (should-error (org-sm-web--apply-review nil "postpone" "7garbage")
                  :type 'user-error)
    (should-error (org-sm-web--apply-review nil "postpone" "0")
                  :type 'user-error)))

(ert-deftest org-sm-web-test-revision-detects-text-title-and-schedule ()
  (org-sm-web-test--with-card
    (let ((original (org-sm-web--revision)))
      (org-sm-web--check-revision original)
      (org-sm--set-body "new body")
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
