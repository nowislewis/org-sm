;;; org-sm-gptel-test.el --- Workbench async safety tests -*- lexical-binding: t; -*-

(require 'ert)
(load-file (expand-file-name "../org-sm.el"
                             (file-name-directory (or load-file-name buffer-file-name))))
(load-file (expand-file-name "../org-sm-gptel.el"
                             (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest org-sm-gptel-test-async-response-does-not-erase-manual-edits ()
  (with-temp-buffer
    (org-sm-capture-mode)
    (insert "draft")
    (let (callback)
      (unwind-protect
          (progn
            ;; A live marker is sufficient to identify a workbench buffer.
            (setq-local org-sm-capture-parent-marker (point-marker))
            (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "refine"))
                      ((symbol-function 'org-sm-gptel--card-request)
                       (lambda (_text _type _mode _custom cb) (setq callback cb))))
              (org-sm-gptel-card-ai)
              (insert " user edit")
              (funcall callback "AI result" nil nil)
              (should (equal (buffer-string) "draft user edit"))))
        (set-marker org-sm-capture-parent-marker nil)))))

(provide 'org-sm-gptel-test)
;;; org-sm-gptel-test.el ends here
