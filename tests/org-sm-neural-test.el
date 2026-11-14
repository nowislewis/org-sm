;;; org-sm-neural-test.el --- Spreading-activation sessions -*- lexical-binding: t; -*-
(require 'ert)
(require 'org-sm)
(require 'org-sm-neural)

(defconst org-sm-neural-test--vault
  (concat
   "* [T] Root\n:PROPERTIES:\n:ID: root\n:SRS_TYPE: topic\n:END:\n"
   "Body links to [[id:far][far]].\n"
   "** [T] Child A\n:PROPERTIES:\n:ID: a\n:SRS_TYPE: topic\n:END:\nA\n"
   "*** [C] Grandchild\n:PROPERTIES:\n:ID: g\n:SRS_TYPE: cloze\n:END:\n{{x}}\n"
   "** [T] Child B\n:PROPERTIES:\n:ID: b\n:SRS_TYPE: topic\n:END:\nB\n"
   "** [T] Gone :dismissed:\n:PROPERTIES:\n:ID: gone\n:SRS_TYPE: topic\n:END:\nZ\n"
   "*** [C] Behind dismissed\n:PROPERTIES:\n:ID: behind\n:SRS_TYPE: cloze\n:END:\n{{y}}\n"
   "* [T] Far\n:PROPERTIES:\n:ID: far\n:SRS_TYPE: topic\n:END:\nFar\n"
   "* [T] Citer\n:PROPERTIES:\n:ID: citer\n:SRS_TYPE: topic\n:END:\nsee [[id:a][<T>]]\n"
   "* [T] Stranger\n:PROPERTIES:\n:ID: s\n:SRS_TYPE: topic\n:END:\nS\n"))

(defmacro org-sm-neural-test--with-vault (&rest body)
  "Run BODY visiting a temp vault file containing the test tree."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "org-sm-neural-" t))
          (file (expand-file-name "v.org" dir))
          (org-sm-directory dir)
          (org-id-locations-file (expand-file-name "ids" dir))
          (org-id-track-globally t))
     (unwind-protect
         (progn
           (with-temp-file file (insert org-sm-neural-test--vault))
           (org-id-update-id-locations (list file) t)
           (with-current-buffer (find-file-noselect file)
             ,@body))
       (when-let* ((buf (get-file-buffer file)))
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf))
       (delete-directory dir t))))

(defun org-sm-neural-test--ids (markers)
  (mapcar (lambda (m) (org-with-point-at m (org-entry-get nil "ID"))) markers))

(ert-deftest org-sm-neural-structure-and-links ()
  (org-sm-neural-test--with-vault
    (goto-char (point-min))
    (re-search-forward ":ID: a")
    (org-back-to-heading t)
    (let ((ids (org-sm-neural-test--ids
                (org-sm-neural-collect (point-marker) 12))))
      ;; Seed first, then parent, own child, sibling; dismissed skipped.
      (should (equal (seq-take ids 4) '("a" "root" "g" "b")))
      (should-not (member "gone" ids))
      ;; ...but the walk passes through it to the cloze extracted below it.
      (should (member "behind" ids))
      ;; Reached via the root's forward link and Citer's backlink to `a'.
      (should (member "far" ids))
      (should (member "citer" ids))
      ;; Unrelated cards never join the cluster.
      (should-not (member "s" ids)))))

(ert-deftest org-sm-neural-respects-limit-and-seed ()
  (org-sm-neural-test--with-vault
    (goto-char (point-min))
    (re-search-forward ":ID: g")
    (org-back-to-heading t)
    (let ((ids (org-sm-neural-test--ids
                (org-sm-neural-collect (point-marker) 3))))
      (should (= (length ids) 3))
      (should (equal (car ids) "g"))
      (should (equal (cadr ids) "a")))))

(ert-deftest org-sm-neural-start-queues-cluster ()
  (org-sm-neural-test--with-vault
    (goto-char (point-min))
    (let (queued)
      (cl-letf (((symbol-function 'org-sm--review-markers)
                 (lambda (markers &rest _) (setq queued markers))))
        (org-sm-neural-start))
      (should (equal (car (org-sm-neural-test--ids queued)) "root"))
      (should (<= (length queued) org-sm-neural-size)))))

;;; org-sm-neural-test.el ends here
