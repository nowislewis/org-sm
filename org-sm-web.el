;;; org-sm-web.el --- Web review front-end for org-sm -*- lexical-binding: t -*-

;; Author: Lewis Liu
;; Package-Requires: ((emacs "29.1") (org-sm "0.1") (simple-httpd "1.6"))
;; Keywords: org, spaced-repetition, web

;;; Commentary:
;;
;; Minimal HTTP/JSON front-end for reviewing due org-sm cards from a browser.
;; It deliberately does not edit, capture, extract, or invoke AI: those write
;; workflows stay in Emacs, leaving this module with one mutation endpoint.
;;
;; Usage:
;;   (require 'org-sm-web)
;;   (setq org-sm-web-bind "0.0.0.0") ; explicit opt-in for phone/LAN access
;;   (org-sm-web-start)

;;; Code:

(require 'org)
(require 'org-id)
(require 'json)
(require 'simple-httpd)
(require 'org-sm)

(defgroup org-sm-web nil
  "Web review front-end for org-sm."
  :group 'org-sm
  :prefix "org-sm-web-")

(defcustom org-sm-web-port 8842
  "Port for the org-sm web server."
  :type 'integer)

(defcustom org-sm-web-root
  (expand-file-name "web/" (file-name-directory (or load-file-name buffer-file-name "")))
  "Directory holding the static web UI."
  :type 'directory)

(defcustom org-sm-web-bind "127.0.0.1"
  "Address the server binds to.
The default is localhost because this unauthenticated server can read and
modify cards.  Set it explicitly to \"0.0.0.0\" to review from a trusted LAN."
  :type '(choice (const :tag "Localhost only" "127.0.0.1")
                 (const :tag "All interfaces (LAN)" "0.0.0.0")
                 (string :tag "Specific IP")))

(defun org-sm-web--json (object)
  "Encode OBJECT as compact JSON."
  (let ((json-encoding-pretty-print nil))
    (json-encode object)))

(defmacro org-sm-web--with-card (id &rest body)
  "Locate card ID, run BODY on its heading, and release the marker."
  (declare (indent 1) (debug (form body)))
  (cl-with-gensyms (marker)
    `(let ((,marker (org-id-find ,id 'marker)))
       (unless ,marker (user-error "No card with id %s" ,id))
       (org-with-point-at ,marker
         (prog1 (progn ,@body)
           (set-marker ,marker nil))))))

(defun org-sm-web--parse-cloze (body)
  "Return BODY as JSON-ready literal/cloze tokens."
  (let ((tokens nil) (pos 0))
    (while (string-match org-sm--cloze-regexp body pos)
      (let ((beg (match-beginning 0)) (end (match-end 0)))
        (when (> beg pos)
          (push (list :text (substring body pos beg)) tokens))
        (push (list :cloze (match-string 1 body)) tokens)
        (setq pos end)))
    (when (< pos (length body))
      (push (list :text (substring body pos)) tokens))
    (apply #'vector (nreverse tokens))))

(defun org-sm-web--intervals ()
  "Return current card's rating labels without changing its schedule."
  (pcase (org-sm-type)
    ('cloze
     (let (out)
       (dolist (entry (org-sm--cloze-preview) (nreverse out))
         (push (car entry) out)
         (push (org-sm--secs-label (plist-get (cdr entry) :interval-secs)) out))))
    ('topic (list :reschedule
                  (format "%dd" (org-sm--topic-read (org-sm--topic-afactor)))))))

(defun org-sm-web--card-at-point ()
  "Serialize the current org-sm card for the browser."
  (let* ((type (org-sm-type))
         (bounds (org-sm--body-bounds))
         (body (org-sm--body-clean
                (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    (list :id (org-id-get-create)
          :type (symbol-name type)
          :title (org-get-heading t t t t)
          :body body
          :clozes (if (eq type 'cloze) (org-sm-web--parse-cloze body) [])
          :intervals (org-sm-web--intervals))))

(defun org-sm-web--due-list ()
  "Return JSON-ready summaries of all due cards."
  (apply #'vector
         (org-sm--map-items
          (lambda ()
            (list :id (org-id-get-create)
                  :title (org-get-heading t t t t)))
          #'org-sm--due-p)))

(defmacro org-sm-web--json-servlet (endpoint args &rest body)
  "Define JSON ENDPOINT whose BODY returns an encodable object."
  (declare (indent 2) (debug (form sexp body)))
  `(httpd-servlet* ,endpoint application/json ,args
     (condition-case err
         (insert (org-sm-web--json (progn ,@body)))
       (error (insert (org-sm-web--json (list :error (error-message-string err))))))))

(org-sm-web--json-servlet api/queue ()
  (org-sm-web--due-list))

(org-sm-web--json-servlet api/card/:id ()
  (org-sm-web--with-card id
    (org-sm-web--card-at-point)))

(defun org-sm-web--rating (rating)
  "Validate web RATING and return its keyword form."
  (let ((value (intern (concat ":" (or rating "")))))
    (unless (memq value org-sm-cloze-ratings)
      (user-error "Unknown cloze rating: %s" rating))
    value))

(defun org-sm-web--apply-review (rating action days)
  "Apply the browser's review decision to the card at point."
  (pcase (org-sm-type)
    ('cloze (org-sm--cloze-grade (org-sm-web--rating rating)))
    ('topic (pcase action
              ("reschedule" (org-sm--topic-grade))
              ("postpone" (org-sm--topic-postpone-grade (or days 7)))
              ("dismiss" (org-sm--dismiss))
              (_ (user-error "Unknown topic action: %s" action))))
    (_ (user-error "Not an SRS card"))))

(org-sm-web--json-servlet api/review/:id (rating action days)
  (org-sm-web--with-card id
    (org-sm-web--apply-review rating action (and days (string-to-number days)))
    (when (buffer-modified-p)
      (let ((save-silently t)) (save-buffer)))
    (list :ok t)))

;;;###autoload
(defun org-sm-web-start ()
  "Start the local org-sm review server."
  (interactive)
  (setq httpd-host org-sm-web-bind
        httpd-port org-sm-web-port
        httpd-root org-sm-web-root)
  (org-sm--ensure-scheduler)
  (httpd-start)
  (message "org-sm-web: http://%s:%d/" org-sm-web-bind org-sm-web-port))

;;;###autoload
(defun org-sm-web-stop ()
  "Stop the org-sm review server."
  (interactive)
  (httpd-stop)
  (message "org-sm-web: stopped"))

(provide 'org-sm-web)
;;; org-sm-web.el ends here
