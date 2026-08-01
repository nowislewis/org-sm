;;; org-sm-web.el --- Web front-end for org-sm review -*- lexical-binding: t -*-

;; Author: Lewis Liu
;; Package-Requires: ((emacs "29.1") (org-sm "0.1") (simple-httpd "1.6"))
;; Keywords: org, spaced-repetition, web

;;; Commentary:
;;
;; A thin HTTP/JSON front-end over org-sm, so cards can be reviewed from a
;; phone browser.  All scheduling computation is delegated to org-sm's pure
;; grading functions -- this file adds no SRS logic of its own.  It only:
;;
;;   1. locates cards by org-id  (org-with-point-at + org-id-find)
;;   2. serializes card state to JSON
;;   3. maps HTTP requests onto org-sm--{topic,cloze}-grade / org-sm--dismiss
;;
;; org-sm.el is never modified by this file.
;;
;; Endpoints:
;;   GET  /api/queue          -> [{id,type,priority,title}, ...]  (due cards)
;;   GET  /api/card/:id       -> {id,type,title,body,clozes}      (one card)
;;   POST /api/body/:id       -> {ok}     (body=<new body text>)
;;   POST /api/capture        -> {ok,id}   (type,body[,title])   (new card)
;;   POST /api/split          -> {ok,cards} (text[,extra])   (AI split, no write)
;;   POST /api/refine/:id     -> {ok,body}   (AI-rewrite the card body in place)
;;   POST /api/extract-split/:id -> {ok,children} (AI-split card into children)
;;   POST /api/extract/:id    -> {ok,child} (type,selected,start,end)
;;   POST /api/review/:id      -> {ok,...}   (rating=<again|hard|good|easy>
;;                                            or action=<reschedule|postpone|dismiss>)
;;
;; Usage:
;;   (require 'org-sm-web)
;;   (org-sm-web-start)        ; serves API + static UI on `org-sm-web-port'
;;   ;; then open http://<host>:8842/ on your phone
;;
;;; Code:

(require 'org)
(require 'org-id)
(require 'json)
(require 'simple-httpd)
(require 'org-sm)

(declare-function org-sm-gptel-split-text "org-sm-gptel")
(declare-function org-sm-gptel-rewrite-text "org-sm-gptel")

(defgroup org-sm-web nil
  "Web front-end for org-sm."
  :group 'org-sm
  :prefix "org-sm-web-")

(defcustom org-sm-web-port 8842
  "Port for the org-sm web server.
Must be >= 1024 unless Emacs runs as root; ports below 1024 are
privileged and `httpd-start' will fail with \"Permission denied\"."
  :type 'integer)

(defcustom org-sm-web-root
  (expand-file-name "web/" (file-name-directory (or load-file-name buffer-file-name "")))
  "Directory holding the static web UI (index.html etc.)."
  :type 'directory)

(defcustom org-sm-web-bind "0.0.0.0"
  "Address the server binds to (passed to `make-network-process' :host).
\"0.0.0.0\" -- all interfaces; reachable from other LAN devices.
\"127.0.0.1\" -- localhost only; reachable from this machine only.
Any other IP binds that specific interface.
Binding all interfaces exposes the server (which has no authentication)
to your whole LAN; keep it to localhost on untrusted networks."
  :type '(choice (const :tag "All interfaces (LAN)" "0.0.0.0")
                 (const :tag "Localhost only" "127.0.0.1")
                 (string :tag "Specific IP")))

;;;; ---- JSON helpers --------------------------------------------------------

(defun org-sm-web--json (object)
  "Encode OBJECT to a compact JSON string."
  (let ((json-encoding-pretty-print nil))
    (json-encode object)))

(defmacro org-sm-web--with-card (id &rest body)
  "Locate card ID by org-id, run BODY with point on its heading.
Signal a 404-style error via `user-error' if not found.  BODY runs
inside `org-with-point-at' so no buffer/narrowing state leaks."
  (declare (indent 1) (debug (form body)))
  (cl-with-gensyms (marker)
    `(let ((,marker (org-id-find ,id 'marker)))
       (unless ,marker (user-error "No card with id %s" ,id))
       (org-with-point-at ,marker
         (prog1 (progn ,@body)
           (set-marker ,marker nil))))))

;;;; ---- Card serialization --------------------------------------------------

(defun org-sm-web--parse-cloze (body)
  "Split BODY into a vector of tokens for the front-end.
Each token is a plist (:text STR) for literal text or
(:cloze STR) for a {{...}} answer.  Uses `org-sm--cloze-regexp'."
  (let ((tokens nil) (pos 0))
    (while (string-match org-sm--cloze-regexp body pos)
      (let ((mb (match-beginning 0)) (me (match-end 0)))
        (when (> mb pos)
          (push (list :text (substring body pos mb)) tokens))
        (push (list :cloze (match-string 1 body)) tokens)
        (setq pos me)))
    (when (< pos (length body))
      (push (list :text (substring body pos)) tokens))
    (apply #'vector (nreverse tokens))))

(defun org-sm-web--intervals ()
  "Return preview intervals for the card at point, for the UI to show.
For cloze: a plist mapping each rating to its short interval label, e.g.
\(:again \"1m\" :hard \"6m\" :good \"10m\" :easy \"4d\").
For topic: (:reschedule \"8d\").  Reads only; writes nothing."
  (pcase (org-sm-type)
    ('cloze
     (let (out)
       (dolist (entry (org-sm--cloze-preview) (nreverse out))
         ;; keyword key (:good) is required for json-encode to emit an object
         (push (car entry) out)
         (push (org-sm--secs-label (plist-get (cdr entry) :interval-secs)) out))))
    ('topic
     (list :reschedule
           (format "%dd" (org-sm--topic-read (org-sm--topic-afactor)))))
    (_ nil)))

(defun org-sm-web--card-at-point ()
  "Return the current heading serialized as a plist for JSON.
Assumes point is on an org-sm heading."
  (let* ((type   (org-sm-type))
         (bounds (org-sm--body-bounds))
         (body   (org-sm--body-clean
                  (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    (list :id       (org-id-get-create)
          :type     (symbol-name type)
          :title    (org-get-heading t t t t)
          :priority (or (org-entry-get nil "PRIORITY") "C")
          :body     body
          :clozes   (if (eq type 'cloze)
                        (org-sm-web--parse-cloze body)
                      (vector))
          :intervals (org-sm-web--intervals))))

(defun org-sm-web--due-list ()
  "Return a vector of due cards (summary plists) for the queue endpoint."
  (apply #'vector
         (org-sm--map-items
          (lambda ()
            (list :id       (org-id-get-create)
                  :type     (symbol-name (org-sm-type))
                  :priority (or (org-entry-get nil "PRIORITY") "C")
                  :title    (org-get-heading t t t t)))
          #'org-sm--due-p)))

;;;; ---- Servlets ------------------------------------------------------------

(defun org-sm-web--respond-error (message)
  "Insert a JSON error object for MESSAGE."
  (insert (org-sm-web--json (list :error message))))

(defmacro org-sm-web--json-servlet (endpoint args &rest body)
  "Define a JSON servlet at ENDPOINT with servlet ARGS.
BODY should return an object; it is JSON-encoded and sent to the client.
Any error is caught and returned as {\"error\": MESSAGE}.  This removes the
`condition-case' / encode boilerplate repeated by every endpoint."
  (declare (indent 2) (debug (form sexp body)))
  `(httpd-servlet* ,endpoint application/json ,args
     (condition-case err
         (insert (org-sm-web--json (progn ,@body)))
       (error (org-sm-web--respond-error (error-message-string err))))))

(org-sm-web--json-servlet api/queue ()
  (org-sm-web--due-list))

(org-sm-web--json-servlet api/card/:id ()
  (org-sm-web--with-card id
    (org-sm-web--card-at-point)))

;;;; ---- Review (write) ------------------------------------------------------

;; Save unconditionally; no concurrent-edit protection (errors propagate).
(defun org-sm-web--save ()
  "Save the current card's buffer to disk."
  (when (buffer-modified-p)
    (let ((save-silently t))
      (save-buffer))))

;; The only place bridging web strings ("good") and org-sm keywords (:good).
(defun org-sm-web--rating->string (rating)
  "Convert a rating keyword (e.g. :good) to its bare string (\"good\")."
  (substring (symbol-name rating) 1))

(defun org-sm-web--rating<-string (rating)
  "Convert RATING string (e.g. \"good\") to a validated keyword (e.g. :good).
Signal an error if it is not one of `org-sm-cloze-ratings'."
  (let ((sym (intern (concat ":" (or rating "")))))
    (if (memq sym org-sm-cloze-ratings)
        sym
      (user-error "Unknown cloze rating: %s" rating))))

(defun org-sm-web--apply-review (type rating action days)
  "Apply a review decision to the card at point and return a result plist.
TYPE is the card type symbol.  A \"dismiss\" ACTION works for any type;
otherwise cloze cards use RATING and topic cards use ACTION (with DAYS for
postpone).  All scheduling is delegated to the pure org-sm grading
functions; this dispatcher performs no computation of its own."
  (cond
   ;; dismiss is type-agnostic, so handle it before the type branches
   ((equal action "dismiss")
    (org-sm--dismiss)
    (list :ok t :action "dismiss"))
   ((eq type 'cloze)
    (let ((r (org-sm--cloze-grade (org-sm-web--rating<-string rating))))
      (list :ok t :rating (org-sm-web--rating->string (plist-get r :rating))
            :due (plist-get r :due))))
   ((eq type 'topic)
    (let ((r (pcase action
               ("reschedule" (org-sm--topic-grade))
               ("postpone"   (org-sm--topic-postpone-grade (or days 7)))
               (_ (user-error "Unknown topic action: %s" action)))))
      (list :ok t :action action :interval-days (plist-get r :interval-days))))
   (t (user-error "Not an SRS card: %s" type))))

;; simple-httpd merges the form-urlencoded body into the query, so POST
;; params arrive as ordinary servlet arguments.
(org-sm-web--json-servlet api/review/:id (rating action days)
  (org-sm-web--with-card id
    (prog1 (org-sm-web--apply-review
            (org-sm-type) rating action
            (and days (string-to-number days)))
      (org-sm-web--save))))

(defun org-sm-web--lan-address ()
  "Best-effort guess of this machine's LAN IPv4 address, or nil."
  (car (seq-filter
        (lambda (ip)
          (and (string-match-p "\\`[0-9.]+\\'" ip)
               (not (string-prefix-p "127." ip))))
        (mapcar (lambda (iface) (format-network-address (cdr iface) t))
                (ignore-errors (network-interface-list))))))

;;;; ---- Editing & extraction (write) ---------------------------------------

(defalias 'org-sm-web--set-body 'org-sm--set-body)

(org-sm-web--json-servlet api/body/:id (body)
  (unless body (user-error "Missing 'body' parameter"))
  (org-sm-web--with-card id
    (org-sm-web--set-body body)
    (org-sm-web--save))
  (list :ok t :id id))

;; Create a brand-new card under `org-sm-capture-file' / `org-sm-capture-olp'.
;; Unlike the other write endpoints this has no id to locate; it delegates to
;; the pure `org-sm--capture', which leaves the target buffer current so we can
;; save it here.  cloze bodies must contain at least one {{answer}} marker.
(org-sm-web--json-servlet api/capture (type body title)
  (unless (member type '("topic" "cloze"))
    (user-error "type must be topic or cloze"))
  (unless (org-string-nw-p body)
    (user-error "Missing 'body' text"))
  (when (and (equal type "cloze") (not (string-match-p org-sm--cloze-regexp body)))
    (user-error "cloze body needs a {{answer}} marker"))
  (save-current-buffer
    (prog1 (list :ok t :id (org-sm--capture (intern type) body nil nil title))
      (org-sm-web--save))))

;; AI split (optional; needs `org-sm-gptel').  Pure transform: takes raw text,
;; returns the AI's proposed cards WITHOUT writing anything.  The front-end
;; shows them for review, then creates each via the existing /api/capture, so
;; there is still one write path.  Synchronous via `org-sm-gptel-split-text'.
(org-sm-web--json-servlet api/split (text extra)
  (unless (fboundp 'org-sm-gptel-split-text)
    (user-error "AI split unavailable: load org-sm-gptel"))
  (unless (org-string-nw-p text)
    (user-error "Missing 'text'"))
  (list :ok t
        :cards (apply #'vector
                      (mapcar (lambda (c)
                                (list :title (or (car c) "") :body (cdr c)))
                              (org-sm-gptel-split-text text extra)))))

;; start/end are character offsets of the selection within the card body.
(org-sm-web--json-servlet api/extract/:id (type selected start end)
  (unless (member type '("topic" "cloze"))
    (user-error "type must be topic or cloze"))
  (unless (and selected (> (length selected) 0))
    (user-error "Missing 'selected' text"))
  (org-sm-web--with-card id
    (prog1 (list :ok t :child
                 (org-sm--extract (intern type) selected
                                  (string-to-number (or start "0"))
                                  (string-to-number (or end (number-to-string (length selected))))))
      (org-sm-web--save))))

;; AI refine (optional; needs `org-sm-gptel').  Pure transform, like
;; /api/split: returns the rewritten body WITHOUT writing anything, so the
;; single write path stays the existing Edit textarea + Save
;; (/api/body/:id).  Mirrors the Emacs `org-sm-gptel-refine', which also
;; only edits the buffer and does not save until the buffer itself is saved.
(org-sm-web--json-servlet api/refine/:id ()
  (unless (fboundp 'org-sm-gptel-rewrite-text)
    (user-error "AI refine unavailable: load org-sm-gptel"))
  (let* ((info (org-sm-web--with-card id
                 (let ((bounds (org-sm--body-bounds)))
                   (cons (org-sm-type)
                         (string-trim (buffer-substring-no-properties
                                      (car bounds) (cdr bounds)))))))
         (type (car info)) (body (cdr info)))
    (list :ok t :body (org-sm-gptel-rewrite-text body type))))

;; AI split (optional; needs `org-sm-gptel').  Split the card at :id into
;; child topic cards via `org-sm-gptel-split-text' + `org-sm--extract-cards',
;; appending back-references to this card; the card itself is kept.
(org-sm-web--json-servlet api/extract-split/:id ()
  (unless (fboundp 'org-sm-gptel-split-text)
    (user-error "AI split unavailable: load org-sm-gptel"))
  (org-sm-web--with-card id
    (let* ((bounds (org-sm--body-bounds))
           (body   (string-trim (buffer-substring-no-properties
                                 (car bounds) (cdr bounds))))
           (cards  (org-sm-gptel-split-text body)))
      (org-back-to-heading t)
      (prog1 (list :ok t :children (length (org-sm--extract-cards cards)))
        (org-sm-web--save)))))

;;;###autoload
(defun org-sm-web-start ()
  "Start the org-sm web server on `org-sm-web-port'.
Binds according to `org-sm-web-bind' (default \"0.0.0.0\", i.e. all
interfaces) so other devices on the same LAN can reach it.  Prints both
the localhost and the LAN URL."
  (interactive)
  ;; `httpd-host' is passed to `make-network-process' as :host.
  (setq httpd-host org-sm-web-bind
        httpd-port org-sm-web-port
        httpd-root org-sm-web-root)
  (org-sm--ensure-scheduler)
  (httpd-start)
  (let ((lan (org-sm-web--lan-address)))
    (message "org-sm-web: http://localhost:%d/%s"
             org-sm-web-port
             (if lan (format "  |  LAN: http://%s:%d/" lan org-sm-web-port) ""))))

;;;###autoload
(defun org-sm-web-stop ()
  "Stop the org-sm web server."
  (interactive)
  (httpd-stop)
  (message "org-sm-web: stopped"))

(provide 'org-sm-web)
;;; org-sm-web.el ends here
