;;; org-sm-web.el --- Web review front-end for org-sm -*- lexical-binding: t -*-

;; Author: Lewis Liu
;; Package-Requires: ((emacs "29.1") (org-sm "0.1") (simple-httpd "1.6"))
;; Keywords: org, spaced-repetition, web

;;; Commentary:
;;
;; Minimal HTTP/JSON front-end for reviewing due org-sm cards from a browser.
;; Scheduling stays in org-sm; phone review and editing live in this module.
;;
;; The browser gets ONE snapshot request (`GET /api/session') carrying every
;; due card plus its precomputed rating intervals.  Measured on a 381-card
;; vault: the whole snapshot serialises in ~350ms and is ~90KB, while a
;; per-card `GET /api/card' costs ~50ms of `org-id-find' each round trip.  One
;; request is both faster and what makes tap-to-grade feel instant; the card
;; queue then lives client-side.
;;
;; Usage:
;;   (require 'org-sm-web)
;;   (org-sm-web-start)               ; prints an IP:port for a trusted LAN
;;   ;; Set org-sm-web-bind to "127.0.0.1" for SSH-tunnel-only access.

;;; Code:

(require 'seq)
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

(defcustom org-sm-web-bind "0.0.0.0"
  "Address the server binds to.
The default serves a trusted LAN so phones can open the displayed IP:port.
Set to \"127.0.0.1\" for SSH-tunnel-only access.  Do not expose this
HTTP server directly to the Internet."
  :type '(choice (const :tag "All interfaces (LAN)" "0.0.0.0")
                 (const :tag "Localhost only" "127.0.0.1")
                 (string :tag "Specific IP")))

(defcustom org-sm-web-token nil
  "Access token required on API requests, or nil to disable auth.
When nil (the default), no token is required; keep the server on localhost
or a trusted LAN.  A token in an HTTP URL does not replace TLS."
  :type '(choice (const :tag "No token (clean IP:PORT)" nil)
                 (string :tag "Fixed token")))

(define-error 'org-sm-web-denied "unauthorized")
(define-error 'org-sm-web-conflict "card changed since it was loaded")

(defvar org-sm-web--token nil
  "Token actually enforced by the running server, or nil.")

(defun org-sm-web--param (name)
  "Return query/form parameter NAME for the running request, or nil."
  (cadr (assoc name httpd-query)))

(defun org-sm-web--authorize! ()
  "Reject untrusted browser origins and mismatched access tokens.
Host checking on localhost also blocks a page using DNS rebinding to reach
this unauthenticated server under an attacker-controlled hostname."
  (let* ((host (cadr (assoc "Host" httpd-request)))
         (origin (cadr (assoc "Origin" httpd-request))))
    (when (and (equal org-sm-web-bind "127.0.0.1")
               (not (and host
                         (string-match-p
                          "\\`\\(localhost\\|127\\.0\\.0\\.1\\):[0-9]+\\'" host))))
      (signal 'org-sm-web-denied nil))
    (when (and origin (not (equal origin (concat "http://" host))))
      (signal 'org-sm-web-denied nil)))
  (when (and org-sm-web--token
             (not (equal (org-sm-web--param "t") org-sm-web--token)))
    (signal 'org-sm-web-denied nil)))

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
         (unwind-protect (progn ,@body)
           (set-marker ,marker nil))))))

(defun org-sm-web--intervals ()
  "Return current card's rating labels without changing its schedule."
  (pcase (org-sm-type)
    ('cloze
     (let (out)
       (dolist (entry (org-sm-preview-cloze) (nreverse out))
         (push (car entry) out)
         (push (org-sm-interval-label (plist-get (cdr entry) :interval-secs)) out))))
    ('topic (list :reschedule
                  (format "%dd" (org-sm-topic-interval (org-sm-topic-afactor)))))))

(defun org-sm-web--book-label ()
  "Return a short shelf label for the current card's file.
`20221201T203741--minto-2019-金字塔原理…__bib_srs.org' becomes
`minto-2019-金字塔原理…', which is what a phone screen can actually show as a
group heading.  Returns nil for the first file's worth of cards until the
basename is known."
  (when-let* ((name (file-name-nondirectory (or (buffer-file-name) ""))))
    (replace-regexp-in-string
     "\\`[0-9T:=-]+" ""
     (file-name-sans-extension
      (replace-regexp-in-string "__.*" "" name)))))

(defun org-sm-web--card-at-point ()
  "Serialize the current org-sm card for the browser, or nil if it has no ID.
Read-only: uses `org-id-get' rather than `org-id-get-create' because an
overdue card that has never been opened in Emacs must not gain a property
drawer just because a phone asked for the queue.  A card with no ID cannot be
addressed back by the review endpoint, so it is skipped instead."
  (when-let* ((id (org-id-get)))
    (let* ((type (org-sm-type))
           (bounds (org-sm-body-bounds))
           (body (org-sm-body-clean
                  (buffer-substring-no-properties (car bounds) (cdr bounds)))))
      (list :id id
            :type (symbol-name type)
            :title (org-get-heading t t t t)
            :body body
            :book (org-sm-web--book-label)
            ;; `org-sm-sched-day-delta' is negative for overdue cards, so
            ;; negate it: days late is a positive number here.
            :overdue (max 0 (- (or (org-sm-sched-day-delta) 0)))
            :intervals (org-sm-web--intervals)
            :revision (org-sm-web--revision)))))

(defun org-sm-web--revision ()
  "Fingerprint this card's text and scheduling state to reject stale writes."
  (let ((bounds (org-sm-body-bounds)))
    (secure-hash
     'sha256
     (prin1-to-string
      (list (org-get-heading t t t t)
            (buffer-substring-no-properties (car bounds) (cdr bounds))
            (org-get-scheduled-time nil)
            (org-get-tags nil t)
            (mapcar (lambda (key) (org-entry-get nil key))
                    '("SRS_TYPE" "SRS_LAST" "SRS_STATE" "SRS_STABILITY"
                      "SRS_DIFFICULTY" "SRS_STEP")))))))

(defun org-sm-web--check-revision (revision)
  "Reject a missing or stale REVISION before changing the current card."
  (unless (and revision (string= revision (org-sm-web--revision)))
    (signal 'org-sm-web-conflict nil)))

(defun org-sm-web--session ()
  "Return the whole due queue as one JSON-ready vector, stalest first.
Ships body text and precomputed intervals for every due card in one reply,
so switching cards needs no GET; each write still awaits its POST response."
  (apply #'vector
         (seq-sort-by (lambda (c) (plist-get c :overdue)) #'>
                      (delq nil (org-sm-map-items
                                 #'org-sm-web--card-at-point
                                 #'org-sm-due-p)))))

(defmacro org-sm-web--json-servlet (endpoint args &rest body)
  "Define token-gated JSON ENDPOINT whose BODY returns an encodable object.
ARGS bind from the query string and, for POSTs, the urlencoded body.

Replies are 401 for a missing/stale token and 400 for a bad request.  Both the
HTTP status and a safe message matter: `httpd-with-buffer' only auto-sends 200,
so the header is sent by hand here -- doing so sets `httpd--header-sent' and
suppresses that auto-send.  Raw `error-message-string' is never echoed, because
on a LAN-facing server it leaks absolute filesystem paths."
  (declare (indent 2) (debug (form sexp body)))
  `(httpd-servlet* ,endpoint application/json ,args
     (condition-case err
         (progn
           (org-sm-web--authorize!)
           (unless (equal (caar httpd-request)
                          ,(if (memq endpoint '(api/session api/git/diff)) "GET" "POST"))
             (user-error "Wrong HTTP method"))
           (insert (org-sm-web--json (progn ,@body))))
       (org-sm-web-denied
        (insert (org-sm-web--json (list :error "unauthorized")))
        (httpd-send-header t 'application/json 401))
       (org-sm-web-conflict
        ;; The one error the browser must tell apart: it has to keep the user's
        ;; text and show it against the newer server copy instead of retrying.
        (insert (org-sm-web--json (list :error "conflict" :retry nil)))
        (httpd-send-header t 'application/json 409))
       (error
        (httpd-log `(error ,(error-message-string err) ,(car err)))
        (insert (org-sm-web--json (list :error "bad request")))
        (httpd-send-header t 'application/json 400)))))

(org-sm-web--json-servlet api/session ()
  (org-sm-web--session))

(defun org-sm-web--rating (rating)
  "Validate web RATING and return its keyword form."
  (let ((value (intern (concat ":" (or rating "")))))
    (unless (memq value org-sm-cloze-ratings)
      (user-error "Unknown cloze rating: %s" rating))
    value))

(defun org-sm-web--positive-days (days)
  "Parse DAYS from a form body, rejecting non-positive or excessive values."
  (unless (and (stringp days) (string-match-p "\\`[0-9]+\\'" days)
               (<= (length days) 4))
    (user-error "Invalid postpone days"))
  (let ((value (string-to-number days)))
    (unless (<= 1 value 3650) (user-error "Invalid postpone days"))
    value))

(defun org-sm-web--apply-review (rating action days)
  "Apply the browser's review decision to the card at point."
  (pcase (org-sm-type)
    ('cloze (org-sm-grade-cloze (org-sm-web--rating rating)))
    ('topic (pcase action
              ("reschedule" (org-sm-grade-topic))
              ("postpone" (org-sm-grade-postpone (org-sm-web--positive-days days)))
              ("dismiss" (org-sm-dismiss))
              (_ (user-error "Unknown topic action: %s" action))))
    (_ (user-error "Not an SRS card"))))

(org-sm-web--json-servlet api/review/:id (rating action days revision)
  (org-sm-web--with-card id
    (org-sm-web--check-revision revision)
    (prog1 (append (list :ok t) (org-sm-web--apply-review rating action days))
      (org-sm-web--save!))))

;;;; ---- Writing from the phone ---------------------------------------------
;; Editing is what the browser is actually for: iOS gives real selection
;; handles, a real Chinese IME and the system edit menu, none of which survive
;; a terminal.  The scheduling semantics stay in `org-sm'; these endpoints only
;; forward text to the same primitives `org-sm-card-workbench' uses.

(defun org-sm-web--leading-delta ()
  "Return how many chars `org-sm-body-clean' strips from the START of the body.
Every body in a real vault carries trailing whitespace, and `org-sm-extract'
interprets its offsets against the RAW region while the snapshot ships the
CLEANED text.  Trailing whitespace cannot shift a start offset, but leading can
-- usually by one newline -- so callers must translate.  Without this, an
extract on such a card would silently mark the wrong span."
  (let* ((bd (org-sm-body-bounds))
         (raw (buffer-substring-no-properties (car bd) (cdr bd))))
    (- (length raw) (length (string-trim-left raw)))))

(defun org-sm-web--save! ()
  "Write the current buffer to disk if it changed, quietly."
  (when (buffer-modified-p)
    (let ((save-silently t)) (save-buffer))))

(defun org-sm-web--set-title (title)
  "Replace current heading's title with TITLE, preserving TODO/priority/tags."
  (unless (org-at-heading-p) (user-error "Not on a heading"))
  (org-sm-set-title title))

(org-sm-web--json-servlet api/patch/:id (body title revision)
  ;; Reject changes to either the title, body or schedule since the snapshot.
  (org-sm-web--with-card id
    (org-sm-web--check-revision revision)
    (cond
     ((and body title)
      ;; `org-edit-headline' may move point: set it before the body write.
      (org-sm-web--set-title title)
      (org-sm-set-body body))
     (body (org-sm-set-body body))
     (title (org-sm-web--set-title title)))
    (org-sm-web--save!)
    (let ((bd (org-sm-body-bounds)))
      (list :ok t
            :body (org-sm-body-clean
                   (buffer-substring-no-properties (car bd) (cdr bd)))
            :title (org-get-heading t t t t)
            :revision (org-sm-web--revision)))))

(org-sm-web--json-servlet api/extract/:id (start end type revision)
  ;; Extract the browser's selection from card :id into a child card.
  ;; START/END are offsets into the CLEANED body the browser was given; TYPE is
  ;; "topic" or "cloze".  Same operation as `org-sm-item-extract' in Emacs --
  ;; the phone supplies the region by number, which iOS selection gives us for
  ;; free.
  (unless (member type '("topic" "cloze"))
    (user-error "Unknown card type: %s" type))
  (org-sm-web--with-card id
    (org-sm-web--check-revision revision)
    (unless (and start end
                 (string-match-p "\\`[0-9]+\\'" start)
                 (string-match-p "\\`[0-9]+\\'" end))
      (user-error "Invalid selection"))
    (let* ((delta (org-sm-web--leading-delta))
           (bd (org-sm-body-bounds))
           (raw-len (- (cdr bd) (car bd)))
           (s (+ delta (string-to-number start)))
           (e (+ delta (string-to-number end))))
      (unless (<= delta s e raw-len)
        (user-error "selection out of range"))
      (let ((selected (buffer-substring-no-properties (+ (car bd) s) (+ (car bd) e))))
        (when (string-empty-p (string-trim selected))
        (user-error "empty selection"))
      (let ((child (org-sm-extract (intern type) selected s e)))
        ;; `org-sm-extract' appends a [[id:CHILD][<T>]] back-reference to THIS
        ;; card, so the parent's body just changed on disk.  Echo it back, or
        ;; the phone's next patch would report a phantom conflict against text
        ;; it never saw.
        (let ((parent (org-sm-web--card-at-point)))
          (org-sm-web--save!)
          (list :ok t :id child :parent parent)))))))

;;;; ---- Git synchronization ------------------------------------------------

(defun org-sm-web--git-run (root &rest args)
  "Run Git ARGS in ROOT; return output only when the command succeeds.
Avoid shell interpolation and never report failed pull/commit/push as success."
  (let ((default-directory root))
    (with-temp-buffer
      (let ((status (apply #'process-file "git" nil (list t t) nil args)))
        (unless (eq status 0)
          (error "Git %s failed: %s" (car args) (string-trim (buffer-string))))
        (string-trim (buffer-string))))))

(defun org-sm-web--git-info ()
  "Return (ROOT . PATHSPEC) for `org-sm-directory', or nil if unset."
  (when (and org-sm-directory (file-directory-p org-sm-directory))
    (let* ((dir (file-name-as-directory (expand-file-name org-sm-directory)))
           (root (ignore-errors (org-sm-web--git-run dir "rev-parse" "--show-toplevel"))))
      (when root
        (cons (file-name-as-directory root)
              (let ((rel (org-sm-web--git-run dir "rev-parse" "--show-prefix")))
                (if (string-empty-p rel) "." rel)))))))

(defun org-sm-web--git-diff ()
  "Return notes status, tracked changes, and branch outgoing count.
Untracked files are listed in status but cannot be previewed by git diff."
  (if-let* ((info (org-sm-web--git-info)))
      (let* ((root (car info)) (rel (cdr info))
             (status (org-sm-web--git-run root "status" "--short" "--" rel))
             (head (ignore-errors (org-sm-web--git-run root "rev-parse" "--verify" "HEAD")))
             (stat (if head (org-sm-web--git-run root "diff" "HEAD" "--stat" "--" rel)
                     (org-sm-web--git-run root "diff" "--cached" "--stat" "--" rel)))
             (diff (if head (org-sm-web--git-run root "diff" "HEAD" "--" rel)
                     (string-join (list (org-sm-web--git-run root "diff" "--cached" "--" rel)
                                        (org-sm-web--git-run root "diff" "--" rel)) "\n")))
             (outgoing (or (ignore-errors
                             (string-to-number (org-sm-web--git-run
                                                root "rev-list" "--count" "@{upstream}..HEAD"))) 0)))
        (list :ok t :hasChanges (not (string-empty-p status))
              :outgoing outgoing :status status :stat stat
              :diff (if (> (length diff) 30000)
                        (concat (substring diff 0 30000) "\n\n... (diff truncated) ...")
                      diff)))
    (list :ok nil :error "Set org-sm-directory to a Git repository")))

(defun org-sm-web--unsaved-files-p (root)
  "Return non-nil if an open buffer under ROOT is modified."
  (let ((dir (file-name-as-directory (file-truename root))))
    (seq-some (lambda (buf)
                (with-current-buffer buf
                  (and (buffer-file-name) (buffer-modified-p)
                       (file-in-directory-p (buffer-file-name) dir))))
              (buffer-list))))

(defun org-sm-web--git-pull ()
  "Pull the repository, then reload clean file buffers changed on disk."
  (if-let* ((info (org-sm-web--git-info)))
      (let ((root (car info)))
        (when (org-sm-web--unsaved-files-p root)
          (user-error "Save modified repository buffers before pulling"))
        (let ((output (org-sm-web--git-run root "pull" "--rebase" "--autostash")))
          (dolist (buf (buffer-list))
            (with-current-buffer buf
              (when (and (buffer-file-name)
                         (file-in-directory-p (buffer-file-name) root)
                         (not (buffer-modified-p))
                         (file-exists-p (buffer-file-name))
                         (not (verify-visited-file-modtime buf)))
                (revert-buffer t t))))
          (list :ok t :output output)))
    (list :ok nil :error "Set org-sm-directory to a Git repository")))

(defun org-sm-web--git-push (&optional custom-msg)
  "Commit only files under `org-sm-directory', then push."
  (if-let* ((info (org-sm-web--git-info)))
      (let* ((root (car info)) (rel (cdr info))
             (status (org-sm-web--git-run root "status" "--short" "--" rel)))
        (when (org-sm-web--unsaved-files-p root)
          (user-error "Save modified repository buffers before pushing"))
        (if (string-empty-p status)
            ;; A previous commit may have succeeded while its push failed.
            (list :ok t :output (org-sm-web--git-run root "push") :hasChanges nil)
          (let* ((count (length (split-string status "\n" t)))
                 (msg (or (and (org-string-nw-p custom-msg) custom-msg)
                          (format "sync(notes): %d file(s) updated [%s]"
                                  count (format-time-string "%Y-%m-%d %H:%M")))))
            (org-sm-web--git-run root "add" "-A" "--" rel)
            ;; --only ignores any unrelated paths already staged in the index.
            (let ((committed (org-sm-web--git-run root "commit" "--only" "-m" msg "--" rel)))
              (list :ok t :commit msg
                    :output (concat committed "\n" (org-sm-web--git-run root "push")))))))
    (list :ok nil :error "Set org-sm-directory to a Git repository")))

(org-sm-web--json-servlet api/git/diff ()
  (org-sm-web--git-diff))

(org-sm-web--json-servlet api/git/sync (action msg)
  (pcase action
    ("pull" (org-sm-web--git-pull))
    ("push" (org-sm-web--git-push msg))
    (_ (user-error "Unknown git sync action: %s" action))))

(defun org-sm-web--local-ip ()
  "Return the most likely local LAN IPv4 address as a string.
Queries active network interfaces in pure Elisp, preferring private LAN
ranges (192.168.x.x, 10.x.x.x, 172.16-31.x.x), falling back to `hostname -I'
or localhost."
  (let ((ips nil))
    (dolist (item (ignore-errors (network-interface-list)))
      (let ((addr (cdr item)))
        (when (and (vectorp addr) (= (length addr) 5) (/= (aref addr 0) 127) (> (aref addr 0) 0))
          (let ((ip (format "%d.%d.%d.%d" (aref addr 0) (aref addr 1) (aref addr 2) (aref addr 3))))
            (unless (member ip ips)
              (push ip ips))))))
    (or (seq-find (lambda (ip) (string-prefix-p "192.168." ip)) ips)
        (seq-find (lambda (ip) (string-prefix-p "10." ip)) ips)
        (seq-find (lambda (ip) (string-prefix-p "172." ip)) ips)
        (car ips)
        (let ((raw (car (ignore-errors (process-lines "hostname" "-I")))))
          (and raw (car (split-string raw "[ \t\n\r]+" t))))
        "127.0.0.1")))

(defun org-sm-web--display-host ()
  "Return the IP or hostname to display in the startup message."
  (if (member org-sm-web-bind '("0.0.0.0" "::" nil))
      (org-sm-web--local-ip)
    org-sm-web-bind))

;;;###autoload
(defun org-sm-web-start ()
  "Start the local org-sm review server.
If the server is already running, it restarts cleanly."
  (interactive)
  (when (httpd-running-p)
    (httpd-stop))
  (setq org-sm-web--token org-sm-web-token
        httpd-host org-sm-web-bind
        httpd-port org-sm-web-port
        httpd-root org-sm-web-root)
  (httpd-start)
  (message "org-sm-web: phone URL http://%s:%d/%s%s"
           (org-sm-web--display-host)
           org-sm-web-port
           (if org-sm-web--token (format "?t=%s" org-sm-web--token) "")
           (if (equal org-sm-web-bind "127.0.0.1")
               " (requires SSH port forwarding)"
             " (trusted LAN only)")))

;;;###autoload
(defun org-sm-web-stop ()
  "Stop the org-sm review server."
  (interactive)
  (httpd-stop)
  (message "org-sm-web: stopped"))

(provide 'org-sm-web)
;;; org-sm-web.el ends here
