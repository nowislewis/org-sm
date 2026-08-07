;;; org-sm-buttons.el --- Mouse-driven bottom bar for org-sm -*- lexical-binding: t -*-

;; Author: Lewis Liu
;; Package-Requires: ((emacs "29.1") (org-sm "0.1"))
;; Keywords: org, spaced-repetition, convenience

;;; Commentary:
;;
;; `org-sm-buttons-mode' adds a small persistent side window at the bottom of
;; the frame.  Its buttons drive an org-sm review without requiring the
;; minibuffer's `read-multiple-choice' prompts:
;;
;;   inactive      Start review
;;   topic         Reschedule, Postpone, Skip, Dismiss, Abort
;;   cloze hidden  Reveal, Skip, Dismiss, Abort
;;   cloze shown   Again, Hard, Good, Easy, Skip, Dismiss, Abort
;;
;; The extension deliberately owns presentation only.  Scheduling and writes
;; remain in org-sm's existing grading functions, so the button bar and the
;; keyboard / web interfaces cannot diverge in their SRS behaviour.
;;
;; Enable it once in the user's configuration:
;;
;;   (require 'org-sm-buttons)
;;   (org-sm-buttons-mode 1)
;;
;; Buttons respond to mouse-1, mouse-2, RET, and TAB navigation.  The bar is
;; persistent while the mode is enabled, so its Start button is also a
;; mouse-only entry point to a review session.

;;; Code:

(require 'button)
(require 'org-sm)

(defgroup org-sm-buttons nil
  "Mouse-driven review controls for org-sm."
  :group 'org-sm
  :prefix "org-sm-buttons-")

(defcustom org-sm-buttons-postpone-days 7
  "Number of days the topic Postpone button moves a card forward."
  :type 'integer)

(defcustom org-sm-buttons-window-height 3
  "Initial height, in lines, of the org-sm button side window.
After rendering, `org-sm-buttons-refresh' fits the window to its wrapped
content, up to `org-sm-buttons-max-window-height'."
  :type 'integer)

(defcustom org-sm-buttons-max-window-height 8
  "Maximum height, in lines, of the fitted org-sm button side window."
  :type 'integer)

(defcustom org-sm-buttons-button-padding 2
  "Blank character cells on each side of every button label."
  :type 'integer)

(defcustom org-sm-buttons-button-gap 4
  "Blank character cells separating adjacent buttons.
The gap is deliberately not part of either button, reducing touch mis-taps."
  :type 'integer)

(defcustom org-sm-buttons-min-button-width 14
  "Minimum character-cell width of every button in all display types."
  :type 'integer)

(defface org-sm-buttons-primary-face
  '((t :inherit success :weight bold :box t))
  "Face for primary org-sm button-bar actions."
  :group 'org-sm-buttons)

(defface org-sm-buttons-secondary-face
  '((t :inherit font-lock-keyword-face :weight bold :box t))
  "Face for secondary org-sm button-bar actions."
  :group 'org-sm-buttons)

(defface org-sm-buttons-danger-face
  '((t :inherit error :weight bold :box t))
  "Face for destructive org-sm button-bar actions."
  :group 'org-sm-buttons)

(defconst org-sm-buttons--buffer-name " *org-sm review controls*"
  "Name of the button-bar buffer.")

;; Declared before helper functions so byte compilation knows it is global;
;; `define-minor-mode' below supplies the interactive setter and docstring.
(defvar org-sm-buttons-mode nil)

(defvar org-sm-buttons--dismiss-marker nil
  "Marker for the card awaiting a second dismiss confirmation click.")

(defvar org-sm-buttons--button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    ;; `button-map' intentionally uses mouse-2, matching links.  These are
    ;; controls rather than links, so mouse-1 should activate them as well.
    (define-key map [mouse-1] #'push-button)
    map)
  "Keymap installed on each button in the org-sm button bar.")

(defun org-sm-buttons--review-active-p ()
  "Return non-nil when org-sm currently displays a review card."
  (buffer-live-p org-sm--current-buffer))

(defun org-sm-buttons--current-type ()
  "Return the type of the current review card, or nil.
This is evaluated in `org-sm--current-buffer', never in the button buffer."
  (when (org-sm-buttons--review-active-p)
    (with-current-buffer org-sm--current-buffer
      (save-excursion
        (org-sm-type)))))

(defun org-sm-buttons--remaining-count ()
  "Return the number of review cards including the displayed one."
  (if (org-sm-buttons--review-active-p)
      (1+ (length org-sm--queue))
    0))

(defun org-sm-buttons--cloze-state ()
  "Return the current review cloze state, or nil for a non-cloze review.
`org-sm--cloze-state' is buffer-local to the source card.  Rendering happens
in the button-bar buffer, so it must be read explicitly from that source."
  (when (org-sm-buttons--review-active-p)
    (with-current-buffer org-sm--current-buffer
      org-sm--cloze-state)))

(defun org-sm-buttons--cloze-labels ()
  "Return an alist of rating symbols and their current interval labels."
  (when (eq (org-sm-buttons--current-type) 'cloze)
    (with-current-buffer org-sm--current-buffer
      (save-excursion
        (mapcar (lambda (entry)
                  (cons (car entry)
                        (org-sm--secs-label
                         (plist-get (cdr entry) :interval-secs))))
                (org-sm--cloze-preview))))))

(defun org-sm-buttons--visit-card ()
  "Select the current review card and put point on its containing heading.
Button actions begin in the side-window buffer, whereas org-sm's scheduling
functions intentionally operate at point in the source org buffer."
  (unless (org-sm-buttons--review-active-p)
    (user-error "No active org-sm review"))
  (pop-to-buffer org-sm--current-buffer)
  (unless (org-sm--goto-enclosing-card)
    ;; A narrowed review buffer normally makes this branch unreachable, but it
    ;; gives a useful failure rather than scheduling an unrelated heading if a
    ;; user edited the source buffer between two clicks.
    (user-error "The current org-sm review card is no longer at point")))

(defun org-sm-buttons--clear-dismiss-confirmation ()
  "Clear any pending destructive-action confirmation."
  (when (markerp org-sm-buttons--dismiss-marker)
    (set-marker org-sm-buttons--dismiss-marker nil))
  (setq org-sm-buttons--dismiss-marker nil))

(defun org-sm-buttons--dismiss-pending-p ()
  "Return non-nil if the displayed card awaits dismissal confirmation."
  (and (markerp org-sm-buttons--dismiss-marker)
       (marker-buffer org-sm-buttons--dismiss-marker)
       (org-sm-buttons--review-active-p)
       (with-current-buffer org-sm--current-buffer
         (save-excursion
           (goto-char org-sm-buttons--dismiss-marker)
           (and (org-at-heading-p) (org-sm-type))))))

(defun org-sm-buttons--start ()
  "Start an org-sm review from the button bar."
  (interactive)
  (org-sm-buttons--clear-dismiss-confirmation)
  (call-interactively #'org-sm-review-start)
  (org-sm-buttons-refresh))

(defun org-sm-buttons--topic-reschedule ()
  "Grade the displayed topic as read and advance the review queue."
  (interactive)
  (org-sm-buttons--clear-dismiss-confirmation)
  (org-sm-buttons--visit-card)
  (let ((result (org-sm--topic-grade)))
    (org-sm--advance
     (format "topic → %d days" (plist-get result :interval-days))))
  (org-sm-buttons-refresh))

(defun org-sm-buttons--topic-postpone ()
  "Postpone the displayed topic by `org-sm-buttons-postpone-days' days."
  (interactive)
  (org-sm-buttons--clear-dismiss-confirmation)
  (org-sm-buttons--visit-card)
  (let ((result (org-sm--topic-postpone-grade org-sm-buttons-postpone-days)))
    (org-sm--advance
     (format "topic postpone → %d days" (plist-get result :interval-days))))
  (org-sm-buttons-refresh))

(defun org-sm-buttons--cloze-reveal ()
  "Reveal cloze answers on the displayed card."
  (interactive)
  (org-sm-buttons--clear-dismiss-confirmation)
  (org-sm-buttons--visit-card)
  (unless (eq org-sm--cloze-state 'hidden)
    (user-error "The cloze answer is already visible"))
  (org-sm-cloze-reveal-overlays)
  (setq org-sm--cloze-state 'revealed)
  (message "org-sm: cloze revealed")
  (org-sm-buttons-refresh))

(defun org-sm-buttons--cloze-grade (rating)
  "Apply cloze RATING to the displayed card and advance the queue."
  (org-sm-buttons--clear-dismiss-confirmation)
  (org-sm-buttons--visit-card)
  (unless (eq org-sm--cloze-state 'revealed)
    (user-error "Reveal the cloze answer before rating it"))
  (org-sm-cloze-remove-overlays)
  (let* ((result (org-sm--cloze-grade rating))
         (due (format-time-string
               "%F %H:%M"
               (parse-iso8601-time-string (plist-get result :due)))))
    (org-sm--advance (format "cloze %s → %s" rating due)))
  (org-sm-buttons-refresh))

(defun org-sm-buttons--skip ()
  "Leave the current card unchanged and advance the review queue."
  (interactive)
  (org-sm-buttons--clear-dismiss-confirmation)
  (org-sm-buttons--visit-card)
  (org-sm--advance "skipped")
  (org-sm-buttons-refresh))

(defun org-sm-buttons--dismiss ()
  "Ask for one additional click before dismissing the displayed card.
The second click performs the existing dismissal and advances the queue.
This avoids an irreversible touch mis-tap without adding a minibuffer prompt."
  (interactive)
  (org-sm-buttons--visit-card)
  (if (org-sm-buttons--dismiss-pending-p)
      (progn
        (org-sm-buttons--clear-dismiss-confirmation)
        (org-sm--dismiss)
        ;; Calling `org-sm-item-dismiss' here would not advance after a final
        ;; item: at this point the current card has already been popped from
        ;; --queue.
        (org-sm--advance "dismissed"))
    (org-sm-buttons--clear-dismiss-confirmation)
    (setq org-sm-buttons--dismiss-marker (copy-marker (point)))
    (message "org-sm: click Confirm dismiss to remove this card from review"))
  (org-sm-buttons-refresh))

(defun org-sm-buttons--cancel-dismiss ()
  "Cancel a pending dismissal confirmation."
  (interactive)
  (org-sm-buttons--clear-dismiss-confirmation)
  (message "org-sm: dismiss cancelled")
  (org-sm-buttons-refresh))

(defun org-sm-buttons--abort ()
  "Abort the active review from the button bar."
  (interactive)
  (org-sm-buttons--clear-dismiss-confirmation)
  (org-sm-buttons--visit-card)
  (org-sm-review-abort)
  (org-sm-buttons-refresh))

(defun org-sm-buttons--close ()
  "Disable the persistent org-sm button bar."
  (interactive)
  (org-sm-buttons-mode -1))

(defun org-sm-buttons--button-text (label)
  "Return LABEL as one uniformly sized, indivisible touch target.
Non-breaking spaces let the built-in `fill-region' wrap *between* buttons,
not in the middle of a label."
  (let* ((base (concat (make-string org-sm-buttons-button-padding ? )
                       (string-replace " " " " label)
                       (make-string org-sm-buttons-button-padding ? )))
         (missing (max 0 (- org-sm-buttons-min-button-width
                            (string-width base)))))
    (concat (make-string (/ missing 2) ? )
            base
            (make-string (- missing (/ missing 2)) ? ))))

(defun org-sm-buttons--insert-button (label command face &optional help)
  "Insert a clickable LABEL that invokes COMMAND, styled with FACE.
The padded blank cells enlarge the target uniformly in GUI and terminal
Emacs.  `org-sm-buttons--render' uses built-in `fill-region' to wrap the
resulting button row."
  (insert-text-button (org-sm-buttons--button-text label)
                      'action (lambda (_button) (call-interactively command))
                      'face face
                      'follow-link t
                      'help-echo (or help label)
                      'keymap org-sm-buttons--button-map)
  (insert (make-string org-sm-buttons-button-gap ? )))

(defun org-sm-buttons--insert-header ()
  "Insert the status line for the current bar state."
  (let ((type (org-sm-buttons--current-type)))
    (insert (propertize "org-sm" 'face 'bold)
            "  "
            (cond
             ((not type) "No active review")
             ((eq type 'topic)
              (format "%d topic%s remaining"
                      (org-sm-buttons--remaining-count)
                      (if (= (org-sm-buttons--remaining-count) 1) "" "s")))
             ((eq (org-sm-buttons--cloze-state) 'hidden)
              (format "%d cloze%s remaining · answer hidden"
                      (org-sm-buttons--remaining-count)
                      (if (= (org-sm-buttons--remaining-count) 1) "" "s")))
             (t
              (format "%d cloze%s remaining · choose a rating"
                      (org-sm-buttons--remaining-count)
                      (if (= (org-sm-buttons--remaining-count) 1) "" "s")))))
    (insert "\n")))

(defun org-sm-buttons--insert-dismiss-action ()
  "Insert the appropriate safe dismissal control(s) for the current card."
  (if (org-sm-buttons--dismiss-pending-p)
      (progn
        (org-sm-buttons--insert-button "Confirm dismiss" #'org-sm-buttons--dismiss
                                       'org-sm-buttons-danger-face)
        (org-sm-buttons--insert-button "Cancel" #'org-sm-buttons--cancel-dismiss
                                       'org-sm-buttons-secondary-face))
    (org-sm-buttons--insert-button "Dismiss" #'org-sm-buttons--dismiss
                                   'org-sm-buttons-danger-face)))

(defun org-sm-buttons--insert-actions ()
  "Insert buttons appropriate for the active review state."
  (pcase (org-sm-buttons--current-type)
    ('topic
     (org-sm-buttons--insert-button "Reschedule" #'org-sm-buttons--topic-reschedule
                                    'org-sm-buttons-primary-face)
     (org-sm-buttons--insert-button
      (format "Postpone %dd" org-sm-buttons-postpone-days)
      #'org-sm-buttons--topic-postpone 'org-sm-buttons-secondary-face)
     (org-sm-buttons--insert-button "Skip" #'org-sm-buttons--skip
                                    'org-sm-buttons-secondary-face)
     (org-sm-buttons--insert-dismiss-action)
     (org-sm-buttons--insert-button "Abort" #'org-sm-buttons--abort
                                    'org-sm-buttons-secondary-face))
    ('cloze
     (if (eq (org-sm-buttons--cloze-state) 'hidden)
         (org-sm-buttons--insert-button "Reveal" #'org-sm-buttons--cloze-reveal
                                        'org-sm-buttons-primary-face)
       (dolist (rating org-sm-cloze-ratings)
         (let ((label (alist-get rating (org-sm-buttons--cloze-labels))))
           (org-sm-buttons--insert-button
            (format "%s (%s)" (capitalize (substring (symbol-name rating) 1)) label)
            (pcase rating
              (:again (lambda () (interactive) (org-sm-buttons--cloze-grade :again)))
              (:hard  (lambda () (interactive) (org-sm-buttons--cloze-grade :hard)))
              (:good  (lambda () (interactive) (org-sm-buttons--cloze-grade :good)))
              (:easy  (lambda () (interactive) (org-sm-buttons--cloze-grade :easy))))
            (pcase rating
              (:again 'org-sm-buttons-danger-face)
              (:good  'org-sm-buttons-primary-face)
              (_      'org-sm-buttons-secondary-face))))))
     (org-sm-buttons--insert-button "Skip" #'org-sm-buttons--skip
                                    'org-sm-buttons-secondary-face)
     (org-sm-buttons--insert-dismiss-action)
     (org-sm-buttons--insert-button "Abort" #'org-sm-buttons--abort
                                    'org-sm-buttons-secondary-face))
    (_
     (org-sm-buttons--insert-button "Start review" #'org-sm-buttons--start
                                    'org-sm-buttons-primary-face)
     (org-sm-buttons--insert-button "Close" #'org-sm-buttons--close
                                    'org-sm-buttons-secondary-face))))

(defun org-sm-buttons--render (&optional window)
  "Render the button bar for WINDOW, wrapping at its text width.
When WINDOW is nil, use an existing bar window or the selected window."
  (let ((window (or window (get-buffer-window (current-buffer) t) (selected-window)))
        (inhibit-read-only t))
    (let ((fill-column (max 20 (window-body-width window))))
      (erase-buffer)
      (org-sm-buttons--insert-header)
      (let ((actions-beg (point)))
        (org-sm-buttons--insert-actions)
        ;; Emacs already has a robust paragraph wrapper.  Button labels contain
        ;; non-breaking spaces, so it can only wrap at the separators between
        ;; whole buttons and keeps their text properties intact.
        (fill-region actions-beg (point-max)))
      (goto-char (point-min)))))

;;;###autoload
(defun org-sm-buttons-refresh ()
  "Refresh, wrap, and fit the bottom org-sm review-control window."
  (interactive)
  (when org-sm-buttons-mode
    (let* ((buffer (get-buffer-create org-sm-buttons--buffer-name))
           (window (display-buffer-in-side-window
                    buffer `((side . bottom)
                             (slot . 0)
                             (window-height . ,org-sm-buttons-window-height)))))
      (with-current-buffer buffer
        (org-sm-buttons-bar-mode)
        (org-sm-buttons--render window))
      (set-window-dedicated-p window t)
      (set-window-parameter window 'no-other-window t)
      (fit-window-to-buffer window org-sm-buttons-max-window-height 1))))

(defun org-sm-buttons--after-review-command (&rest _)
  "Refresh controls after an org-sm keyboard command completes."
  (when org-sm-buttons-mode
    (org-sm-buttons--clear-dismiss-confirmation)
    (org-sm-buttons-refresh)))

(defconst org-sm-buttons--advised-commands
  '(org-sm-review-start org-sm-review-confirm org-sm-item-dismiss org-sm-review-abort)
  "Public org-sm commands after which the button bar needs repainting.")

(define-derived-mode org-sm-buttons-bar-mode special-mode "org-sm Buttons"
  "Major mode for the non-editable org-sm review-control side window."
  (setq-local cursor-type nil)
  (setq-local truncate-lines nil))

;;;###autoload
(define-minor-mode org-sm-buttons-mode
  "Toggle the persistent mouse-driven org-sm review button bar.

The mode displays a bottom side window with Start review while idle and the
appropriate review actions while a card is active.  Buttons wrap to the
window width, have the same padded character-cell target in GUI and terminal
Emacs, and the bar fits its content.  It does not replace org-sm's scheduler;
each action delegates to existing core logic."
  :global t
  :group 'org-sm-buttons
  (if org-sm-buttons-mode
      (progn
        (dolist (command org-sm-buttons--advised-commands)
          (advice-add command :after #'org-sm-buttons--after-review-command))
        (org-sm-buttons-refresh))
    (dolist (command org-sm-buttons--advised-commands)
      (advice-remove command #'org-sm-buttons--after-review-command))
    (when-let* ((window (get-buffer-window org-sm-buttons--buffer-name t)))
      (delete-window window))))

(provide 'org-sm-buttons)

;;; org-sm-buttons.el ends here
