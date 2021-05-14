(require 'pidgin-dbus)

(setq accounts (dbus-get-active-accounts))
(setq pidgin-current-conv nil)

(defun strip-text-properties (txt)
  (set-text-properties 0 (length txt) nil txt) txt)

(defun pidgin-send-message ()
  (interactive)
  (let* ((buffer (strip-text-properties (buffer-string)))
         (msg (nth 0 (last (split-string buffer "####\n> ")))))
    (beginning-of-line)
    (replace-string msg "")
    (dbus-send-im-message (plist-get pidgin-current-conv 'im-id) msg)))

(defvar pidgin-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'pidgin-send-message)
    (define-key map (kbd "RET") 'pidgin-send-message)
    map))

(define-derived-mode pidgin-mode text-mode "Pidgin"
  (use-local-map pidgin-mode-map)
  (message "Hit"))

(defface pidgin-chat-me
  '((t :foreground "red" :weight bold))
  "Face for pidgin chat logged in user name"
  :group 'pidgin-chat-faces)

(defface pidgin-chat-other-user
  '((t :foreground "blue" :weight bold))
  "Face for other user in a pidgin chat"
  :group 'pidgin-chat-faces)

(defun sender-style (sender)
  (let* ((my-names (flatten-list
                    (mapcar (lambda (x)
                              (list (plist-get x 'user-name) (plist-get x 'alias))) accounts)))
         (me (member sender my-names)))
    (if me
        (propertize sender 'face 'pidgin-chat-me 'read-only t)
      (propertize sender 'face 'pidgin-chat-other-user 'read-only t))))

(defun format-msg (msg)
  (concat
   (sender-style (plist-get msg 'sender)) ": "
   (propertize (plist-get msg 'message) 'read-only t) "\n"))

(defun populate-conversation-buffer (conversation-name buffer)
  (let* ((conversation (dbus-get-conversation-by-name conversation-name))
         (history (dbus-get-conversation-history (plist-get conversation 'id))))
    (with-current-buffer buffer
      (pidgin-mode)
      (make-local-variable 'pidgin-current-conv)
      (setq pidgin-current-conv conversation)

      (dolist (msg history)
        (insert (format-msg msg)))
      (insert (concat (propertize "####\n>" 'read-only t) " ")))
    buffer))

(defun create-conversation-buffer (conversation-name buff-name)
  (if-let ((buffer (get-buffer buff-name)))
      buffer
    (->> buff-name
      (generate-new-buffer)
      (populate-conversation-buffer conversation-name))))

(defun pidgin-select-conversation ()
  (interactive)
  (let* ((conversations (dbus-get-conversations))
         (conversation-name (ivy-read "Conversations: "
                                      (mapcar (lambda (x) (plist-get x 'title)) conversations)
                                      :require-match t)))
    (-> conversation-name
      (create-conversation-buffer (concat "Pidgin: " conversation-name))
      (switch-to-buffer nil 'force-same-window))))

(pidgin-select-conversation)
