(load "./dbus.el")

(dbus-get-active-accounts)
(dbus-get-buddies-all)
(dbus-get-conversations)
(dbus-get-conversation-history 160922)

;; (ivy-read "Pick a person: "
;; '("one" "two" "three"))

(setq accounts (dbus-get-active-accounts))

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
        (propertize sender 'face 'pidgin-chat-me)
      (propertize sender 'face 'pidgin-chat-other-user))))

(defun format-msg (msg)
  (concat
   (sender-style (plist-get msg 'sender)) ": "
   (plist-get msg 'message) "\n"))

(defun populate-conversation-buffer (conversation-name buffer)
  (let* ((conversation (dbus-get-conversation-by-name conversation-name))
         (history (dbus-get-conversation-history (plist-get conversation 'id))))
    (with-current-buffer buffer
      (dolist (msg history)
        (insert (format-msg msg)))
      (text-mode))
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
