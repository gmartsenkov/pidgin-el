(load "./dbus.el")

(dbus-get-active-accounts)
(dbus-get-buddies-all 282)
(dbus-get-conversations)
(dbus-get-conversation-history 160922)

;; (ivy-read "Pick a person: "
;; '("one" "two" "three"))

(defun format-msg (msg)
  (concat
   (plist-get msg 'sender) ": "
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
