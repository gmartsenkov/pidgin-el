(load "./dbus.el")

(dbus-get-active-accounts)
(dbus-get-buddies-all 282)
(dbus-get-conversations)
(dbus-get-conversation-history 160922)

;; (ivy-read "Pick a person: "
;; '("one" "two" "three"))

(defun create-or-switch-buffer (name)
  (if-let ((buffer (get-buffer name)))
      buffer
    (generate-new-buffer name)))

(defun pidgin-select-conversation ()
  (interactive)
  (let ((conversations (dbus-get-conversations)))
    (ivy-read "Conversations: "
              (mapcar (lambda (x) (plist-get x 'title)) conversations)
              :action (lambda (name) (-> name
                                       (create-or-switch-buffer)
                                       (switch-to-buffer nil 'force-same-window)))
              :require-match t)))

(pidgin-select-conversation)
