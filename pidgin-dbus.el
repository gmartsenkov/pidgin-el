(require 'dbus)

(defmacro pidgin-dbus-purple-call-method (method &rest args)
  `(dbus-call-method :session "im.pidgin.purple.PurpleService"
                     "/im/pidgin/purple/PurpleObject"
                     "im.pidgin.purple.PurpleInterface"
                     ,method ,@args))

(defun pidgin-recieve-signal (account sender text conversation flags)
  (message sender)
  (message text))

;; (dbus-register-signal :session "im.pidgin.purple.PurpleService"
;;                           "/im/pidgin/purple/PurpleObject"
;;                           "im.pidgin.purple.PurpleInterface"
;;                           "ReceivedImMsg"
;;                           'pidgin-recieve-signal)

(defun dbus-get-account (account-id)
  (let ((alias (pidgin-dbus-purple-call-method "PurpleAccountGetAlias" :int32 account-id))
        (name (pidgin-dbus-purple-call-method "PurpleAccountGetUsername" :int32 account-id)))
    (list 'id account-id 'alias alias 'user-name name)))

(defun dbus-get-active-accounts ()
  (let ((account-ids (pidgin-dbus-purple-call-method "PurpleAccountsGetAllActive")))
    (mapcar 'dbus-get-account account-ids)))

(defun get-buddy (buddy-id)
  (let ((alias (pidgin-dbus-purple-call-method "PurpleBuddyGetAlias" :int32 buddy-id))
        (online (pidgin-dbus-purple-call-method "PurpleBuddyIsOnline" :int32 buddy-id))
        (name (pidgin-dbus-purple-call-method "PurpleBuddyGetName" :int32 buddy-id)))
    (list 'id buddy-id 'alias alias 'name name 'online online)))

(defun dbus-get-buddies-all ()
  (let ((buddies (pidgin-dbus-purple-call-method "PurpleBlistGetBuddies")))
    (mapcar 'get-buddy buddies)))

(defun get-conversation (conversation-id)
  (let ((title (pidgin-dbus-purple-call-method "PurpleConversationGetTitle" :int32 conversation-id))
        (type (pidgin-dbus-purple-call-method "PurpleConversationGetType" :int32 conversation-id))
        (im-id (pidgin-dbus-purple-call-method "PurpleConvIm" :int32 conversation-id)))
    (list 'id conversation-id
          'title title
          'type type
          'im-id im-id)))

(defun dbus-get-conversations ()
  (let ((conversations (pidgin-dbus-purple-call-method "PurpleGetConversations")))
    (mapcar 'get-conversation conversations)))

(defun get-message (message-id)
  (let ((sender (pidgin-dbus-purple-call-method "PurpleConversationMessageGetSender" :int32 message-id))
        (timestamp (pidgin-dbus-purple-call-method "PurpleConversationMessageGetTimestamp" :int32 message-id))
        (msg (pidgin-dbus-purple-call-method "PurpleConversationMessageGetMessage" :int32 message-id)))
    (list 'id message-id
          'message msg
          'sender sender
          'timestamp timestamp)))

(defun dbus-get-conversation-history (conversation-id)
  (let ((message-history (pidgin-dbus-purple-call-method "PurpleConversationGetMessageHistory" :int32 conversation-id)))
    (mapcar 'get-message message-history)))

(defun dbus-get-conversation-by-name (name)
  (seq-find (lambda (x) (string= name (plist-get x 'title))) (dbus-get-conversations)))

(defun dbus-send-im-message (im-id msg)
  (pidgin-dbus-purple-call-method "PurpleConvImSend" :int32 im-id :string msg))

(provide 'pidgin-dbus)
