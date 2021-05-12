(require 'dbus)

(defmacro pidgin-dbus-purple-call-method (method &rest args)
  `(dbus-call-method :session "im.pidgin.purple.PurpleService"
                     "/im/pidgin/purple/PurpleObject"
                     "im.pidgin.purple.PurpleInterface"
                     ,method ,@args))

(defun dbus-get-account (account-id)
  (let ((alias (pidgin-dbus-purple-call-method "PurpleAccountGetAlias" :int32 account-id)))
    (list 'id account-id 'alias alias)))

(defun dbus-get-active-accounts ()
  (let ((account-ids (pidgin-dbus-purple-call-method "PurpleAccountsGetAllActive")))
    (mapcar 'dbus-get-account account-ids)))

(defun get-buddy (buddy-id)
  (let ((alias (pidgin-dbus-purple-call-method "PurpleBuddyGetAlias" :int32 buddy-id)))
    (list 'id buddy-id 'alias alias)))

(defun dbus-get-buddies-all (account-id)
  (let ((buddies (pidgin-dbus-purple-call-method "PurpleFindBuddies" :int32 account-id :string "")))
    (mapcar 'get-buddy buddies)))

(defun get-conversation (conversation-id)
  (let ((title (pidgin-dbus-purple-call-method "PurpleConversationGetTitle" :int32 conversation-id))
        (type (pidgin-dbus-purple-call-method "PurpleConversationGetType" :int32 conversation-id)))
    (list 'id conversation-id
          'title title
          'type type)))

(defun dbus-get-conversations ()
  (let ((conversations (pidgin-dbus-purple-call-method "PurpleGetConversations")))
    (mapcar 'get-conversation conversations)))
