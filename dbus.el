(require 'dbus)

(defmacro pidgin-dbus-purple-call-method (method &rest args)
  `(dbus-call-method :session "im.pidgin.purple.PurpleService"
                     "/im/pidgin/purple/PurpleObject"
                     "im.pidgin.purple.PurpleInterface"
                     ,method ,@args))

(defun dbus-account-ids () (pidgin-dbus-purple-call-method "PurpleAccountsGetAllActive"))

(defun dbus-get-user (account-id)
  (let ((alias (pidgin-dbus-purple-call-method "PurpleAccountGetAlias" :int32 account-id)))
    (list 'id account-id 'alias alias)))
