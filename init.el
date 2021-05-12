(load "./dbus.el")

(dbus-get-active-accounts)
(dbus-get-buddies-all 282)
(dbus-get-conversations)
(dbus-get-conversation-history 160922)

;; (ivy-read "Pick a person: "
;; '("one" "two" "three"))
