(load "./dbus.el")

(let ((account-ids (dbus-account-ids)))
  (mapcar 'dbus-get-user account-ids))

;; (ivy-read "Pick a person: "
;; '("one" "two" "three"))
