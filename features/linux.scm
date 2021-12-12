(define-module (features linux)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages linux)
  #:use-module (guix gexp)
  #:use-module (rde packages)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:export (feature-bluetooth))

(define* (feature-bluetooth)
  "Setup and configure bluetooth."

  (define (get-system-services _)
    (list
     (simple-service
      'dbus-services
      dbus-root-service-type
      (list blueman))

     (service
      bluetooth-service-type)

;;      (simple-service
;;       'bluetooth-add-config
;;       etc-service-type
;;       `(("bluetooth/audio.conf"
;;          ,(plain-file
;;            "audio.conf"
;;            "[General]
;; AutoConnect=true
     ;; MultiProfile = multiple
     ;; [Headset]
     ;; HFP = false"))))
     (extra-special-file
      "/etc/bluetooth/audio.conf"
      (plain-file
        "audio.conf"
        "[General]
AutoConnect=true
MultiProfile = multiple
[Headset]
HFP = false"))

     
     (simple-service
      'bluetooth-add-packages
      profile-service-type
      (append (list blueman bluez)))))

  (feature
   (name 'bluetooth)
   (values '((bluetooth . #t)))
   (system-services-getter get-system-services)))
