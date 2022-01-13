(define-module (features linux)
  #:use-module (gnu home services)
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
     (service bluetooth-service-type
              (bluetooth-configuration
               (auto-enable? #t)))
     (simple-service
      'dbus-services
      dbus-root-service-type
      (list blueman))))

  (define (get-home-services _)
    (list

     (simple-service
      'bluetooth-add-packages
      home-profile-service-type
      (append (list blueman bluez)))))

  (feature
   (name 'bluetooth)
   (values '((bluetooth . #t)))
   (system-services-getter get-system-services)
   (home-services-getter get-home-services)))
