(define-module (config features linux)
  #:use-module (gnu home services)
  #:use-module (gnu home-services wm)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (rde features predicates)
  #:use-module (rde features)
  #:use-module (rde packages)
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

  (define (get-home-services config)
    (list
     (simple-service
      'bluetooth-add-packages
      home-profile-service-type
      (append (list blueman bluez)))
     (when (get-value 'sway config)
       (simple-service
	'emacs-update-environment-variables-on-sway-start
	home-sway-service-type
	`((for_window "[app_id=\".blueman-manager-real\"]" floating enable, border pixel)
          (for_window "[app_id=\".blueman-services-real\"]" floating enable, border pixel)
          (exec blueman-applet))))))

  (feature
   (name 'bluetooth)
   (values '((bluetooth . #t)))
   (system-services-getter get-system-services)
   (home-services-getter get-home-services)))
