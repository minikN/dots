(define-module (features applauncher)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix gexp)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:export (feature-bemenu))

(define* (feature-bemenu
          #:key
          (default-app-launcher? #t)
          (package bemenu)
          (cmd "/bin/bemenu-run"))
  "Setup and configure bemenu.q"
  (ensure-pred package? package)
  (ensure-pred string? cmd)
  (ensure-pred boolean? default-app-launcher?)

  (feature
   (name 'bemenu)
   (values `((bemenu . ,package)
             (default-app-launcher . ,(file-append package cmd))
             ,@(when default-app-launcher?
                 `((default-app-launcher . ,(file-append package cmd)))
                 '())))))
