(define-module (config features package-management)
  #:use-module (gnu home services)
  #:use-module (gnu home-services shells)
  #:use-module (gnu services)
  #:use-module (gnu services nix)
  #:use-module (gnu packages base)
  #:use-module (gnu packages package-management)
  #:use-module (rde features predicates)
  #:use-module (rde features)
  #:export (feature-nix))

(define* (feature-nix)
  "Install and configure the nix package manager."

  (define (get-system-services _)
    (list
     (service nix-service-type)))

  (define (get-home-services config)
    (list
     (simple-service
      'setup-nix
      home-shell-profile-service-type
      (list "source /run/current-system/profile/etc/profile.d/nix.sh"))
     (simple-service
      'setup-nix-env-vars
      home-environment-variables-service-type
      `(("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.nix-profile/share")
        ("NIXPKGS_ALLOW_UNFREE" . "1")))))

  (feature
   (name 'nix)
   (values '((nix . #t)))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))
feature-nix
