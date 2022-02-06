(define-module (personal features games)
  #:use-module (gnu home services)
  #:use-module (gnu home-services wm)
  #:use-module (gnu packages base)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages wine)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (nongnu packages steam-client)
  #:use-module (nongnu packages wine)
  #:use-module (rde features predicates)
  #:use-module (rde features)
  #:export (feature-games-base
            feature-games-steam))

(define* (feature-games-base)
  "Install and configure base packages for gaming."

  (define (get-home-services config)
    (list
     (simple-service
      'steam-add-packages
      home-profile-service-type
      (append
       (list spirv-cross
             spirv-headers
             spirv-tools
             vkd3d
             ;dxvk-next
             wine
             ;wine-staging
             winetricks
             vulkan-loader
             vulkan-tools
             vulkan-headers)))))

  (feature
   (name 'games-base)
   (values '((games-base . #t)))
   (home-services-getter get-home-services)))

(define* (feature-games-steam)
  "Install and configure steam."

  (define (get-home-services config)
    (require-value 'games-base config "feature-games-base is required.")
    (list
     (simple-service
      'steam-add-packages
      home-profile-service-type
      (append
       (list steam)))
     (when (get-value 'sway config)
       (simple-service
	'emacs-update-environment-variables-on-sway-start
	home-sway-service-type
	`((assign "[class=\"Steam\"]" workspace 7)
          (assign "[class=\"^steam_.$\"]" workspace 7))))))

  (feature
   (name 'games-steam)
   (values '((games-steam . #t)))
   (home-services-getter get-home-services)))
