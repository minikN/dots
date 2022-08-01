(define-module (config features games)
  #:use-module (gnu home services)
  #:use-module (gnu home-services shells)
  #:use-module (gnu home-services wm)
  #:use-module (gnu packages base)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages wine)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xorg)
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
       (list ;; dxvk-next
             ;; wine-staging
             mesa
             mesa-headers
             mesa-opencl
             mesa-opencl-icd
             mesa-utils
             spirv-cross
             spirv-headers
             spirv-tools
             vkd3d
             vulkan-headers
             vulkan-loader
             vulkan-tools
             wine
             winetricks)))))

  (feature
   (name 'games-base)
   (values '((games-base . #t)))
   (home-services-getter get-home-services)))

(define* (feature-games-steam
          #:key
          (steam steam)
          (sandbox-location #f)
          (steamos? #f)
          (steamos-tty-number 3)
          (gpu-driver xf86-video-amdgpu))
  "Install and configure steam."

  (ensure-pred any-package? steam)
  (ensure-pred any-package? gpu-driver)
  (ensure-pred boolean? steamos?)

  (define (get-home-services config)
    (require-value 'games-base config "feature-games-base is required.")
    (list
     (simple-service
      'steam-add-packages
      home-profile-service-type
      (append
       (list steam)
       (if steamos?
           ;; In home streaming on wayland is still bugged. So we need
           ;; to setup an x server for this. But hey, issue is just open
           ;; since 2019.
           ;; https://github.com/ValveSoftware/steam-for-linux/issues/6148
           (list xorg-server
                 xinit
                 xf86-input-libinput
                 xf86-video-fbdev
                 gpu-driver)
           '())))
     (when steamos?
       (let ((script (computed-file
                      "steamos"
                      #~(call-with-output-file #$output
                          (lambda (file)
                            (display
                             (string-append
                              "#!/bin/sh\nDIR=$HOME/.guix-home/profile\n"
                              #$(file-append xinit "/bin/xinit" " ")
                              #$(file-append coreutils "/bin/env" " " "-u" " " "SDL_VIDEODRIVER" " ")
                              #$(file-append steam "/bin/steam" " " "-bigpicture")
                              " -- "
                              #$(file-append xorg-server "/bin/Xorg" " " ":1" " " "vt")
                              #$(number->string steamos-tty-number)
                              " -keeptty "
                              "-configdir $DIR/share/X11/xorg.conf.d "
                              "-modulepath $DIR/lib/xorg/modules")
                             file)
                            (chmod file #o744))))))
         (simple-service
          'run-steamos-on-login
          home-shell-profile-service-type
          (list
           #~(format #f "[ $(tty) = /dev/tty~a ] && exec ~a"
                     #$steamos-tty-number
                     #$(file-append script))))))
     (when sandbox-location
       (simple-service 'steam-set-sandbox-location
		       home-environment-variables-service-type
		       `(("GUIX_SANDBOX_HOME" . ,sandbox-location))))
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
