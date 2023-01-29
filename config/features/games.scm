(define-module (config features games)
  #:use-module (config packages)
  #:use-module (gnu home services)
  #:use-module (gnu home-services shells)
  #:use-module (gnu home-services wm)
  #:use-module (gnu packages base)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages wine)
  #:use-module (gnu packages games)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nongnu packages wine)
  #:use-module (nongnu packages steam-client)
  #:use-module (rde features predicates)
  #:use-module (rde features)
  #:export (feature-steam))

(define* (feature-steam
          #:key
          (steam steam)
          (steamos? #f)
          (steamos-tty-number 3)
          (sandbox-location #f)
          (gpu-driver xf86-video-amdgpu))
  "Install and configure steam."

  (ensure-pred any-package? steam)
  (ensure-pred any-package? gpu-driver)
  (ensure-pred boolean? steamos?)

  (define (get-home-services config)
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
                 xrandr
                 xf86-input-libinput
                 xf86-input-keyboard
                 xf86-input-mouse
                 xf86-input-joystick
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
                              #$(file-append steamos-compositor-plus "/bin/steamos-session")
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

  (define (get-system-services config)
    (list
     (udev-rules-service 'steam-devices steam-devices-udev-rules)))

  (feature
   (name 'steam)
   (values '((steam . #t)))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))
