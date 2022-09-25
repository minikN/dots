(define-module (config features games)
  #:use-module (config packages)
  #:use-module (gnu home services)
  #:use-module (gnu home-services shells)
  #:use-module (gnu home-services wm)
  #:use-module (gnu packages base)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages wine)
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
  #:export (feature-games-base
            feature-games-steam))

;; Source:
;; https://github.com/podiki/dot.me/blob/master/guix/.config/guix/config.scm#L57
(define %steam-input-udev-rules
  (file->udev-rule
    "60-steam-input.rules"
    (let ((version "8a3f1a0e2d208b670aafd5d65e216c71f75f1684"))
      (origin
       (method url-fetch)
       (uri (string-append "https://raw.githubusercontent.com/ValveSoftware/"
                           "steam-devices/" version "/60-steam-input.rules"))
       (sha256
        (base32 "1k6sa9y6qb9vh7qsgvpgfza55ilcsvhvmli60yfz0mlks8skcd1f"))))))

(define %steam-vr-udev-rules
  (file->udev-rule
    "60-steam-vr.rules"
    (let ((version "13847addfd56ef70dee98c1f7e14c4b4079e2ce8"))
      (origin
       (method url-fetch)
       (uri (string-append "https://raw.githubusercontent.com/ValveSoftware/"
                           "steam-devices/" version "/60-steam-vr.rules"))
       (sha256
        (base32 "0f05w4jp2pfp948vwwqa17ym2ps7sgh3i6sdc69ha76jlm49rp0z"))))))

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
          (steamos? #f)
          (steamos-tty-number 3)
          (sandbox-location #f)
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
                              ;; #$(file-append pulseaudio "/bin/pactl" " " "load-module module-null-sink sink_name=dummy && ")
                              ;; #$(file-append pulseaudio "/bin/pactl" " " "load-module module-loopback && ")
                              #$(file-append xinit "/bin/xinit" " ")
                              #$(file-append coreutils "/bin/env" " " "-u" " " "SDL_VIDEODRIVER" " ")
                              ;; #$(file-append steam "/bin/steam" " " "-tenfoot -steamos -fulldesktopres -nointro")
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
     ;; (when (get-value 'pipewire config)
     ;;   (simple-service
     ;;    'pipewire-add-config
     ;;    etc-service-type
     ;;    (list
     ;;     `("pipewire/pipewire.conf"
     ;;       ,(plain-file
     ;;         "pipewire.conf"
     ;;         "default.clock.allowed-rates = [ 48000 ]\n")))))
     ;; Add udev rules for steam input devices.
     (udev-rules-service 'steam-input %steam-input-udev-rules)
     (udev-rules-service 'steam-vr %steam-vr-udev-rules)))

  (feature
   (name 'games-steam)
   (values '((games-steam . #t)))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))
