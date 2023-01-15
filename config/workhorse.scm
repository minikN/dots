;;; This machine is meant to run Debian 11 with RDE on top!
;;; First install Debian 11.6 without any DE.
;;;
;;; Install packages: firmware_linux, git, make,
;;; network-manager, nscd, policykit-1
;;; (remove git, make after Guix installation)
;;;
;;; Install Guix through install script:
;;; https://guix.gnu.org/en/manual/en/guix.html#Binary-Installation
;;;
;;; Reconfigure home with RDE config
;;;
;;; Newer kernel
;;; Add backports repository
;;; echo 'deb http://deb.debian.org/debian bullseye-backports main' \
;;; | sudo tee -a /etc/apt/sources.list
;;;
;;; Hardware stuff
;;; Add tuxedo repository:
;;; wget -O - https://deb.tuxedocomputers.com/0x54840598.pub.asc \
;;; | gpg --dearmor > 0x54840598.pub.gpg
;;;
;;; cat 0x54840598.pub.gpg \
;;; | sudo tee -a /usr/share/keyrings/tuxedo-keyring.gpg > /dev/null
;;;
;;; echo 'deb [arch=amd64 signed-by=/usr/share/keyrings/tuxedo-keyring.gpg] \
;;; https://deb.tuxedocomputers.com/ubuntu jammy main' \
;;; | sudo tee -a /etc/apt/sources.list.d/tuxedo-computers.list

;;; Installed packages (apt):
;; ffmpeg
;; firmware-linux
;; libdrm2
;; libgbm1
;; libnss3
;; libxshmfence1
;; linux-headers-6.0.0-0.deb11.6-amd64
;; linux-image-6.0.0-0.deb11.6-amd64
;; network-manager
;; nscd
;; policykit-1
;; sudo
;; tuxedo-restore-audio-fix
;; tuxedo-touchpad-fix
;; tuxedo-touchpad-switch

(define-module (config workhorse)
  #:use-module (config base)
  #:use-module (config packages)

  #:use-module (gnu home services)
  #:use-module (gnu home-services shells)
  #:use-module (gnu packages base)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu system file-systems)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)

  #:use-module (rde features base)
  #:use-module (rde features fontutils)
  #:use-module (rde features linux)
  #:use-module (rde features system)
  #:use-module (rde features wm)
  #:use-module (rde features)

  #:export (workhorse-config))

(define workhorse-packages
  (list glibc-locales
        nss-certs))

(define workhorse-sway-config
  `((output DP-1 pos 0 0)
    (output DP-2 pos 2560 0)
    (workspace 1 output DP-1) ;; Browser
    (workspace 2 output DP-2) ;; Terminal
    (workspace 3 output DP-2) ;; Code
    (workspace 4 output DP-2) ;; Agenda
    (workspace 5 output DP-1) ;; Music/Video
    (workspace 6 output DP-1) ;; Chat
    (output eDP-1 scale 1.5)))

(define workhorse-filesystems
  (list (file-system ;; System partition
         (device (file-system-label "GUIX"))
         (mount-point "/")
         (type "btrfs"))
        (file-system ;; Boot partition
         (device (file-system-label "BOOT"))
         (mount-point "/boot/efi")
         (type "vfat"))))

(define workhorse-features
  (list
   ;;; Host info
   (feature-host-info
    #:host-name "workhorse"
    #:timezone  "Europe/Berlin"
    #:locale "en_US.utf8")

   ;;; Kernel
   (feature-kernel
    #:kernel linux
    #:kernel-arguments (list "modprobe.blacklist=nouveau")
    #:initrd microcode-initrd
    #:initrd-modules '("vmd")
    #:firmware (list linux-firmware sof-firmware))

   ;;; File systems
   (feature-file-systems
    #:file-systems workhorse-filesystems)

   ;;; Packages
   (feature-base-packages
    #:system-packages
    (append %base-system-packages)
    #:home-packages
    (append %base-home-packages
            workhorse-packages))

   ;;; HiDPI
   (feature-hidpi)

   ;;; Backlight
   (feature-backlight)

   ;;; Services
   (feature-custom-services
    #:home-services
    (list
     (simple-service
      'set-locpath
      home-environment-variables-service-type
      '(("SSL_CERT_DIR" . "$HOME/.guix-home/profile/etc/ssl/certs")
        ("SSL_CERT_FILE" . "$HOME/.guix-home/profile/etc/ssl/certs/ca-certificates.crt")
        ("GUIX_LOCPATH" . "$HOME/.guix-home/profile/lib/locale")
        ("LC_ALL" . "en_US.utf8")))))

   ;;; Sway
   (feature-sway
    #:xwayland? #t
    #:extra-config
    (append %base-sway-config
            workhorse-sway-config))
   (feature-sway-run-on-tty #:sway-tty-number 2)
   (feature-sway-screenshot)

   (feature-waybar
    #:output 'eDP-1
    #:height 30
    #:extra-config
    '(((position . top)
       (layer . top)
       (height . 30)
       (name . left)
       (output . DP-1))
      ((position . top)
       (layer . top)
       (height . 30)
       (name . right)
       (output . DP-2)))
    #:waybar-modules
    (list
     (waybar-sway-workspaces
      #:bar-id 'left
      #:format-icons
      '(("1" . " WWW")
        ("5" . " MUSIC")
        ("6" . " CHAT")
        ("urgent" . )
        ("focused" . )
        ("default" . ))
      #:persistent-workspaces
      '(("1" . #())
        ("5" . #())
        ("6" . #())))
     (waybar-sway-workspaces
      #:bar-id 'right
      #:format-icons
      '(("2" . " TERM")
        ("3" . " CODE")
        ("4" . " AGENDA")
        ("urgent" . )
        ("focused" . )
        ("default" . ))
      #:persistent-workspaces
      '(("2" . #())
        ("3" . #())
        ("4" . #())))
     (waybar-sway-workspaces
      #:format-icons
      '(("1" . " WWW")
        ("2" . " TERM")
        ("3" . " CODE")
        ("4" . " AGENDA")
        ("5" . " MUSIC")
        ("6" . " CHAT")
        ("urgent" . )
        ("focused" . )
        ("default" . ))
      #:persistent-workspaces
      '(("1" . #())
        ("2" . #())
        ("3" . #())
        ("4" . #())
        ("5" . #())
        ("6" . #())))
     (waybar-sway-window)
     (waybar-cpu #:bar-id 'right)
     (waybar-memory #:bar-id 'right)
     (waybar-disk #:bar-id 'right)
     (waybar-temperature #:bar-id 'right)
     (waybar-battery #:bar-id 'right)
     (waybar-volume
      #:bar-id 'right
      #:show-percentage? #t
      #:scroll-step 5)
     (waybar-tray #:bar-id 'right)
     (waybar-clock
      #:bar-id 'right
      #:format "{:%H:%M}")
     (waybar-cpu #:bar-id 'main)
     (waybar-memory #:bar-id 'main)
     (waybar-disk #:bar-id 'main)
     (waybar-temperature #:bar-id 'main)
     (waybar-battery #:bar-id 'main)
     (waybar-volume
      #:bar-id 'main
      #:show-percentage? #t
      #:scroll-step 5)
     (waybar-tray #:bar-id 'main)
     (waybar-clock
      #:bar-id 'main
      #:format "{:%H:%M}")))))

(define workhorse-config
  (rde-config
   (features
    (append
     %base-features
     workhorse-features))))
