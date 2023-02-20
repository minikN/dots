(define-module (config elftower)
  #:use-module (config base)
  #:use-module (config packages)
  #:use-module (config features wm)

  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home-services wm)
  #:use-module (gnu system file-systems)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)

  #:use-module (rde features base)
  #:use-module (rde features fontutils)
  #:use-module (rde features linux)
  #:use-module (rde features system)
  #:use-module (rde features wm)
  #:use-module (rde features)

  #:export (elftower-config))

(define left 'DP-6)
(define right 'DP-5)
(define primary 'eDP-1)

(define sway-extra-config-service
  (simple-service
   'sway-extra-config
   home-sway-service-type
   `((output ,left pos 0 0)
     (output ,right pos 2560 0)
     (workspace 1 output ,left) ;; Browser
     (workspace 2 output ,right) ;; Terminal
     (workspace 3 output ,right) ;; Code
     (workspace 4 output ,right) ;; Agenda
     (workspace 5 output ,left) ;; Music/Video
     (workspace 6 output ,left) ;; Chat
     (workspace 7 output ,primary)
     (workspace 8 output ,primary)
     (workspace 9 output ,primary)
     (workspace 0 output ,primary)
     (output ,primary scale 1.5))))

(define elftower-filesystems
  (list (file-system ;; System partition
         (device (file-system-label "GUIX"))
         (mount-point "/")
         (type "btrfs"))
        (file-system ;; Boot partition
         (device (file-system-label "BOOT"))
         (mount-point "/boot/efi")
         (type "vfat"))))
(define elftower-features
  (list
   ;;; Host info
   (feature-host-info
    #:host-name "elftower"
    #:timezone  "Europe/Berlin"
    #:locale "en_US.utf8")

   ;;; Kernel
   (feature-kernel
    #:kernel linux
    #:initrd microcode-initrd
    #:initrd-modules '("vmd")
    #:firmware (list linux-firmware sof-firmware))

   ;;; File systems
   (feature-file-systems
    #:file-systems elftower-filesystems)

   ;;; Packages
   (feature-base-packages
    #:system-packages
    (append %base-system-packages)
    #:home-packages
    (append %base-home-packages))

   ;; Services
   (feature-custom-services
    #:home-services
    (list
     sway-extra-config-service
     (simple-service
      'set-locpath
      home-environment-variables-service-type
      '(("PATH" . "$PATH:$HOME/.local/bin")))))

   ;;; HiDPI
   (feature-hidpi)

   ;;; Backlight
   (feature-backlight)

   ;;; Sway

   ;;; Waybar
   (feature-waybar
    #:height 30
    #:output primary
    #:extra-config
    `(;; left bar
      ((position . top)
       (layer . top)
       (height . 30)
       (name . left)
       (output . ,left))
      ;; right bar
      ((position . top)
       (layer . top)
       (height . 30)
       (name . right)
       (output . ,right)))
    #:waybar-modules
    (append
     (waybar-left-modules)
     waybar-right-modules
     waybar-primary-modules))))

(define elftower-config
  (rde-config
   (features
    (append
     %base-features
     elftower-features))))
