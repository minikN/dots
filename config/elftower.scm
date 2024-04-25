(define-module (config elftower)
  #:use-module (config file-systems nas)

  #:use-module (config base)
  #:use-module (config packages)
  #:use-module (config features wm)

  #:use-module (gnu packages)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home-services wm)
  #:use-module (gnu system file-systems)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)

  #:use-module (rde features base)
  #:use-module (rde features docker)
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
  (append
   (list (file-system ;; System partition
          (device (file-system-label "GUIX"))
          (mount-point "/")
          (type "btrfs"))
         (file-system ;; Boot partition
          (device (file-system-label "BOOT"))
          (mount-point "/boot/efi")
          (type "vfat")))
   %nas-filesystems))

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
  #:kernel-arguments (list "efi_pstore.pstore_disable=1")
  #:kernel-loadable-modules (list tp-smapi-module)
  #:firmware
  (list
   linux-firmware
   sof-firmware
   i915-firmware))

   ;;; File systems
  (feature-file-systems
   #:file-systems elftower-filesystems)

  (feature-custom-services
   #:system-services
   (append %nas-mount-services))

   ;;; Packages
  (feature-base-packages
   #:system-packages
   (append %base-system-packages)
   #:home-packages
   (append %base-home-packages))


   ;;; HiDPI
  (feature-hidpi)

   ;;; Backlight
  (feature-backlight)

   ;;; Docker
  ;; curl -SL https://github.com/docker/compose/releases/download/v2.17.2/docker-compose-linux-x86_64 -o $HOME/.docker/cli-plugins/docker-compose
  ;; sudo chmod +x $HOME/.docker/cli-plugins/docker-compose
  (feature-docker)

   ;;; Waybar
  (feature-waybar
   #:waybar-modules
   waybar-main-modules)))

(define elftower-config
  (rde-config
   (features
    (append
     %base-features
     elftower-features))))
