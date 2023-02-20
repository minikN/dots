(define-module (config geekcave)
  #:use-module (config base)
  #:use-module (config features games)
  #:use-module (config packages)
  #:use-module (config features wm)
  ;; #:use-module (config features engineering)

  #:use-module (gnu services)
  #:use-module (gnu home-services wm)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages music)
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

  #:export (geekcave-config))

(define primary 'DP-1)
(define right 'DP-2)

(define sway-extra-config-service
  (simple-service
   'sway-extra-config
   home-sway-service-type
   `((output ,primary pos 0 0)
     (output ,right pos 2560 0)
     (workspace 1 output ,primary)   ;; Browser
     (workspace 2 output ,right)   ;; Terminal
     (workspace 3 output ,right)   ;; Code
     (workspace 4 output ,right)   ;; Agenda
     (workspace 5 output ,primary)   ;; Music/Video
     (workspace 6 output ,primary)   ;; Chat
     (workspace 7 output ,primary))))

(define geekcave-filesystems
  (list (file-system ;; System partition
         (device (file-system-label "GUIX"))
         (mount-point "/")
         (type "btrfs"))
        (file-system ;; System partition
         (device (file-system-label "GAMES"))
         (mount-point "/home/db/Games")
         (type "btrfs"))
        (file-system ;; Boot partition
         (device (file-system-label "BOOT"))
         (mount-point "/boot/efi")
         (type "vfat"))))

(define geekcave-packages
  (list
   rofi-ttv

   guitarix
   guitarix-lv2

   carla
   jack-1
   qjackctl))

(define geekcave-features
  (list
   ;;; Host info
   (feature-host-info
    #:host-name "geekcave"
    #:timezone  "Europe/Berlin"
    #:locale "en_US.utf8")

   ;;; Kernel
   (feature-kernel
    #:kernel linux
    #:kernel-arguments (list "modprobe.blacklist=nouveau")
    #:initrd microcode-initrd
    #:firmware (list amdgpu-firmware linux-firmware))

   ;;; File systems
   (feature-file-systems
    #:file-systems geekcave-filesystems)

   ;;; Packages
   (feature-base-packages
    #:system-packages
    (append %base-system-packages)
    #:home-packages
    (append %base-home-packages
            geekcave-packages))

   ;;; Services
   (feature-custom-services
    #:home-services
    (list
     sway-extra-config-service
     ssh-extra-config-service))

   ;;; HiDPI
   (feature-hidpi)

   (feature-steam
    #:steamos? #t
    #:sandbox-location
    (string-append
     (getenv "HOME")
     "/Games/steam-standbox"))

   ;; nix-env -iA nixpkgs.docker-compose
   (feature-docker)

   ;; Waybar
   (feature-waybar
    #:height 30
    #:output primary
    #:extra-config
    `(((position . top)
       (layer . top)
       (height . 30)
       (name . right)
       (output . ,right)))
    #:waybar-modules
    (append
     (waybar-left-modules #:bar-id 'main)
     waybar-right-modules))))

(define geekcave-config
  (rde-config
   (features
    (append
     %base-features
     geekcave-features))))
