(define-module (config workhorse)
  #:use-module (config base)
  #:use-module (config packages)
  #:use-module (gnu packages)
  #:use-module (gnu system file-systems)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features linux)
  #:use-module (rde features system)
  #:use-module (rde features fontutils)
  #:use-module (rde features wm)
  #:export (workhorse-config))

(define workhorse-sway-config
  `((output DP-1 pos 0 0)
    (output DP-2 pos 2560 0)
    (workspace 1 output DP-1) ;; Browser
    (workspace 2 output DP-2) ;; Terminal
    (workspace 3 output DP-2) ;; Code
    (workspace 4 output DP-2) ;; Agenda
    (workspace 5 output DP-1) ;; Music/Video
    (workspace 6 output DP-1) ;; Chat
    (workspace 7 output DP-1) ;; Games
    (output eDP-1 scale 1.5)))

(define workhorse-sway-config
  `((output DP-1 pos 0 0)
    (output DP-2 pos 2560 0)
    (workspace 1 output DP-1)   ;; Browser
    (workspace 2 output DP-2)   ;; Terminal
    (workspace 3 output DP-2)   ;; Code
    (workspace 4 output DP-2)   ;; Agenda
    (workspace 5 output DP-1)   ;; Music/Video
    (workspace 6 output DP-1)   ;; Chat
    (workspace 7 output DP-1))) ;; Games

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
   (feature-host-info #:host-name "workhorse"
                      #:timezone  "Europe/Berlin"
                      #:locale "en_US.UTF-8")

   ;;; Kernel
   (feature-kernel #:kernel linux
                   #:initrd microcode-initrd
                   #:initrd-modules '("vmd")
                   #:firmware (list linux-firmware sof-firmware))

   ;;; File systems
   (feature-file-systems #:file-systems workhorse-filesystems)

   ;;; Packages
   (feature-base-packages #:system-packages
                          (append %base-system-packages)
                          #:home-packages
                          (append %base-home-packages))

   ;;; HiDPI
   (feature-hidpi)

   ;;; Backlight
   (feature-backlight)

   ;;; Sway
   (feature-sway #:xwayland? #t
                 #:extra-config
                 (append %base-sway-config
                         workhorse-sway-config))))

(define workhorse-config
  (rde-config
   (features
    (append
     %base-features
     workhorse-features))))
