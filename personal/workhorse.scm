(define-module (personal workhorse)
  #:use-module (gnu packages)
  #:use-module (gnu system file-systems)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features linux)
  #:use-module (rde features system)
  #:use-module (rde features fontutils)
  #:export (workhorse-features
            workhorse-sway-config))

(define workhorse-sway-config
  `((output HDMI-A-1 pos 0 0)
    (output eDP-1 pos 2560 0)
    (workspace 1 output HDMI-A-1)     ;; Browser
    (workspace 2 output eDP-1) ;; Terminal
    (workspace 3 output eDP-1) ;; Code
    (workspace 4 output eDP-1) ;; Agenda
    (workspace 5 output HDMI-A-1) ;; Music/Video
    (workspace 6 output HDMI-A-1) ;; Chat
    (workspace 7 output HDMI-A-1) ;; Games
    ))

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

   ;;; HiDPI
   (feature-hidpi)

   ;;; Backlight
   (feature-backlight)
   ))



