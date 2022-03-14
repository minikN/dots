(define-module (personal geekcave)
  #:use-module (gnu system file-systems)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features linux)
  #:use-module (rde features system)
  #:use-module (rde features fontutils)
  #:use-module (personal features games)
  #:export (geekcave-features
            geekcave-sway-config))

(define geekcave-sway-config
  `((output DP-2 pos 0 0)
    (output HDMI-A-1 pos 2560 0)
    (workspace 1 output DP-2)     ;; Browser
    (workspace 2 output HDMI-A-1) ;; Terminal
    (workspace 3 output HDMI-A-1) ;; Code
    (workspace 4 output HDMI-A-1) ;; Agenda
    (workspace 5 output DP-2)     ;; Music/Video
    (workspace 6 output DP-2)     ;; Chat
    (workspace 7 output DP-2)     ;; Games
    ))

(define geekcave-filesystems
  (list (file-system ;; System partition
         (device (file-system-label "GUIX"))
         (mount-point "/")
         (type "btrfs"))
        (file-system ;; Games partition
         (device (file-system-label "GAMES"))
         (mount-point "/home/db/games") ;; TODO: Fix mount point
         (type "btrfs"))
        (file-system ;; Boot partition
         (device (file-system-label "BOOT"))
         (mount-point "/boot/efi")
         (type "vfat"))))

(define geekcave-features
  (list
   ;;; Host info
   (feature-host-info #:host-name "geekcave"
                      #:timezone  "Europe/Berlin"
                      #:locale "en_US.utf8")

   ;;; Kernel
   (feature-kernel #:kernel linux-5.15
                   #:kernel-arguments (list "modprobe.blacklist=nouveau")
                   #:initrd microcode-initrd
                   #:firmware (list amdgpu-firmware linux-firmware))

   ;;; File systems
   (feature-file-systems #:file-systems geekcave-filesystems)

   ;;; HiDPI
   (feature-hidpi)

   ;;; Games
   (feature-games-base)
   (feature-games-steam #:sandbox-location "~/games/steam-sandbox")))

