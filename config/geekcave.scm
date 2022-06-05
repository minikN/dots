(define-module (config geekcave)
  #:use-module (config base)
  #:use-module (config features package-management)
  #:use-module (gnu system file-systems)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features linux)
  #:use-module (rde features system)
  #:use-module (rde features fontutils)
  #:use-module (rde features wm)
  #:use-module (config features games)
  #:use-module (config features wm)
  
  #:export (geekcave-config))

(define geekcave-sway-config
  `((output DP-1 pos 0 0)
    (output DP-2 pos 2560 0)
    (workspace 1 output DP-1)   ;; Browser
    (workspace 2 output DP-2)   ;; Terminal
    (workspace 3 output DP-2)   ;; Code
    (workspace 4 output DP-2)   ;; Agenda
    (workspace 5 output DP-1)   ;; Music/Video
    (workspace 6 output DP-1)   ;; Chat
    (workspace 7 output DP-1))) ;; Games

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

(define geekcave-packages
  (list "cura"))

(define geekcave-features
  (list
   ;;; Host info
   (feature-host-info #:host-name "geekcave"
                      #:timezone  "Europe/Berlin"
                      #:locale "en_US.utf8")

   ;;; Kernel
   (feature-kernel #:kernel linux-lts
                   #:kernel-arguments (list "modprobe.blacklist=nouveau")
                   #:initrd microcode-initrd
                   #:firmware (list amdgpu-firmware linux-firmware))

   ;;; File systems
   (feature-file-systems #:file-systems geekcave-filesystems)

   ;;; Packages
   (feature-base-packages #:system-packages
                          (append (pkgs %base-system-packages))
                          #:home-packages
                          (append (pkgs %base-home-packages)
                                  (pkgs geekcave-packages)))
   ;;; HiDPI
   (feature-hidpi)

   ;; Sway
   (feature-sway #:xwayland? #t
                 #:extra-config
                 (append %base-sway-config
                         geekcave-sway-config))
   (feature-sway-run-on-tty #:sway-tty-number 2)
   (feature-sway-screenshot)

   ;; Nix
   ;;; TODO: Move this to base-features
   (feature-nix)

   ;; Waybar
   (feature-waybar #:waybar-modules
                   (list
                    (waybar-module-workspaces)
                    (waybar-module-window)
                    (waybar-module-disk)
                    (waybar-module-disk-games)
                    (waybar-module-cpu)
                    (waybar-module-memory)
                    (waybar-module-temperature)
                    (waybar-tray)
                    (waybar-module-audio)
                    (waybar-clock #:format "{:%H:%M}")))

   ;;; Games
   (feature-games-base)
   (feature-games-steam #:sandbox-location "~/games/steam-sandbox")))

(define geekcave-config
  (rde-config
   (features
    (append
     %base-features
     geekcave-features))))

