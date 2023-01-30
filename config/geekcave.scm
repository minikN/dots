(define-module (config geekcave)
  #:use-module (config base)
  #:use-module (config features games)
  #:use-module (config features package-management)
  #:use-module (config packages)
  ;; #:use-module (config features engineering)

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
    (list ssh-extra-config-service))

   ;;; HiDPI
   (feature-hidpi)

   ;; Sway
   (feature-sway
    #:xwayland? #t
    #:extra-config
    (append %base-sway-config
            geekcave-sway-config))
   (feature-sway-run-on-tty
    #:sway-tty-number 2)
   (feature-sway-screenshot)

   (feature-steam
    #:steamos? #t)

   ;; nix-env -iA nixpkgs.docker-compose
   (feature-docker)

   ;; Nix
   ;;; TODO: Move this to base-features
   (feature-nix)

   ;; Waybar
   (feature-waybar
    #:output 'DP-1
    #:height 30
    #:extra-config
    '(((position . top)
       (layer . top)
       (height . 30)
       (name . right)
       (output . DP-2)))
    #:waybar-modules
    (list
     (waybar-sway-workspaces
      #:format-icons
      '(("1" . " WWW")
        ("5" . " MUSIC")
        ("6" . " CHAT")
        ("7" . " GAMES")
        ("urgent" . )
        ("focused" . )
        ("default" . ))
      #:persistent-workspaces
      '(("1" . #())
        ("5" . #())
        ("6" . #())
        ("7" . #())))
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
     (waybar-sway-window)
     (waybar-sway-window #:bar-id 'right)
     (waybar-cpu #:bar-id 'right)
     (waybar-memory #:bar-id 'right)
     (waybar-disk #:bar-id 'right)
     (waybar-temperature #:bar-id 'right)
     (waybar-volume
      #:bar-id 'right
      #:show-percentage? #t
      #:scroll-step 5)
     (waybar-tray #:bar-id 'right)
     (waybar-clock
      #:bar-id 'right
      #:format "{:%H:%M}")))

   (feature-kanshi
    #:extra-config
    `((profile desktop ((output DP-1 enable)
                        (output DP-2 enable)
                        (output HDMI-A-2 disable)))))))

(define geekcave-config
  (rde-config
   (features
    (append
     %base-features
     geekcave-features))))
