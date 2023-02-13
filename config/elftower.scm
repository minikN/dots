(define-module (config elftower)
  #:use-module (config base)
  #:use-module (config packages)

  #:use-module (gnu packages)
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

(define left 'DP-1)
(define right 'HDMI-A-1)
(define primary 'eDP-1)

(define elftower-sway-config
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
    (output ,primary scale 1.5)))

(define elftower-filesystems
  (list (file-system ;; System partition
         (device (file-system-label "GUIX"))
         (mount-point "/")
         (type "btrfs"))
        (file-system ;; Boot partition
         (device (file-system-label "BOOT"))
         (mount-point "/boot/efi")
         (type "vfat"))))

(define (common-modules bar)
  (list
   (waybar-cpu #:bar-id bar)
   (waybar-memory #:bar-id bar)
   (waybar-disk #:bar-id bar)
   (waybar-temperature #:bar-id bar)
   (waybar-volume
    #:show-percentage? #t
    #:scroll-step 5
    #:bar-id bar)
   (waybar-battery #:bar-id bar)
   (waybar-tray #:bar-id bar)
   (waybar-clock
    #:format "{:%H:%M}"
    #:bar-id bar)))

(define left-bar-modules
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
   (waybar-sway-window
    #:bar-id 'left)))

(define right-bar-modules
  (append (list
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
           (waybar-sway-window
            #:bar-id 'right))
          (common-modules 'right)))

(define primary-bar-modules
  (append (list
           (waybar-sway-workspaces
            #:all-outputs? #t
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
           (waybar-sway-window))
          (common-modules 'main)))

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

   ;;; HiDPI
   (feature-hidpi)

   ;;; Backlight
   (feature-backlight)

   ;;; Sway
   (feature-sway
    #:xwayland? #t
    #:extra-config
    (append %base-sway-config
            elftower-sway-config))
   (feature-sway-run-on-tty #:sway-tty-number 2)
   (feature-sway-screenshot)


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
     left-bar-modules
     right-bar-modules
     primary-bar-modules))))

(define elftower-config
  (rde-config
   (features
    (append
     %base-features
     elftower-features))))
