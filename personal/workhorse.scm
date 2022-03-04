(define-module (personal workhorse)
  #:use-module (gnu packages)
  #:use-module (gnu system file-systems)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features linux)
  #:use-module (rde features system)
  #:export (workhorse-features))

(define* (pkgs #:rest lst)
  (map specification->package+output lst))

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

   ;; Packages
   ;; (feature-base-packages #:system-packages
   ;;                        (append (pkgs "xf86-video-nouveau"
   ;;                                      "wpa-supplicant")))
   
   ;;; Kernel
   (feature-kernel #:kernel linux-5.15
                   #:initrd microcode-initrd
                   #:initrd-modules '("vmd")
                   #:firmware (list linux-firmware))

   ;;; File systems
   (feature-file-systems #:file-systems workhorse-filesystems)

   ;;; HiDPI
   (feature-hidpi)))


