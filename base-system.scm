(define-module (base-system)
  #:use-module (gnu)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 pretty-print)
  #:use-module (gnu services linux)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu services sddm)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages wine)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages video)
  #:use-module (guix channels)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(define %guix-channels
  (scheme-file
   "channels.scm"
   #~(cons* (channel
             (name 'nonguix)
             (url "https://gitlab.com/nonguix/nonguix")
             (introduction
              (make-channel-introduction
               "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
               (openpgp-fingerprint
                "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
            (channel
             (name 'flat)
             (url "https://github.com/flatwhatson/guix-channel.git")
             (introduction
              (make-channel-introduction
               "33f86a4b48205c0dc19d7c036c85393f0766f806"
               (openpgp-fingerprint
		"736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
            %default-channels)))

(define %sddm-hidpi
  (plain-file
   "hidpi.conf"
   "[Wayland]
    EnableHiDPI=true"))

(define-public base-operating-system
  (operating-system
   (kernel linux-lts)
   (initrd microcode-initrd)
   (firmware (list amdgpu-firmware linux-firmware))

   ;; Kernel arguments
   (kernel-arguments (append 
		      '("quiet"
			"modprobe.blacklist=nouveau"
			"net.ifnames=0")
		      %default-kernel-arguments))
   
   ;; Machine settings
   (host-name "geekcave")
   (timezone "Europe/Berlin")
   (locale "en_US.utf8")
   (keyboard-layout (keyboard-layout "us" "altgr-intl"))
   
   ;; User account
   (users (cons (user-account
		 (name "db")
		 (group "users")
		 (home-directory "/home/db")
		 (shell (file-append zsh "/bin/zsh"))
		 (supplementary-groups '("wheel"
					 "audio"
					 "video"
					 "input"
					 "cdrom")))
		%base-user-accounts))

   ;; Services
   (services (cons* 
	      (extra-special-file "/etc/guix/channels.scm" %guix-channels)
	      (extra-special-file "/etc/sddm.conf.d/hidpi.conf" %sddm-hidpi)
	      (service sddm-service-type)
	      (remove (lambda (service)
			(eq? (service-kind service) gdm-service-type))
		      (modify-services %desktop-services
				       (guix-service-type
					config =>
					(guix-configuration
					 (inherit config)
					 (substitute-urls
					  (append (list "https://mirror.brielmaier.net")
						  %default-substitute-urls))
					 (authorized-keys
					  (append (list (local-file "/home/db/.config/guix/mirror.brielmaier.net.pub"))
						  %default-authorized-guix-keys))))))))


   ;; Bootloader		
   (bootloader (bootloader-configuration
		(bootloader grub-efi-bootloader)
		(target "/boot/efi")
		(timeout 3)))
   
   ;; File systems
   (file-systems (cons* (file-system ;; System partition
			 (device (file-system-label "GUIX"))
			 (mount-point "/")
			 (type "ext4"))
			(file-system ;; Boot partition
			 (device (file-system-label "BOOT"))
			 (mount-point "/boot/efi")
			 (type "vfat"))
			%base-file-systems))

   ;; Packages to install
   (packages
    (append
     (list git
	   curl
	   vim
	   nss-certs
	   sway
	   swaybg
	   swayidle
	   swaylock
	   mesa
	   dxvk
	   vkd3d
	   mesa-headers
	   spirv-cross
	   spirv-tools
	   mesa-utils
	   spirv-headers
	   zsh)
     %base-packages))

   (name-service-switch %mdns-host-lookup-nss)))
base-operating-system
