(define-module (base-system)
  #:use-module (gnu)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 pretty-print)
  #:use-module (gnu services linux)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu packages nvidia))

(use-package-modules wm)
(use-service-modules networking)
(use-service-modules desktop)

(define-public base-operating-system
  (operating-system
   (kernel linux)
   (initrd microcode-initrd)
   (firmware (list linux-firmware))

   ;; Add the 'net.ifnames' argument to prevent network interfaces
   ;; from having really long names.  This can cause an issue with
   ;; wpa_supplicant when you try to connect to a wifi network.
   (kernel-arguments '("quiet"
		       "modprobe.blacklist=radeon,nouveau"
		       "net.ifnames=0"))
   
   ;; Loadable kernel modules
   (kernel-loadable-modules (list nvidia-driver))
   
   ;; Services
   (services (cons* (simple-service 
		     'custom-udev-rules udev-service-type 
		     (list nvidia-driver))
	     	    (service kernel-module-loader-service-type
			     '("ipmi_devintf"
			       "nvidia"
			       "nvidia-modeset"
			       "nvidia-uvm"))
		    (service dhcp-client-service-type)
		    ;(remove (lambda (service)
			;      (eq? (service-kind service) gdm-service-type))
                   %desktop-services)))
   
   (host-name "geekcave")
   (timezone "Europe/Berlin")
   (locale "en_US.utf8")
   (keyboard-layout (keyboard-layout "us" "altgr-intl"))
   		
   (users (cons (user-account
		 (name "db")
		 (group "users")
		 (home-directory "/home/db")
		 (supplementary-groups '("wheel"
					 "audio"
					 "video"
					 "input"
					 "cdrom")))
		%base-user-accounts))
		
   (bootloader (bootloader-configuration
		(bootloader grub-efi-bootloader)
		(target "/boot/efi")
		(timeout 3)))
		
   (file-systems (cons* (file-system ;; System partition
			 (device (file-system-label "GUIX"))
			 (mount-point "/")
			 (type "ext4"))
			(file-system ;; Boot partition
			 (device (file-system-label "BOOT"))
			 (mount-point "/boot/efi")
			 (type "vfat"))
		       %base-file-systems))

   ;; Add some extra packages useful for the installation process
   (packages
    (append
     (list (specification->package "git")
	   (specification->package "wget")
	   (specification->package "vim")
	   (specification->package "emacs")
	   (specification->package "emacs-exwm")
	   (specification->package "emacs-desktop-environment")
	   (specification->package "nss-certs")
	   (specification->package "nvidia-driver"))))
		     
   (name-service-switch %mdns-host-lookup-nss)))
base-operating-system
