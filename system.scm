(define-module (nongnu system install)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages package-management)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu packages nvidia)
  #:export (installation-os-nonfree))	

(use-modules (gnu)
	     (srfi srfi-1)
	     (ice-9 pretty-print)
	     (guix channels)
	     (guix inferior)
	     (gnu services desktop))
(use-package-modules wm)
(use-service-modules networking)

(define installation-os-nonfree
  (operating-system
   (inherit installation-os)
   (kernel
    (let*
      ((channels
        (list (channel
               (name 'nonguix)
               (url "https://gitlab.com/nonguix/nonguix")
               (commit "ff6ca98099c7c90e64256236a49ab21fa96fe11e"))
              (channel
               (name 'guix)
               (url "https://git.savannah.gnu.org/git/guix.git")
               (commit "3be96aa9d93ea760e2d965cb3ef03540f01a0a22"))))
       (inferior
        (inferior-for-channels channels)))
      (first (lookup-inferior-packages inferior "linux" "5.4.123"))))
   (initrd microcode-initrd)
   (firmware (list linux-firmware))

   ;; Add the 'net.ifnames' argument to prevent network interfaces
   ;; from having really long names.  This can cause an issue with
   ;; wpa_supplicant when you try to connect to a wifi network.
   (kernel-arguments '("quiet"
		       "modprobe.blacklist=radeon"
		       "modprobe.blacklist=nouveau"
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
                   %base-services))
   
   (host-name "geekcave")
   (timezone "Europe/Berlin")
   (locale "en_US.utf8")
   (keyboard-layout
   		(keyboard-layout "us" "altgr-intl"))
   		
   (users (cons (user-account
		 (name "db")
		 (group "users")
		 (home-directory "/home/db")
		 (supplementary-groups '("wheel"
					 "audio"
					 "video"
					 "cdrom")))
		%base-user-accounts))
		
   (bootloader (bootloader-configuration
		(bootloader grub-efi-bootloader)
		(target "/boot/efi")
		(timeout 3)))
		
   (file-systems (cons* (file-system ;; System partition
			 (device (file-system-label "GUIX"))
			 (mount-point "/")
			 (type "ext3"))
			(file-system ;; Boot partition
			 (device (file-system-label "BOOT"))
			 (mount-point "/boot/efi")
			 (type "vfat"))
		       %base-file-systems))

   ;; Add some extra packages useful for the installation process
   (packages
    (append (list git
		  curl
		  vim
		  emacs-no-x-toolkit
		  sway
		  nss-certs
		  nvidia-driver)
	    (operating-system-packages installation-os)))
	    
   (services (remove
   		(lambda (service)
		       (eq? (service-kind service) gdm-service-type))
		     %desktop-services))
		     
   (services (append
   		(list (service dhcp-client-service-type))
   		%base-services))
   (name-service-switch %mdns-host-lookup-nss)))
installation-os-nonfree
