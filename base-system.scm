(define-module (base-system)
  #:use-module (gnu)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 pretty-print)
  #:use-module (gnu services linux)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages shells)
  #:use-module (guix channels)
  #:use-module (guix utils)
  #:use-module (guix inferior)
  #:use-module (guix packages)
  #:use-module (guix transformations)
  #:use-module (flat packages emacs)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu packages nvidia))

(use-package-modules wm)
(use-service-modules desktop)

(define transform
  (options->transformation
   '((with-graft . "mesa=nvda"))))

;; Xorg settings to be passed to the session.
;; TODO: Clean up.
(define %xorg-monitor-config
  "Section \"ServerLayout\"
      Identifier     \"Layout0\"
      Screen      0  \"Screen0\" 0 0
      InputDevice    \"Keyboard0\" \"CoreKeyboard\"
      InputDevice    \"Mouse0\" \"CorePointer\"
      Option         \"Xinerama\" \"0\"
  EndSection
   
  Section \"Module\"
      Load           \"dbe\"
      Load           \"extmod\"
      Load           \"type1\"
      Load           \"freetype\"
      Load           \"glx\"
  EndSection

  Section \"InputDevice\"
      # generated from default
      Identifier     \"Mouse0\"
      Driver         \"mouse\"
      Option         \"Protocol\" \"auto\"
      Option         \"Device\" \"/dev/psaux\"
      Option         \"Emulate3Buttons\" \"no\"
      Option         \"ZAxisMapping\" \"4 5\"
  EndSection

  Section \"InputDevice\"
      # generated from default
      Identifier     \"Keyboard0\"
      Driver         \"kbd\"
  EndSection

  Section \"Monitor\"
      # HorizSync source: edid, VertRefresh source: edid
      Identifier     \"Monitor0\"
      VendorName     \"Unknown\"
      ModelName      \"Philips PHL 245E1\"
      HorizSync       30.0 - 114.0
      VertRefresh     48.0 - 75.0
      Option         \"DPMS\"
  EndSection

  Section \"Device\"
      Identifier     \"Device0\"
      Driver         \"nvidia\"
      VendorName     \"NVIDIA Corporation\"
      BoardName      \"GeForce GTX 1050 Ti\"
  EndSection

  Section \"Screen\"
      Identifier     \"Screen0\"
      Device         \"Device0\"
      Monitor        \"Monitor0\"
      DefaultDepth    24
      Option         \"Stereo\" \"0\"
      Option         \"nvidiaXineramaInfoOrder\" \"DFP-2\"
      Option         \"TripleBuffer\" \"true\"
      Option         \"TearFree\" \"true\"
      Option         \"MetaModes\" \"HDMI-0: nvidia-auto-select +2560+0 {ForceCompositionPipeline=On, ForceFullCompositionPipeline=On}, DP-0: nvidia-auto-select +0+0 {ForceCompositionPipeline=On, ForceFullCompositionPipeline=On}\"
      Option         \"SLI\" \"Off\"
      Option         \"MultiGPU\" \"Off\"
      Option         \"BaseMosaic\" \"off\"
      SubSection     \"Display\"
	  Depth       24
      EndSubSection
  EndSection")        

;; Override emacs-exwm package definition
;; To include emacs-native-comp and picom.
(define-public emacs-native-comp-exwm
  (package
   (inherit emacs-exwm)
   (name "emacs-native-comp-exwm")
   (synopsis "test")
   (inputs
    `(("picom" ,picom)
      ,@(package-inputs emacs-exwm)))
   (arguments
    `(,@(package-arguments emacs-exwm)
      #:emacs ,emacs-native-comp
      #:phases (modify-phases %standard-phases
			      (add-after 'build 'install-xsession
					 (lambda* (#:key inputs outputs #:allow-other-keys)
						  (let* ((out (assoc-ref outputs "out"))
							 (xsessions (string-append out "/share/xsessions"))
							 (bin (string-append out "/bin"))
							 (exwm-executable (string-append bin "/exwm")))

						    ;; Add a .desktop file to xsessions
						    (mkdir-p xsessions)
						    (mkdir-p bin)
						    (make-desktop-entry-file
						     (string-append xsessions "/exwm.desktop")
						     #:name ,name
						     #:comment ,synopsis
						     #:exec exwm-executable
						     #:try-exec exwm-executable)

						    ;; Add a shell wrapper to bin
						    (with-output-to-file exwm-executable
						      (lambda _
							(format #t "#!~a ~@
										       ~a +SI:localuser:$USER ~@
										       ~a &
										       exec ~a --exit-with-session ~a \"$@\" --eval '~s' ~%"
								(string-append (assoc-ref inputs "bash") "/bin/sh")
								(string-append (assoc-ref inputs "xhost") "/bin/xhost")
								(string-append (assoc-ref inputs "picom") "/bin/picom")
								(string-append (assoc-ref inputs "dbus") "/bin/dbus-launch")
								(string-append (assoc-ref inputs "emacs") "/bin/emacs")
								'(cond
								  ((file-exists-p "~/.exwm")
								   (load-file "~/.exwm"))
								  ((not (featurep 'exwm))
								   (require 'exwm)
								   (require 'exwm-config)
								   (exwm-config-default)
								   (message (concat "exwm configuration not found. "
										    "Falling back to default configuration...")))))))
						    (chmod exwm-executable #o555)
						    #t))))))))


(define-public base-operating-system
  (operating-system
   (kernel linux-lts)
   (initrd microcode-initrd)
   (firmware (list linux-firmware))

   ;; Kernel arguments
   (kernel-arguments (append 
		      '("quiet"
			"modprobe.blacklist=radeon,nouveau"
			"net.ifnames=0")
		      %default-kernel-arguments))
   
   ;; Loadable kernel modules
   (kernel-loadable-modules (list nvidia-driver))
   
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
   (services (cons* (simple-service 
		     'custom-udev-rules udev-service-type 
		     (list nvidia-driver))
	     	    (service kernel-module-loader-service-type
			     '("ipmi_devintf"
			       "nvidia"
			       "nvidia_modeset"
			       "nvidia_uvm"))

		    ;; Login manager
		    (service slim-service-type
			     (slim-configuration
			      (xorg-configuration (xorg-configuration
						   (keyboard-layout keyboard-layout)
						   (modules (cons* nvidia-driver %default-xorg-modules))
						   (server (transform xorg-server))
						   (drivers '("nvidia"))
						   (extra-config (list %xorg-monitor-config))))))

		    ;; zshell
		    (simple-service 'zshrc etc-service-type
				    `(("zprofile" ,(plain-file "zprofile" "\
							       emulate sh -c 'source /etc/profile'
							       export ZDOTDIR=\"$HOME/.config/zsh\""))))

		    ;; Remove GDM and add substitute server.
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
	   emacs-native-comp
	   emacs-native-comp-exwm
					;emacs-desktop-environment
	   nss-certs
	   nvidia-driver
	   zsh)
     %base-packages))

   (name-service-switch %mdns-host-lookup-nss)))
base-operating-system
