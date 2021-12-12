(define-module (config)
  #:use-module (flat packages emacs)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system mapped-devices)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (rde features base)
  #:use-module (rde features bittorrent)
  #:use-module (rde features docker)
  #:use-module (rde features emacs)
  #:use-module (rde features fontutils)
  #:use-module (rde features gnupg)
  #:use-module (rde features keyboard)
  #:use-module (rde features linux)
  #:use-module (rde features mail)
  #:use-module (rde features password-utils)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features ssh)
  #:use-module (rde features system)
  #:use-module (rde features terminals)
  #:use-module (rde features tmux)
  #:use-module (rde features version-control)
  #:use-module (rde features wm)
  #:use-module (rde features xdg)
  #:use-module (rde features)
  #:use-module (rde packages)
  #:use-module (features applauncher)
  #:use-module (features games)
  #:use-module (features linux)
  #:use-module (features emacs-xyz))

(define* (pkgs #:rest lst)
  (map specification->package+output lst))

(define %brielmaier-public-key
  (plain-file
   "mirror.brielmaier.net.pub"
   "(public-key
   (ecc
(curve Ed25519)
(q #7514F8D729DB1935470A581CE3851ED9FD6F1F9BAFE1D8BEC77A931ADB7A4337#)
))"))

(define* (mail-lst id fqda urls)
  "Make a simple mailing-list."
  (mailing-list
   (id   id)
   (fqda fqda)
   (config (l2md-repo
            (name (symbol->string id))
            (urls urls)))))

(define mailbox-folder-mapping
  '(("inbox"   . "INBOX")
    ("sent"    . "Sent")
    ("drafts"  . "Drafts")
    ("trash"   . "Trash")
    ("junk"    . "Junk")
    ("archive" . "Archiv")))

(define mailbox-isync-settings
  (generate-isync-serializer "imap.mailbox.org" mailbox-folder-mapping))

(define %db-features
  (list
   (feature-user-info #:user-name "db"
                      #:full-name "Demis Balbach"
                      #:email "db@minikn.xyz"
                      #:user-groups (list "wheel"
                                          "audio"
                                          "video"
                                          "input"
                                          "cdrom"
                                          "disk"
                                          "lp"))
   (feature-mail-settings #:mail-accounts
                          (list (mail-account
                                 (id 'personal)
                                 (fqda "db@minikn.xyz")
                                 (type 'mailbox)
                                 (pass-cmd "pass show Mail/Mailbox")))
                          #:mailing-lists
                          (list (mail-lst 'guix-devel "guix-devel@gnu.org"
                                    '("https://yhetil.org/guix-devel/0"))))
   (feature-ssh)
   (feature-gnupg #:gpg-primary-key "F17DDB98CC3C405C"
                  #:gpg-ssh-agent? #t
                  ;#:pinentry-flavor 'emacs
                  #:ssh-keys '(("E3FFA5A1B444A4F099E594758008C1D8845EC7C0")))
   (feature-password-store #:remote-password-store-url "git@gitlab.com:minikN/pass.git")
   (feature-keyboard #:keyboard-layout
                     (keyboard-layout "us" "altgr-intl" #:options '("ctrl:nocaps")))))

(define %main-features
  (list

   ;;;
   ;;; Services
   ;;;
   (feature-base-services #:guix-substitute-urls (list "https://mirror.brielmaier.net")
                          #:guix-authorized-keys (list %brielmaier-public-key))
   (feature-desktop-services)

   ;;;
   ;;; Bluetooth
   ;;;
   (feature-bluetooth)

   ;;;
   ;;; Fonts
   ;;;
   (feature-fonts #:font-monospace (font "Iosevka" #:size 15 #:weight 'semi-light))

   ;;;
   ;;; Terminal, shell
   (feature-alacritty)
   (feature-zsh)

   ;;;
   ;;; Git
   ;;;
   (feature-git #:sign-commits? #t
                #:git-gpg-sign-key "F17DDB98CC3C405C"
                #:git-send-email? #t)

   ;;;
   ;;; Emacs
   ;;;
   (feature-emacs)
   (feature-emacs-leader-keys)
   (feature-emacs-appearance)
   (feature-emacs-completion #:mini-frame? #t)
   (feature-emacs-evil)
   (feature-emacs-erc #:erc-server "minikn.xyz"
                      #:erc-port 6698
                      #:erc-nick "db")
   (feature-emacs-faces)
   (feature-emacs-files)
   (feature-emacs-git)
   (feature-emacs-message #:message-signature
                          "Best regards / Mit freundlichen Grüßen,\nDemis Balbach")
   (feature-emacs-project)
   (feature-emacs-syntax)
   (feature-emacs-which-key)

   ;;;
   ;;; Mail
   ;;;
   (feature-notmuch)
   (feature-l2md)
   (feature-msmtp #:msmtp-provider-settings
                  `((mailbox . ((host . "smtp.mailbox.org")
                                (port . 587)))))
   (feature-isync #:isync-verbose #t
                  #:isync-serializers
                  `((mailbox . ,mailbox-isync-settings)))

   ;;;
   ;;; WM
   ;;;
   (feature-bemenu #:default-app-launcher? #t)
   (feature-sway #:xwayland? #t
                 #:extra-config
                 `((include ,(local-file "./config/sway/config"))))
   (feature-sway-run-on-tty #:sway-tty-number 2)
   (feature-sway-screenshot)
   (feature-sway-statusbar)
   (feature-pipewire)
   (feature-xdg #:xdg-user-directories-configuration
                (home-xdg-user-directories-configuration
                 (music "$HOME/music")
                 (videos "$HOME/vids")
                 (pictures "$HOME/pics")
                 (documents "$HOME/docs")
                 (download "$HOME/dl")
                 (desktop "$HOME")
                 (publicshare "$HOME")
                 (templates "$HOME")))

   ;;;
   ;;; Packages
   ;;;
   (feature-base-packages #:system-packages
                          (append
                           (pkgs
                            "adwaita-icon-theme"
                            "curl"
                            "git"
                            "gst-libav"
                            "gst-plugins-bad"
                            "gst-plugins-good"
                            "gst-plugins-base"
                            "gst-plugins-ugly"
                            "htop"
                            "vim"
                            "jack2"
                            "make"
                            "mesa"
                            "mesa-headers"
                            "mesa-utils"
                            "mesa-opencl"
                            "mesa-opencl-icd"
                            "nyxt"
                            "ungoogled-chromium-wayland"
                            "ublock-origin-chromium"
                            "pavucontrol"))
                          #:home-packages
                          (append
                           (pkgs
                            "streamlink"
                            "carla"
                            "youtube-dl"
                            "mpv")))
   (feature-games-base)
   (feature-games-steam)))
(define %geekcave-filesystems
  (list (file-system ;; System partition
         (device (file-system-label "GUIX"))
         (mount-point "/")
         (type "btrfs"))
        (file-system ;; Boot partition
         (device (file-system-label "BOOT"))
         (mount-point "/boot/efi")
         (type "vfat"))))

(define %geekcave-features
  (list
   (feature-host-info #:host-name "geekcave"
                      #:timezone  "Europe/Berlin"
                      #:locale "en_US.utf8")
   (feature-kernel #:kernel linux-5.14
                   #:kernel-arguments (list "modprobe.blacklist=nouveau")
                   #:initrd microcode-initrd
                   #:firmware (list amdgpu-firmware linux-firmware))
   (feature-file-systems #:file-systems %geekcave-filesystems)
   (feature-hidpi)))

(define-public geekcave-config
  (rde-config
   (features
    (append
     %db-features
     %main-features
     %geekcave-features))))

(define geekcave-os
  (rde-config-operating-system geekcave-config))

(define geekcave-he
  (rde-config-home-environment geekcave-config))

(define (dispatcher)
  (let ((target (getenv "TARGET")))
    (match target
	   ("geekcave-he" geekcave-he)
	   ("geekcave-os" geekcave-os))))
(dispatcher)
