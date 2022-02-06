(define-module (config)
  #:use-module (gnu packages)
  #:use-module (gnu services nix)
  #:use-module (gnu services)
  #:use-module (gnu system keyboard)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (personal features emacs-xyz)
  #:use-module (personal features linux)
  #:use-module (personal geekcave)
  #:use-module (rde features base)
  #:use-module (rde features emacs)
  #:use-module (rde features fontutils)
  #:use-module (rde features gnupg)
  #:use-module (rde features keyboard)
  #:use-module (rde features linux)
  #:use-module (rde features mail)
  #:use-module (rde features password-utils)
  #:use-module (rde features shells)
  #:use-module (rde features ssh)
  #:use-module (rde features terminals)
  #:use-module (rde features version-control)
  #:use-module (rde features wm)
  #:use-module (rde features xdg)
  #:use-module (rde features))

(define* (pkgs #:rest lst)
  (map specification->package+output lst))

(define %nonguix-public-key
  (plain-file
   "nonguix-signing-key.pub"
   "(public-key
   (ecc
(curve Ed25519)
(q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)
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

;;;
;;; Common features
;;; These include all features I need to be present
;;; despite the machine we're dealing with.
;;;
(define %main-features
  (list
   ;;; User info
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

   ;;; Mail
   (feature-mail-settings #:mail-accounts
                          (list (mail-account
                                 (id 'personal)
                                 (fqda "db@minikn.xyz")
                                 (type 'mailbox)
                                 (pass-cmd "pass show Mail/mailbox.org/db@minikn.xyz")))
                          #:mailing-lists
                          (list (mail-lst 'guix-devel "guix-devel@gnu.org"
                                    '("https://yhetil.org/guix-devel/0"))))

   ;;; GnuPG
   (feature-gnupg #:gpg-primary-key "F17DDB98CC3C405C"
                  #:gpg-ssh-agent? #t
                  #:ssh-keys '(("E3FFA5A1B444A4F099E594758008C1D8845EC7C0")))

   ;;; SSH
   (feature-ssh)

   ;;; Git
   (feature-git #:sign-commits? #t
                #:git-gpg-sign-key "F17DDB98CC3C405C"
                #:git-send-email? #t)

   ;;; Passwords
   (feature-password-store #:remote-password-store-url "git@gitlab.com:minikN/pass.git")

   ;;; Keyboard layout
   (feature-keyboard #:keyboard-layout
                     (keyboard-layout "us" "altgr-intl" #:options '("ctrl:nocaps")))

   ;;; Services
   (feature-custom-services #:system-services (list (service nix-service-type)))
   (feature-base-services #:guix-substitute-urls (list "https://substitutes.nonguix.org")
                          #:guix-authorized-keys (list %nonguix-public-key))
   (feature-desktop-services)

   ;;; Bluetooth
   (feature-bluetooth)

   ;;; Fonts
   (feature-fonts #:font-monospace (font "Iosevka" #:size 15 #:weight 'semi-light))

   ;;; Terminal, shell
   (feature-zsh)
   (feature-alacritty #:config-file (local-file "./config/alacritty/alacritty.yml")
                      #:default-terminal? #t)

   ;;; Emacs
   (feature-emacs)
   (feature-emacs-leader-keys)
   (feature-emacs-appearance)
   (feature-emacs-completion #:mini-frame? #f)
   (feature-emacs-vertico)
   (feature-emacs-evil)
   (feature-emacs-erc #:erc-server "minikn.xyz"
                      #:erc-port 6698
                      #:erc-nick "db")
   (feature-emacs-dired)
   (feature-emacs-faces)
   (feature-emacs-files)
   (feature-emacs-git)
   (feature-emacs-message #:message-signature
                          "Best regards / Mit freundlichen Grüßen,\nDemis Balbach")
   (feature-emacs-project)
   (feature-emacs-syntax)
   (feature-emacs-which-key)

   ;;; Mail
   (feature-notmuch)
   (feature-l2md)
   (feature-msmtp #:msmtp-provider-settings
                  `((mailbox . ((host . "smtp.mailbox.org")
                                (port . 587)))))
   (feature-isync #:isync-verbose #t
                  #:isync-serializers
                  `((mailbox . ,mailbox-isync-settings)))

   ;;; WM
   (feature-sway #:xwayland? #t
                 #:extra-config
                 `((default_border none)
                   (output DP-2 pos 0 0)
                   (output HDMI-A-1 pos 2560 0)
                   (workspace 1 output DP-2)     ;; Browser
                   (workspace 2 output HDMI-A-1) ;; Terminal
                   (workspace 3 output HDMI-A-1) ;; Code
                   (workspace 4 output HDMI-A-1) ;; Agenda
                   (workspace 5 output DP-2)     ;; Music/Video
                   (workspace 6 output DP-2)     ;; Chat
                   (workspace 7 output DP-2)     ;; Games
                   (assign "[app_id=\"Chromium-browser\"]" workspace 1) ;; TODO: Move
                   (for_window "[app_id=\"pavucontrol\"]" floating enable, border pixel) ;; TODO: Move
                   (for_window "[app_id=\"pinentry-qt\"]" floating enable, border pixel) ;; TODO: Move
                   (bindsym $mod+grave exec $term) ;; TODO: Move
                   (bindsym $mod+Shift+q kill) ;; TODO: Move
                   ))
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

   ;;; Packages
   (feature-base-packages #:system-packages
                          (append
                           (pkgs
                            "adwaita-icon-theme"
                            "hicolor-icon-theme"
                            "curl"
                            "git"
                            "gst-libav"
                            "gst-plugins-bad"
                            "gst-plugins-good"
                            "gst-plugins-base"
                            "gst-plugins-ugly"
                            "htop"
                            "vim"
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
                            "calf"
                            "jack2"
                            ;; "nautilus"
                            "guitarix"
                            "guitarix-lv2"
                            "carla"
                            "qjackctl"
                            "youtube-dl"
                            "mpv")))))

(define-public geekcave-config
  (rde-config
   (features
    (append
     %main-features
     geekcave-features))))

(define geekcave-os
  (rde-config-operating-system geekcave-config))

(define geekcave-he
  (rde-config-home-environment geekcave-config))

(define (dispatcher)
  (let ((target (getenv "TARGET")))
    (match target
	   ("geekcave-he" geekcave-he)
	   ("geekcave-os" geekcave-os))))

;;; Enable this to print the entire config object
;;; to see what's enable for example
;(pretty-print-rde-config geekcave-config)

(dispatcher)
