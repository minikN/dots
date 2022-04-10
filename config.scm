(define-module (config)
  #:use-module (gnu home-services shells)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu services nix)
  #:use-module (gnu services)
  #:use-module (gnu system keyboard)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (personal features emacs-xyz)
  #:use-module (personal features linux)
  #:use-module (personal features wm)
  #:use-module (personal geekcave)
  #:use-module (personal workhorse)
  #:use-module (rde features base)
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
  #:use-module (rde features version-control)
  #:use-module (rde features wm)
  #:use-module (rde features xdg)
  #:use-module (rde features xdisorg)
  #:use-module (rde features))

(define* (pkgs lst)
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

   ;;; Bluetooth
   (feature-bluetooth)

   ;;; Terminal, shell
   (feature-zsh)
   (feature-vterm)
   (feature-alacritty #:config-file (local-file "./config/alacritty/alacritty.yml")
                      #:default-terminal? #f)

   ;;; Emacs
   (feature-emacs)
   (feature-emacs-appearance)
   (feature-emacs-completion #:mini-frame? #f)
   (feature-emacs-vertico)
   (feature-emacs-evil)
   (feature-emacs-erc #:erc-server "minikn.xyz"
                      #:erc-port 6697
                      #:erc-nick "minikN"
                      #:extra-config `((setq erc-email-userid "minikN")))
   (feature-emacs-dired)
   (feature-emacs-faces)
   (feature-emacs-files)
   (feature-emacs-git)
   (feature-emacs-message #:message-signature
                          "Best regards / Mit freundlichen Grüßen,\nDemis Balbach")
   (feature-emacs-project)
   (feature-emacs-perspective)
   (feature-emacs-syntax)
   (feature-emacs-which-key #:min-height 5)
   (feature-emacs-lang-base)
   (feature-emacs-lang-javascript)

   ;; direnv
   (feature-direnv)

   ;;; Mail
   (feature-notmuch)
   (feature-l2md)
   (feature-msmtp #:msmtp-provider-settings
                  `((mailbox . ((host . "smtp.mailbox.org")
                                (port . 587)))))
   (feature-isync #:isync-verbose #t
                  #:isync-serializers
                  `((mailbox . ,mailbox-isync-settings)))
   ;;; Fonts
   (feature-fonts #:font-monospace (font "Iosevka" #:size 15 #:weight 'semi-light))

   ;;; Services
   (feature-custom-services #:system-services (list (service nix-service-type)) ;; TODO: Move to own feature
                            #:home-services (list (simple-service               ;; TODO: Move to own feature
                                                   'setup-nix-on-login
                                                   home-shell-profile-service-type
                                                   (list "source /run/current-system/profile/etc/profile.d/nix.sh"))))
   (feature-base-services #:guix-substitute-urls (list "https://substitutes.nonguix.org")
                          #:guix-authorized-keys (list %nonguix-public-key))
   (feature-desktop-services)

   (feature-pipewire)
   (feature-rofi)
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
   (feature-bootloader)))

(define base-sway-config
  `((default_border none)
    (assign "[app_id=\"Chromium-browser\"]" workspace 1) ;; TODO: Move
    (for_window "[app_id=\"pavucontrol\"]" floating enable, border pixel) ;; TODO: Move
    (for_window "[app_id=\"pinentry-qt\"]" floating enable, border pixel) ;; TODO: Move
    (bindsym $mod+grave exec $term) ;; TODO: Move
    (bindsym $mod+Shift+q kill) ;; TODO: Move
    (bindsym $mod+Shift+Ctrl+r mode "resize")
    (mode "resize" ((bindsym Left resize shrink width 10px)
                    (bindsym Down resize grow height 10px)
                    (bindsym Up resize shrink height 10px)
                    (bindsym Right resize grow width 10px)
                    (bindsym Escape mode "default")))
    (exec nm-applet --indicator) ;; NetworkManager
    ))

(define %base-home-packages
  (list
   "curl"
   "git"
   "htop"
   "vim"
   "make"
   "ungoogled-chromium-wayland"
   "ublock-origin-chromium"
   "pavucontrol"
   ;; "streamlink"
   ;; "calf"
   ;; "jack2"
   ;; "guitarix"
   ;; "guitarix-lv2"
   ;; "carla"
   ;; "qjackctl"
   ;; "youtube-dl"
   ;; "mpv"
   ))

(define %base-system-packages
  (list
   "adwaita-icon-theme"
   "hicolor-icon-theme"
   "gst-libav"
   "gst-plugins-bad"
   "gst-plugins-good"
   "gst-plugins-base"
   "gst-plugins-ugly"))

(define-public geekcave-config
  (rde-config
   (features
    (append
     (list
      (feature-base-packages
       #:system-packages
       (append (pkgs %base-system-packages))
       #:home-packages
       (append (pkgs %base-home-packages)))
      (feature-sway
       #:xwayland? #t
       #:extra-config
       (append base-sway-config
               geekcave-sway-config))
      (feature-sway-run-on-tty #:sway-tty-number 2)
      (feature-sway-screenshot)
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
                        (waybar-clock #:format "{:%H:%M}"))))
     %main-features
     geekcave-features))))

(define-public workhorse-config
  (rde-config
   (features
    (append
     (list
      (feature-base-packages
       #:home-packages (list glibc-locales))
      (feature-custom-services
       #:home-services workhorse-services
       )
      (feature-sway
       #:xwayland? #t
       #:extra-config
       (append base-sway-config
               workhorse-sway-config))
      (feature-sway-screenshot)
      (feature-sway-desktop-file)
      (feature-waybar #:waybar-modules
                      (list
                        (waybar-module-workspaces)
                        (waybar-module-window)
                        (waybar-module-disk)
                        (waybar-module-cpu)
                        (waybar-module-memory)
                        (waybar-module-temperature)
                        (waybar-tray)
                        (waybar-module-audio)
                        (waybar-battery)
                        (waybar-clock #:format "{:%H:%M}"))))
     workhorse-features))))

(define geekcave-os
  (rde-config-operating-system geekcave-config))

(define geekcave-he
  (rde-config-home-environment geekcave-config))

(define workhorse-os
  (rde-config-operating-system workhorse-config))

(define workhorse-he
  (rde-config-home-environment workhorse-config))

(define (dispatcher)
  (let ((target (getenv "TARGET")))
    (match target
	   ("workhorse-he" workhorse-he)
	   ("workhorse-os" workhorse-os)
	   ("geekcave-he" geekcave-he)
	   ("geekcave-os" geekcave-os))))

;;; Enable this to print the entire config object
;;; to see what's enable for example
;(pretty-print-rde-config geekcave-config)

(dispatcher)
