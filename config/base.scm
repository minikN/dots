(define-module (config base)
  #:use-module (gnu home-services shells)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cups)
  #:use-module (gnu services)
  #:use-module (gnu services cups)
  #:use-module (gnu services nix)
  #:use-module (gnu system keyboard)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (config features emacs-xyz)
  #:use-module (config features javascript)
  #:use-module (config features linux)
  #:use-module (config packages)
  #:use-module (config packages node-xyz)
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
  #:use-module (rde features xdg)
  #:use-module (rde features xdisorg)
  #:use-module (rde features)

  #:export (pkgs
            %base-system-packages
            %base-home-packages
            %base-features
            %base-sway-config))

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
(define %base-features
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
   (feature-alacritty #:config-file (local-file "./applications/alacritty/alacritty.yml")
                      #:default-terminal? #f)

   ;;; Emacs
   (feature-emacs)
   (feature-emacs-appearance)
   (feature-emacs-completion #:mini-frame? #f)
   (feature-emacs-vertico)
   (feature-emacs-evil)
   (feature-emacs-erc #:erc-server "minikn.xyz"
                      #:erc-port 6697
                      #:erc-nick "db")
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

   ;; WIP
   (feature-emacs-corfu #:emacs-corfu-doc emacs-corfu-doc)
   (feature-emacs-eglot)
   (feature-javascript #:typescript node-typescript-4.7.3
                       #:typescript-language-server node-typescript-language-server-0.11.1
                       #:eslint node-eslint-8.17.0)
   
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
   (feature-base-services #:guix-substitute-urls (list "https://bordeaux.guix.gnu.org"
                                                       "https://substitutes.nonguix.org")
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

(define %base-sway-config
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
   "firefox"
   "ungoogled-chromium-wayland"
   "ublock-origin-chromium"
   "pavucontrol"
   "gimp"
   ;; "calf"
   ;; "jack2"
   ;; "guitarix"
   ;; "guitarix-lv2"
   ;; "carla"
   ;; "qjackctl"
   ;; "youtube-dl"
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
