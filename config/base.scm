(define-module (config base)
  #:use-module (config features linux)
  #:use-module (config features package-management)

  #:use-module (config packages)

  #:use-module (contrib features javascript)
  #:use-module (contrib features emacs-xyz)

  #:use-module (gnu home-services ssh)
  #:use-module (gnu packages node)
  #:use-module (gnu packages emacs)
  #:use-module (gnu services)

  #:use-module (guix gexp)

  #:use-module (rde features base)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features fontutils)
  #:use-module (rde features gnupg)
  #:use-module (rde features irc)
  #:use-module (rde features keyboard)
  #:use-module (rde features linux)
  #:use-module (rde features mail)
  #:use-module (rde features networking)
  #:use-module (rde features password-utils)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features ssh)
  #:use-module (rde features system)
  #:use-module (rde features terminals)
  #:use-module (rde features version-control)
  #:use-module (rde features web-browsers)
  #:use-module (rde features wm)
  #:use-module (rde features xdg)
  #:use-module (rde features xdisorg)
  #:use-module (rde features)
  #:use-module (rde packages)

  #:export (%base-system-packages
            %base-home-packages
            %base-features
            %base-sway-config
            ssh-extra-config-service))

(define %nonguix-public-key
  (plain-file
   "nonguix-signing-key.pub"
   "(public-key
   (ecc
(curve Ed25519)
(q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)
))"))

;;;
;;; Default packages
;;;
(define %base-home-packages
  (append
   (strings->packages
    "git" "curl" "vim" "make"

    "flatpak"
    "font-awesome" ;; needed for waybar

    "pavucontrol"
    "gimp"

    "firefox"
    "emacs-arei"

    "kid3"

    "thunar"
    "thunar-volman"
    "thunar-shares-plugin"
    "thunar-archive-plugin"

    "gst-libav"
    "gst-plugins-bad"
    "gst-plugins-good"
    "gst-plugins-base"
    "gst-plugins-ugly"

    "adwaita-icon-theme"
    "hicolor-icon-theme"
    )))

(define %base-system-packages
  (append (strings->packages
           "gvfs"
           "cifs-utils"
           "nfs-utils"
           "libnfs")))


(define ssh-extra-config-service
  (simple-service
   'ssh-extra-config
   home-ssh-service-type
   (home-ssh-extension
    (extra-config
     (append
      (list
       (ssh-host
        (host "gnomebeach")
        (options
         '((host-name . "116.203.96.122")
           (port . 22)
           (control-master . "auto")
           (control-path . "~/.ssh/master-%r@%h:%p")
           (compression . #t))))
       (ssh-host
        (host "elftower")
        (options
         '((host-name . "192.168.178.92")
           (port . 22)
           (control-master . "auto")
           (control-path . "~/.ssh/master-%r@%h:%p")
           (compression . #t)))))))
    (toplevel-options
     '((host-key-algorithms . "+ssh-rsa")
       (pubkey-accepted-key-types . "+ssh-rsa"))))))

;;;
;;; Common features
;;; These include all features I need to be present
;;; despite the machine we're dealing with.
;;;
(define %base-features
  (list
   ;;; User info
   (feature-user-info
    #:user-name "db"
    #:full-name "Demis Balbach"
    #:email "db@minikn.xyz"
    #:user-groups (list "wheel" "audio" "video"
                        "input" "cdrom" "disk"
                        "lp"))

   ;;; Mail
   (feature-mail-settings
    #:mail-accounts
    (list (mail-account
           (id 'personal)
           (fqda "db@minikn.xyz")
           (type 'mailbox)
           (pass-cmd "pass show Mail/mailbox.org/db@minikn.xyz"))))

   ;;; GnuPG
   (feature-gnupg
    #:gpg-primary-key "F17DDB98CC3C405C"
    #:gpg-ssh-agent? #t
    #:ssh-keys '(("E3FFA5A1B444A4F099E594758008C1D8845EC7C0")))

   ;;; SSH
   (feature-ssh)

   ;;; Git
   (feature-git
    #:sign-commits? #t
    #:git-sign-key "F17DDB98CC3C405C"
    #:git-send-email? #t
    #:extra-config
    '((format
       ((to . ~abcdw/rde-devel@lists.sr.ht)
        (subjectPrefix . "PATCH rde")
        (coverLetter . auto)))))

   ;;; Passwords
   (feature-password-store
    #:remote-password-store-url "git@gitlab.com:minikN/pass.git")

   ;;; Keyboard layout
   (feature-keyboard
    #:keyboard-layout
    (keyboard-layout
     "us"
     "altgr-intl"
     #:options '("ctrl:nocaps")))

   ;;; Bluetooth
   (feature-bluetooth)

   ;;; Terminal, shell
   (feature-zsh)
   (feature-vterm)
   (feature-alacritty
    #:config-file (local-file "./applications/alacritty/alacritty.yml")
    #:default-terminal? #t)
   (feature-compile)

   ;;; IRC
   (feature-irc-settings
    #:irc-accounts
    (list
     (irc-account
      (id 'soju)
      (network "irc.minikn.xyz")
      (bouncer? #t)
      (nick "minikN"))
     (irc-account
      (id 'libera)
      (network "irc.libera.chat")
      (bouncer? #f)
      (nick "minikN"))))

   ;;; Emacs
   (feature-emacs
    #:emacs emacs-next-pgtk
    #:default-terminal? #f
    #:default-application-launcher? #f)
   (feature-emacs-appearance
    #:fringes 8)
   (feature-emacs-modus-themes)
   (feature-emacs-completion
    #:mini-frame? #f)
   (feature-emacs-vertico)
   (feature-emacs-erc)
   (feature-emacs-dired)
   (feature-emacs-git)
   (feature-emacs-message
    #:message-signature
    "Best regards / Mit freundlichen Grüßen,\nDemis Balbach")
   (feature-emacs-project)
   ;; (feature-emacs-perspective)
   (feature-emacs-which-key
    #:min-height 5)
   ;; (feature-emacs-evil)

   ;; ;;; LSP
   ;; (feature-emacs-corfu)
   ;; (feature-emacs-eglot)
   ;; (feature-emacs-geiser)
   ;; (feature-javascript
   ;;  #:node node-lts)

   ;; direnv
   (feature-direnv)

   ;; ;;; Mail
   (feature-notmuch
    #:notmuch-search-oldest-first #f)
   (feature-msmtp)
   (feature-isync)
   (feature-l2md) ;; needed for mbsync -a

   ;; Networking
   (feature-networking)

   (feature-ungoogled-chromium
    #:default-browser? #t)

   ;;; Fonts
   (feature-fonts
    #:default-font-size 15)

   (feature-swaynotificationcenter)

   ;;; Services
   (feature-base-services
    #:guix-substitute-urls (list "https://bordeaux.guix.gnu.org"
                                 "https://substitutes.nonguix.org")
    #:guix-authorized-keys (list %nonguix-public-key))
   (feature-desktop-services)

   (feature-pipewire)
   (feature-rofi)
   (feature-xdg
    #:xdg-user-directories-configuration
    (home-xdg-user-directories-configuration
     (music "$HOME/music")
     (videos "$HOME/vids")
     (pictures "$HOME/pics")
     (documents "$HOME/docs")
     (download "$HOME/dl")
     (desktop "$HOME")
     (publicshare "$HOME")
     (templates "$HOME")))
   (feature-bootloader)

   ;; ;;; Sway
   (feature-sway
    #:xwayland? #t
    #:extra-config
    `((default_border none)
      (assign "[app_id=\"chromium-browser\"]" workspace 1)
      (for_window "[app_id=\"pavucontrol\"]" floating enable, border pixel)
      (for_window "[app_id=\"pinentry-qt\"]" floating enable, border pixel)
      (for_window "[title=\"File Operation Progress\"]" floating enable, border pixel)
      (for_window "[app_id=\"org.kde.krename\"]" floating enable, border pixel)
      (for_window "[app_id=\"org.rncbc.qjackctl\"]" floating enable, border pixel)
      (bindsym $mod+Shift+q kill)
      (bindsym $mod+Shift+w layout tabbed)
      (bindsym $mod+Shift+s layout stacking)
      (bindsym $mod+Shift+e layout toggle split)
      (bindsym $mod+Shift+Ctrl+r mode "resize")
      (mode "resize" ((bindsym Left resize shrink width 30px)
                      (bindsym Down resize grow height 30px)
                      (bindsym Up resize shrink height 30px)
                      (bindsym Right resize grow width 30px)
                      (bindsym Escape mode "default")))))
   (feature-sway-run-on-tty #:sway-tty-number 2)
   (feature-sway-screenshot)
   (feature-swaylock
    #:swaylock (@ (gnu packages wm) swaylock-effects)
    ;; The blur on lock screen is not privacy-friendly.
    #:extra-config '((screenshots)
                     (effect-blur . 7x5)
                     (clock)))

   ;;; Package management
   ;;; Has to come after feature-sway.
   ;;(feature-nix)
   ))
