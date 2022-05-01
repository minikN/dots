(define-module (personal workhorse)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home-services shells)
  #:use-module (gnu system file-systems)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features linux)
  #:use-module (rde features system)
  #:use-module (rde features fontutils)
  #:export (workhorse-features
	    workhorse-services
            workhorse-sway-config))

(define workhorse-sway-config
  `((output HDMI-A-1 pos 0 0)
    (output eDP-1 pos 2560 0)
    (workspace 1 output HDMI-A-1)     ;; Browser
    (workspace 2 output eDP-1) ;; Terminal
    (workspace 3 output eDP-1) ;; Code
    (workspace 4 output eDP-1) ;; Agenda
    (workspace 5 output HDMI-A-1) ;; Music/Video
    (workspace 6 output HDMI-A-1) ;; Chat
    (workspace 7 output HDMI-A-1) ;; Games
    (output eDP-1 scale 1.5)))

(define workhorse-services
  (list
   (simple-service
    'setup-gnome-session
    home-shell-profile-service-type
    (list
     "if \
[ \"$XDG_SESSION_DESKTOP\" = \"ubuntu-wayland\" ] || \
[ \"$XDG_SESSION_DESKTOP\" = \"ubuntu\" ]; then
	export XDG_CURRENT_DESKTOP=\"ubuntu:GNOME\"
      	unset CLUTTER_BACKEND
	unset RTC_USE_PIPEWIRE
	unset SDL_VIDEODRIVER
	unset MOZ_ENABLE_WAYLAND
	unset ELM_ENGINE
	unset ECORE_EVAS_ENGINE
	unset QT_QPA_PLATFORM
	unset _JAVA_AWT_WM_NONREPARENTING
fi
[ \"$XDG_SESSION_DESKTOP\" = \"ubuntu\" ] && \
export XDG_SESSION_TYPE=\"x11\"
"))
   (simple-service
    'setup-env-vars
    home-environment-variables-service-type
    `(("GUIX_LOCPATH" . "$HOME/.guix-home/profile/lib/locale") ;; requires glibc-locales
      ;; TODO: Exlude in feature-nix
      ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.nix-profile/share")
      ("XCURSOR_PATH" . "$XCURSOR_PATH:/usr/share/icons")))))

(define workhorse-features
  (list
   ;;; HiDPI
   (feature-hidpi)))


