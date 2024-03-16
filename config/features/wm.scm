(define-module (config features wm)
  #:use-module (rde features wm)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gl)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:export (waybar-main-modules))

(define waybar-main-modules
  (append (list
           ;; Left
           (waybar-sway-workspaces
            ;#:all-outputs? #t
            #:format-icons
            '(("1" . " WWW")
              ("2" . " TERM")
              ("3" . " CODE")
              ("4" . " AGENDA")
              ("5" . " MUSIC")
              ("6" . " CHAT")
              ("urgent" . )
              ("focused" . )
              ("default" . ))
            #:persistent-workspaces
            '(("1" . #())
              ("2" . #())
              ("3" . #())
              ("4" . #())
              ("5" . #())
              ("6" . #())))

           ;; Center
           (waybar-sway-window)

           ;; Right
           (waybar-cpu)
           (waybar-memory)
           (waybar-disk)
           (waybar-temperature)
           (waybar-battery)
           (waybar-volume)
           (waybar-tray)
           (waybar-clock)
           )))
