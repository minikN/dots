(define-module (config features wm)
  #:use-module (rde features wm)
  #:export (waybar-common-modules
            waybar-left-modules
            waybar-right-modules
            waybar-primary-modules))

(define (waybar-common-modules bar)
  (list
   (waybar-cpu #:bar-id bar)
   (waybar-memory #:bar-id bar)
   (waybar-disk #:bar-id bar)
   (waybar-temperature #:bar-id bar)
   (waybar-volume
    #:show-percentage? #t
    #:scroll-step 5
    #:bar-id bar)
   (waybar-battery #:bar-id bar)
   (waybar-tray #:bar-id bar)
   (waybar-clock
    #:format "{:%H:%M}"
    #:bar-id bar)))

(define* (waybar-left-modules
         #:key (bar-id 'left))
  (list
   (waybar-sway-workspaces
    #:bar-id bar-id
    #:format-icons
    '(("1" . " WWW")
      ("5" . " MUSIC")
      ("6" . " CHAT")
      ("urgent" . )
      ("focused" . )
      ("default" . ))
    #:persistent-workspaces
    '(("1" . #())
      ("5" . #())
      ("6" . #())))
   (waybar-sway-window
    #:bar-id bar-id)))

(define waybar-right-modules
  (append (list
           (waybar-sway-workspaces
            #:bar-id 'right
            #:format-icons
            '(("2" . " TERM")
              ("3" . " CODE")
              ("4" . " AGENDA")
              ("urgent" . )
              ("focused" . )
              ("default" . ))
            #:persistent-workspaces
            '(("2" . #())
              ("3" . #())
              ("4" . #())))
           (waybar-sway-window
            #:bar-id 'right))
          (waybar-common-modules 'right)))

(define waybar-primary-modules
  (append (list
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
           (waybar-sway-window))
          (waybar-common-modules 'main)))
