(define-module (personal features wm)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (rde features)
  #:use-module (rde features wm)
  #:use-module (guix gexp)
  #:export (feature-sway-desktop-file
	    waybar-module-workspaces
            waybar-module-temperature
            waybar-module-memory
            waybar-module-cpu
            waybar-module-disk
            waybar-module-disk-games
            waybar-module-audio
            waybar-module-window))

(define* (feature-sway-desktop-file)
  (define (get-home-services config)
    (let ((sway (get-value 'sway config)))
      (list
       (simple-service
	'setup-sway-desktop-file
	home-files-service-type
	(list
	 `("config/sway/sway.desktop"
	   ,(mixed-text-file "sway.desktop" "\
[Desktop Entry]
Name=Sway (RDE)
Comment=This session logs you into Sway
Exec="
			     (file-append sway "/bin/sway")
			     "\nTryExec="
			     (file-append sway "/bin/sway")
			     "\nType=Application
DesktopNames=sway
X-GDM-SessionRegisters=true")))))))
  
  (feature
   (name 'sway-desktop-file)
   (values `(('sway-desktop-file . #t)))
   (home-services-getter get-home-services))) 

(define* (waybar-module-workspaces)
  (waybar-sway-workspaces
   #:format-icons '(("1" . )
                    ("2" . )
                    ("3" . )
                    ("4" . )
                    ("5" . )
                    ("6" . )
                    ("7" . )
                    ("urgent" . )
                    ("focused" . )
                    ("default" . ))
   #:persistent-workspaces '(("1" . #())
                             ("2" . #())
                             ("3" . #())
                             ("4" . #())
                             ("5" . #())
                             ("6" . #())
                             ("7" . #()))))

(define* (waybar-module-temperature)
  ((@@ (rde features wm) waybar-module)
   'temperature
   `(("thermal-zone" . 1))))

(define* (waybar-module-memory)
  ((@@ (rde features wm) waybar-module)
   'memory
   `(("interval" . 30)
     ("format" . " {percentage}%")
     ("tooltip" . "{used:0.1f} RAM used"))))

(define* (waybar-module-cpu)
  ((@@ (rde features wm) waybar-module)
   'cpu
   `(("interval" . 2)
     ("format" . " {usage}%")
     ("tooltip" . "Frequency: {max_frequency}"))))

(define* (waybar-module-disk)
  ((@@ (rde features wm) waybar-module)
   'disk#root
   `(("interval" . 30)
     ("format" . " {percentage_used}%"))))

(define* (waybar-module-disk-games)
  ((@@ (rde features wm) waybar-module)
   'disk#games
   `(("interval" . 30)
     ("format" . " {percentage_used}%")
     ("path" . "/home/db/games"))))

(define* (waybar-module-audio)
  ((@@ (rde features wm) waybar-module)
   'pulseaudio
   `(("scroll-step" . 3)
     ("format" . "{icon} {volume}%")
     ("format-muted" . " 0%")
     ("format-icons" . (("headphone" . )
                        ("headset" . )
                        ("default" . #(  ))))
     ("on-click" . "pavucontrol")
     ("on-click-middle" . "pactl set-sink-mute @DEFAULT_SINK@ toggle"))))

(define* (waybar-module-window)
  ((@@ (rde features wm) waybar-module)
   'sway/window
   `(("max-length" . 50)
     ("rewrite" . (("(.*) - Chromium" . " $1")
                   ("(.*) - GNU Emacs .+" . " $1"))))
   `((#{#window}#
      ((margin-left . 1em)
       (margin-right . 1em))))
   #:placement 'modules-center))

