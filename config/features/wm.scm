(define-module (config features wm)
  #:use-module (rde features wm)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gl)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:export (waybar-common-modules
            waybar-left-modules
            waybar-right-modules
            waybar-primary-modules))

(define* (waybar-custom-cpu #:key (bar-id 'main))
  "Displays information about the current CPU load."
  (lambda (config)
    ((@@ (rde features wm) waybar-module)
     'cpu
     `((interval . 2)
       (format . " {max_frequency} Ghz | {usage}%"))
     #:bar-id bar-id)))

(define* (waybar-custom
          #:key
          (bar-id 'main)
          (name 'main)
          (exec #f)
          (return-type "json")
          (icon #f))
  "Executes a custom script EXEC. The script is expected
to return a valid json object."
  (lambda (config)
    ((@@ (rde features wm) waybar-module)
     (symbol-append 'custom/ name)
     `((interval . 2)
       (exec . ,exec)
       (return-type . ,return-type)
       (tooltip . "{tooltip}")
       (format . ,(if icon
                       (string-append icon " {}")
                       "{}")))
     #:bar-id bar-id)))

(define waybar-custom-gpu
  (program-file "waybar-custom-gpu.sh"
                #~(system
                   (let* ((raw-clock (string-append
                                      #$(file-append coreutils "/bin/cat")
                                      " /sys/class/drm/card0/device/pp_dpm_sclk" " | "
                                      #$(file-append grep "/bin/egrep") " -o '[0-9]{0,4}Mhz \\W'" " | "
                                      #$(file-append sed "/bin/sed") " \"s/Mhz \\*//\""
                                      ))
                          (clock (string-append
                                  #$(file-append coreutils "/bin/echo")
                                  " \"scale=1;" "$(" raw-clock ")" "/1000\" | "
                                  #$(file-append bc "/bin/bc") " | "
                                  #$(file-append sed "/bin/sed") " -e 's/^-\\./-0./' -e 's/^\\./0./'"
                                  ))
                          (raw-temp (string-append
                                     #$(file-append coreutils "/bin/cat")
                                     " /sys/class/drm/card0/device/hwmon/hwmon4/temp1_input"))
                          (temp (string-append
                                  #$(file-append coreutils "/bin/echo")
                                  " $((`" raw-temp "`/1000))"))
                          (busy-percent (string-append
                                         #$(file-append coreutils "/bin/cat")
                                         " /sys/class/hwmon/hwmon4/device/gpu_busy_percent"
                                         ))
                          (device-info (string-append
                                        #$(file-append mesa-utils "/bin/glxinfo") " -B | "
                                        #$(file-append grep "/bin/grep") " 'Device:' | "
                                        #$(file-append sed "/bin/sed") " 's/^.*: //'"
                                        ))
                          (driver-info (string-append
                                        #$(file-append mesa-utils "/bin/glxinfo")
                                        " -B | " #$(file-append grep "/bin/grep") " \"OpenGL version\"")))
                     (string-append
                      #$(file-append coreutils "/bin/echo") " "
                      "'{"
                      "\"text\": \"'" "$(" clock ")" "' Ghz | '" "$(" temp ")" "'°C | '" "$(" busy-percent ")" "'%\","
                      "\"class\": \"custom-gpu\","
                      "\"tooltip\": \"<b>'" "$(" device-info ")" "'</b>\\n'" "$(" driver-info ")" "'\""
                      "}'")
                     ))))

(define (waybar-common-modules bar)
  (list
   (waybar-custom-cpu #:bar-id bar)
   (waybar-custom
    #:bar-id bar
    #:name 'gpu
    #:icon ""
    #:exec waybar-custom-gpu)
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
