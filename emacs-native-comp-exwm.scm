(define-module (emacs-native-comp-exwm)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages emacs-xyz))

(define-public emacs-native-comp-exwm
  (package
   (inherit emacs-exwm)
   (name "emacs-native-comp-exwm")
   (synopsis "test")
   (inputs
    `(("picom" ,picom)
      ,@(package-inputs emacs-exwm)))
   (arguments
    `(,@(package-arguments emacs-exwm)
      #:emacs ,emacs-native-comp
      #:phases (modify-phases %standard-phases
			      (add-after 'build 'install-xsession
					 (lambda* (#:key inputs outputs #:allow-other-keys)
						  (let* ((out (assoc-ref outputs "out"))
							 (xsessions (string-append out "/share/xsessions"))
							 (bin (string-append out "/bin"))
							 (exwm-executable (string-append bin "/exwm")))

						    ;; Add a .desktop file to xsessions
						    (mkdir-p xsessions)
						    (mkdir-p bin)
						    (make-desktop-entry-file
						     (string-append xsessions "/exwm.desktop")
						     #:name ,name
						     #:comment ,synopsis
						     #:exec exwm-executable
						     #:try-exec exwm-executable)

						    ;; Add a shell wrapper to bin
						    (with-output-to-file exwm-executable
						      (lambda _
							(format #t "#!~a ~@
										       ~a +SI:localuser:$USER ~@
										       ~a &
										       exec ~a --exit-with-session ~a \"$@\" --eval '~s' ~%"
								(string-append (assoc-ref inputs "bash") "/bin/sh")
								(string-append (assoc-ref inputs "xhost") "/bin/xhost")
								(string-append (assoc-ref inputs "picom") "/bin/picom")
								(string-append (assoc-ref inputs "dbus") "/bin/dbus-launch")
								(string-append (assoc-ref inputs "emacs") "/bin/emacs")
								'(cond
								  ((file-exists-p "~/.exwm")
								   (load-file "~/.exwm"))
								  ((not (featurep 'exwm))
								   (require 'exwm)
								   (require 'exwm-config)
								   (exwm-config-default)
								   (message (concat "exwm configuration not found. "
										    "Falling back to default configuration...")))))))
						    (chmod exwm-executable #o555)
						    #t))))))))
