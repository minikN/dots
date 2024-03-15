(define-module (config)
  ;#:use-module (config workhorse)
  ;#:use-module (config geekcave)
  #:use-module (config elftower)
  #:use-module (ice-9 match)
  #:use-module (rde features))

;; (define* (use-nested-configuration-modules
;;           #:key
;;           (users-subdirectory "/config/features")
;;           (hosts-subdirectory "/config/packages"))
;;   (use-modules (guix discovery)
;;                (guix modules))

;;   (define current-module-file
;;     (search-path %load-path
;;                  (module-name->file-name (module-name (current-module)))))

;;   (define current-module-directory
;;     (dirname (and=> current-module-file canonicalize-path)))

;;   (define src-directory
;;     (dirname current-module-directory))

;;   (define current-module-subdirectory
;;     (string-drop current-module-directory (1+ (string-length src-directory))))

;;   (define users-modules
;;     (scheme-modules
;;      src-directory
;;      (string-append current-module-subdirectory users-subdirectory)))

;;   (define hosts-modules
;;     (scheme-modules
;;      src-directory
;;      (string-append current-module-subdirectory hosts-subdirectory)))

;;   (map (lambda (x) (module-use! (current-module) x)) hosts-modules)
;;   (map (lambda (x) (module-use! (current-module) x)) users-modules))

;; (use-nested-configuration-modules)


;; (define geekcave-os
;;  (rde-config-operating-system geekcave-config))

;; (define geekcave-he
;;  (rde-config-home-environment geekcave-config))

;; (define workhorse-os
;;  (rde-config-operating-system workhorse-config))

;; (define workhorse-he
;;  (rde-config-home-environment workhorse-config))

(define elftower-os
 (rde-config-operating-system elftower-config))

(define elftower-he
  (rde-config-home-environment elftower-config))

(define (dispatcher)
 (let ((target (getenv "TARGET")))
   (match target
     ("workhorse-he" workhorse-he)
     ("workhorse-os" workhorse-os)
     ("elftower-he" elftower-he)
     ("elftower-os" elftower-os)
     ("geekcave-he" geekcave-he)
     ("geekcave-os" geekcave-os)
     )))

;;; Enable this to print the entire config object
;;; to see what's enable for example
;; (pretty-print-rde-config geekcave-config)

(dispatcher)

;;elftower-he
