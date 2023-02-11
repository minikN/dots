(define-module (config)
  #:use-module (config workhorse)
  #:use-module (config geekcave)
  #:use-module (config elftower)
  #:use-module (ice-9 match)
  #:use-module (rde features))

(define geekcave-os
  (rde-config-operating-system geekcave-config))

(define geekcave-he
  (rde-config-home-environment geekcave-config))

(define workhorse-os
  (rde-config-operating-system workhorse-config))

(define workhorse-he
  (rde-config-home-environment workhorse-config))

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
      ("geekcave-os" geekcave-os))))

;;; Enable this to print the entire config object
;;; to see what's enable for example
;; (pretty-print-rde-config geekcave-config)

(dispatcher)
