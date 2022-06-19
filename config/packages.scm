(define-module (config packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (emacs-corfu-doc
            ;;emacs-jsdoc
            ))

(define emacs-corfu-doc
  (let ((commit "5dd2a2987a9e470112151d89be3f35d95c633be5")
        (revision "0")
        (version "0.7"))
    (package
     (name "emacs-corfu-doc")
     (version (git-version version revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/galeo/corfu-doc")
             (commit commit)))
       (sha256
        (base32 "01j7cpbijldfg0kdynw8ap2hvcdb7m36n2g7zbkxz56qdhl2s71n"))
       (file-name (git-file-name name version))))
     (build-system emacs-build-system)
     (propagated-inputs (list emacs-corfu))
     (home-page "https://github.com/galeo/corfu-doc")
     (synopsis "Display a documentation popup for completion candidate when using Corfu.")
     (description "Display a documentation popup for completion candidate when using Corfu.")
     (license license:gpl3+))))

;; TODO: Uncomment after https://issues.guix.gnu.org/issue/49946 is merged
;; (define emacs-jsdoc
;;   (let ((commit "2e7c02ff2dc422bc21c405bd90a7092c2f599630")
;;         (revision "0")
;;         (version "0.2"))
;;     (package
;;      (name "emacs-jsdoc")
;;      (version (git-version version revision commit))
;;      (source
;;       (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/isamert/jsdoc.el")
;;              (commit commit)))
;;        (sha256
;;         (base32 "07sz5lpyqv7ixvnnzdfjkj7f0ykdz31lkljp13pvlf36a6sff4rc"))
;;        (file-name (git-file-name name version))))
;;      (build-system emacs-build-system)
;;      (propagated-inputs
;;       (list emacs-s
;;             emacs-dash
;;             emacs-tree-sitter))
;;      (home-page "https://github.com/isamert/jsdoc.el")
;;      (synopsis "Inserts JSDoc function comments/typedefs easily.")
;;      (description "Inserts JSDoc function comments/typedefs easily.
;; It also tries to infer types by itself while doing that.
;; Type inference is quite primitive.")
;;      (license license:gpl3+))))
