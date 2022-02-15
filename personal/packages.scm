(define-module (personal packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-corfu-doc
  (let ((commit "8d8f9317dd75cc83f3a2ba04c2b372f7fb06b2fc")
        (revision "0")
        (version "0.1"))
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
        (base32 "1bd97zv4w6hafqvxlaw9wkl4ang8mcj53pr28a38iy2y2adrksgw"))
       (file-name (git-file-name name version))))
     (build-system emacs-build-system)
     (propagated-inputs (list emacs-corfu))
     (home-page "https://github.com/galeo/corfu-doc")
     (synopsis "Display a documentation popup for completion candidate when using Corfu.")
     (description "Display a documentation popup for completion candidate when using Corfu.")
     (license license:gpl3+))))
