(define-module (config packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages web)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (emacs-corfu-doc
            ;;emacs-jsdoc
            steamos-compositor-plus
            rofi-ttv))

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

(define steamos-modeswitch-inhibitor
  (package
   (name "steamos-modeswitch-inhibitor")
   (version "1.10")
   (source (origin
            (method url-fetch)
            (uri
             (string-append
              "http://repo.steampowered.com/steamos/pool/main/s/steamos-modeswitch-inhibitor/steamos-modeswitch-inhibitor_"
              version
              ".tar.xz"))
            (sha256
             (base32
              "1lskfb4l87s3naz2gmc22q0xzvlhblywf5z8lsiqnkrrxnpbbwj7"))))
     (native-inputs
      (list autoconf
            automake
            pkg-config))
     (inputs
      (list libxxf86vm
            libx11
            libxrender
            libxrandr))
   (build-system gnu-build-system)
   (home-page "http://repo.steampowered.com/steamos/pool/main/s/steamos-modeswitch-inhibitor/")
   (synopsis "SteamOS Mode Switch Inhibitor")
   (description "Shared library which fakes any mode switch attempts to prevent full screen apps from changing resolution.")
   (license license:gpl3+)))

(define steamos-compositor-plus
  (let ((commit "7c0011cf91e87c30c2a630fee915ead58ef9dcf5")
        (revision "0")
        (version "1.3.0"))
    (package
     (name "steamos-compositor-plus")
     (version (git-version version revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ChimeraOS/steamos-compositor-plus")
             (commit commit)))
       (sha256
        (base32 "1bjl517bw10w18f4amdz3kwzkdz8w6wg8md2bk3wpqjrn26p45gd"))
       (file-name (git-file-name name version))))
     (arguments
      (list
       #:phases
       #~(modify-phases
          %standard-phases
          (add-after 'unpack 'copy-scripts
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (bin (string-append out "/bin"))
                              (share (string-append out "/share"))
                              (src (string-append share "/steamos-compositor-plus/bin"))
                              (scripts (string-append bin "/scripts")))
                         (mkdir-p scripts)
                         (copy-recursively "./usr/share" share)
                         (copy-recursively "./usr/bin" bin)
                         (find-files src (lambda (file stat)
                                           (symlink file (string-append scripts "/" (basename file))))))))
          (add-after 'copy-scripts 'patch-bin
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (share (string-append out "/share"))
                              (bin (string-append out "/bin"))
                              (xset (string-append (assoc-ref inputs "xset") "/bin/xset"))
                              (modeswitch-inhibitor (string-append
                                                     (assoc-ref inputs "steamos-modeswitch-inhibitor")
                                                     "/lib/libmodeswitch_inhibitor.so")))
                         (substitute* "./usr/bin/steamos-session"
                                      ;; No need to export scripts to PATH.
                                      (("export.+\\{PATH\\}" line) (string-append "#" line))
                                      (("export.+so" line) (string-append "export" " " "LD_PRELOAD=" modeswitch-inhibitor))
                                      (("set_hd_mode.sh") (string-append bin "/scripts/set_hd_mode.sh"))
                                      (("loadargb_cursor") (string-append bin "/loadargb_cursor"))
                                      (("cp") "ln -s")
                                      (("/usr/share/icons/steam/arrow.png")
                                       (string-append share "/icons/steam/arrow.png"))
                                      (("/usr/share/pixmaps/steam-bootstrapper.jpg")
                                       (string-append share "/pixmaps/steam-bootstrapper.jpg"))
                                      (("steamcompmgr") (string-append bin "/steamcompmgr"))
                                      (("xset") xset)))))
          (add-after 'patch-bin 'copy-bin
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (bin (string-append out "/bin")))
                         (install-file "./usr/bin/steamos-session" bin)
                         (chmod (string-append bin "/steamos-session") #o555)))))))
     (native-inputs
      (list autoconf
            automake
            pkg-config))
     (propagated-inputs
      (list xinit
            xorg-server))
     (inputs
      (list libxxf86vm
            libx11
            libxrender
            libxcomposite
            libgudev
            glu
            sdl-image
            xset
            steamos-modeswitch-inhibitor))
     (build-system gnu-build-system)
     (home-page "https://github.com/ChimeraOS/steamos-compositor-plus")
     (synopsis "Compositor used by SteamOS 2.x with some added tweaks and fixes")
     (description "This is a fork of the SteamOS compositor, currently based on
version 1.35. It includes out of the box 4k (3840x2160) support, allows adjusting
resolution/refresh rate through a configuration file, hides the annoying color
flashing on startup of Proton games and adds a fix for games that start in
the background, including Dead Cells, The Count Lucanor, most Feral games
and probably others.")
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

(define rofi-ttv
  (let ((commit "e9c722481b740196165f840771b3ae58b7291694")
        (revision "0")
        (version "0.1"))
    (package
     (name "rofi-ttv")
     (version (git-version version revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/loiccoyle/rofi-ttv")
             (commit commit)))
       (sha256
        (base32 "1m6jf87gsspi2qmhnf4p2ibqp0g1kvcnphcji8qf4z39x73f7jym"))
       (file-name (git-file-name name version))))
     (build-system gnu-build-system)
     (inputs (list
              curl
              jq
              rofi
              youtube-dl
              mpv))
     (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
         %standard-phases
         (delete 'configure)
         (delete 'install)
         (delete 'build)
         (add-after 'unpack 'patch-bin
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (curl (string-append (assoc-ref inputs "curl") "/bin/curl"))
                             (jq (string-append (assoc-ref inputs "jq") "/bin/jq"))
                             (rofi (string-append (assoc-ref inputs "rofi") "/bin/rofi"))
                             (youtube-dl (string-append (assoc-ref inputs "youtube-dl") "/bin/youtube-dl"))
                             (mpv (string-append (assoc-ref inputs "mpv") "/bin/mpv")))
                        (substitute* "./rofi-ttv"
                                     (("curl") curl)
                                     (("jq") jq)
                                     (("rofi ") (string-append rofi " "))
                                     (("youtube-dl") youtube-dl)
                                     (("mpv") mpv)))))
         (add-after 'patch-bin 'copy-bin
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin")))
                        (install-file "./rofi-ttv" bin)
                        (chmod (string-append bin "/rofi-ttv") #o555)))))))
     (home-page "https://github.com/galeo/corfu-doc")
     (synopsis "A scripts that uses rofi, youtube-dl and mpv to view twitch streams.")
     (description "A scripts that uses rofi, youtube-dl and mpv to view twitch streams.")
     (license license:expat))))
