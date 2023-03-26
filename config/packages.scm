(define-module (config packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu build chromium-extension)

  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)

  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)

  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)

  #:use-module (nongnu packages steam-client)

  ;; #:use-module (gnu packages gcc)
  ;; #:use-module (gnu packages libusb)
  ;; #:use-module (gnu packages llvm)
  ;; #:use-module (guix build-system linux-module)
  ;; #:use-module (nonguix build-system binary)
  )

(define-public steamos-modeswitch-inhibitor
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

(define-public steamos-compositor-plus
  (let ((commit "e4b99dd5f56a388aa24fe3055d0e983cb3d5d32a")
        ;;(commit "7c0011cf91e87c30c2a630fee915ead58ef9dcf5")
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
        (base32 "0rqckj05389djc4ahzfy98p3z0p884gbbl94lbm06za72bhnidr5")
       ;;(base32 "1bjl517bw10w18f4amdz3kwzkdz8w6wg8md2bk3wpqjrn26p45gd")
       )
       (file-name (git-file-name name version))))
     (arguments
      (list
       #:phases
       #~(modify-phases
          %standard-phases
          (add-after 'patch-source-shebangs 'copy-scripts
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
                              (src (string-append share "/steamos-compositor-plus/bin"))
                              (bin (string-append out "/bin"))
                              (aplay (string-append (assoc-ref inputs "alsa-utils") "/bin/aplay"))
                              (pamixer (string-append (assoc-ref inputs "pamixer") "/bin/pamixer"))
                              (udevadm (string-append (assoc-ref inputs "eudev") "/bin/udevadm"))
                              (xset (string-append (assoc-ref inputs "xset") "/bin/xset"))
                              (xrandr (string-append (assoc-ref inputs "xrandr") "/bin/xrandr"))
                              (xinput (string-append (assoc-ref inputs "xinput") "/bin/xinput"))
                              (grep (string-append (assoc-ref inputs "grep") "/bin/grep"))
                              (steam (string-append (assoc-ref inputs "steam") "/bin/steam"))
                              (modeswitch-inhibitor (string-append
                                                     (assoc-ref inputs "steamos-modeswitch-inhibitor")
                                                     "/lib/libmodeswitch_inhibitor.so")))
                         (substitute* "./usr/bin/steamos-session"
                                      ;; No need to export scripts to PATH.
                                      (("export.+\\{PATH\\}" line) (string-append "#" line))
                                      (("\"steam ") (string-append "\"" steam " "))
                                      (("export.+so" line) (string-append "export" " " "LD_PRELOAD=" modeswitch-inhibitor))
                                      (("set_hd_mode.sh") (string-append bin "/scripts/set_hd_mode.sh"))
                                      (("loadargb_cursor") (string-append bin "/loadargb_cursor"))
                                      (("cp") "ln -s")
                                      (("/usr/share/icons/steam/arrow.png")
                                       (string-append share "/icons/steam/arrow.png"))
                                      (("/usr/share/pixmaps/steam-bootstrapper.jpg")
                                       (string-append share "/pixmaps/steam-bootstrapper.jpg"))
                                      (("steamcompmgr") (string-append bin "/steamcompmgr"))
                                      (("steam-browser.desktop") (string-append share "/applications/steam-browser.desktop"))
                                      (("xset") xset))
                         (substitute* (string-append src "/set_hd_mode.sh")
                                      (("xrandr") xrandr)
                                      (("egrep") "grep -E")
                                      (("grep") grep)
                                      (("xinput") xinput)
                                      (("udevadm") udevadm))
                         (substitute* (string-append src "/screen_toggle.sh")
                                      (("xrandr") xrandr))
                         (substitute* (string-append src "/brightness_up.sh")
                                      (("xrandr") xrandr))
                         (substitute* (string-append src "/brightness_down.sh")
                                      (("xrandr") xrandr))
                         (substitute* (string-append src "/audio_volup.sh")
                                      (("aplay") aplay)
                                      (("pamixer") pamixer))
                         (substitute* (string-append src "/audio_voldown.sh")
                                      (("aplay") aplay)
                                      (("pamixer") pamixer))
                         (substitute* (string-append src "/audio_mute.sh")
                                      (("aplay") aplay)
                                      (("pamixer") pamixer))
                         (substitute* (string-append share "/xsessions/steamos.desktop")
                                      (("steamos-session") (string-append bin "/steamos-session")))
                         (substitute* (string-append share "/applications/steam-browser.desktop")
                                      (("steam ") (string-append steam " ")))
                         )))
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
      (list alsa-utils
            eudev
            grep
            libxxf86vm
            libx11
            libxrender
            libxcomposite
            libgudev
            glu
            pamixer
            sdl-image
            steam
            xinput
            xset
            xrandr
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

(define-public rofi-ttv
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

;; (define-public evdi
;;   (package
;;     (name "evdi")
;;     (version "1.12.0")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/DisplayLink/evdi.git")
;;              (commit "bdc258b25df4d00f222fde0e3c5003bf88ef17b5")))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "1yi7mbyvxm9lsx6i1xbwp2bihwgzhwxkydk1kbngw5a5kw9azpws"))))
;;     (build-system linux-module-build-system)
;;     (arguments
;;      `(#:tests? #f ;; no test suite
;;        #:phases
;;        (modify-phases %standard-phases
;;          (add-after 'unpack 'chdir
;;            (lambda _ (chdir "module"))))))
;;     (home-page "https://github.com/DisplayLink/evdi")
;;     (synopsis "EVDI Linux kernel module")
;;     (description
;;      "The @acronym{EVDI, Extensible Virtual Display Interface} is a Linux kernel module
;; that enables management of multiple screens, allowing user-space programs to
;; take control over what happens with the image.")
;;     (license license:gpl2)))

;; (define-public libevdi
;;   (package
;;     (name "libevdi")
;;     (version "1.12.0")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/DisplayLink/evdi.git")
;;              (commit "bdc258b25df4d00f222fde0e3c5003bf88ef17b5")))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "1yi7mbyvxm9lsx6i1xbwp2bihwgzhwxkydk1kbngw5a5kw9azpws"))))
;;     (build-system gnu-build-system)
;;     (arguments
;;      `(#:tests? #f ;; no test suite
;;        #:make-flags `("CC=gcc")
;;        #:phases
;;        (modify-phases %standard-phases
;;          (delete 'configure) ;; no configure script
;;          (replace 'install
;;            (lambda* (#:key outputs #:allow-other-keys)
;;              (let* ((out (assoc-ref outputs "out"))
;;                     (lib (string-append out "/lib")))
;;                (mkdir-p lib)
;;                (copy-file "libevdi.so" (string-append lib "/libevdi.so")))))
;;          (add-after 'unpack 'chdir
;;            (lambda _ (chdir "library"))))))
;;     (inputs
;;      (list libdrm))
;;     (home-page "https://github.com/DisplayLink/evdi")
;;     (synopsis "EVDI Linux kernel module")
;;     (description
;;      "The @acronym{EVDI, Extensible Virtual Display Interface} is a Linux kernel module
;; that enables management of multiple screens, allowing user-space programs to
;; take control over what happens with the image.")
;;     (license license:lgpl2.1)))

;; (define-public displaylink
;;   (package
;;     (name "displaylink")
;;     (version "5.6.1")
;;     (source
;;      (origin
;;        (method url-fetch/zipbomb)
;;        (uri (string-append
;;              "https://www.synaptics.com/sites/default/files/exe_files/2022-08/DisplayLink%20USB%20Graphics%20Software%20for%20Ubuntu"
;;              version
;;              "-EXE.zip"))
;;        (sha256
;;         (base32
;;          "1hihsz35ccydzx04r8r9kz0hvqwj5fgr8zpzvwyhfxp2m549f9w9"))
;;        (file-name (string-append name "-" version ".zip"))))
;;     (supported-systems '("x86_64-linux"))
;;     (build-system binary-build-system)
;;     (inputs
;;      (list
;;       libusb
;;       glibc
;;       `(,gcc "lib")
;;       `(,util-linux "lib")
;;       ))
;;     (arguments
;;      (list
;;       #:validate-runpath? #f
;;       #:patchelf-plan
;;        #~'(("lib/DisplayLinkManager"
;;             ("util-linux"
;;              "gcc"
;;              "glibc"
;;              "libusb")))
;;        #:phases
;;        #~(modify-phases %standard-phases
;;          (add-after 'unpack 'unpack-runfile
;;            (lambda* _
;;              (let* ((lib (string-append #$output "/lib"))
;;                     (bin (string-append #$output "/bin"))
;;                     (src-file (car (find-files "." "\\.run$")))
;;                     (src-folder (string-drop-right src-file 4)))
;;                (invoke "sh" src-file "--keep" "--noexec")
;;                (rename-file (string-append src-folder "/") "lib")
;;                (copy-recursively "lib/aarch64-linux-gnu/" "lib/")
;;                (delete-file-recursively "lib/arm-linux-gnueabihf")
;;                (delete-file-recursively "lib/aarch64-linux-gnu")
;;                (delete-file-recursively "lib/x64-ubuntu-1604")
;;                (delete-file-recursively "lib/x86-ubuntu-1604")
;;                (delete-file-recursively "__MACOSX")
;;                (delete-file src-file)
;;                (delete-file (car (find-files "." "\\.txt$"))))))
;;          (add-after 'install 'symlink-binary
;;            (lambda _
;;              (mkdir-p (string-append #$output "/bin"))
;;              (symlink (string-append #$output "/lib/DisplayLinkManager")
;;                             (string-append #$output "/bin/DisplayLinkManager"))
;;              (invoke "ls" "-la" #$output)))

;;  ;; (add-after 'install 'wrap-where-patchelf-does-not-work
;;  ;;                 (lambda _
;;  ;;                   (wrap-program (string-append #$output "/lib/DisplayLinkManager")
;;  ;;                     `("LD_LIBRARY_PATH" ":" prefix
;;  ;;                       (,(string-join
;;  ;;                          (list
;;  ;;                           (string-append #$(this-package-input "gcc") "/lib")
;;  ;;                           (string-append #$output "/lib")
;;  ;;                           #$output)
;;  ;;                          ":"))))))


;;          )))
;;     (home-page "https://github.com/DisplayLink/evdi")
;;     (synopsis "EVDI Linux kernel module")
;;     (description
;;      "The @acronym{EVDI, Extensible Virtual Display Interface} is a Linux kernel module
;; that enables management of multiple screens, allowing
;; displaylink
;;  user-space programs to
;; take control over what happens with the image.")
;;     (license license:lgpl2.1)))
