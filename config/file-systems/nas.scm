(define-module (config file-systems nas)
  #:use-module (gnu system file-systems)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)

  #:use-module (guix gexp)

  #:export (%nas-filesystems
            %nas-mount-services))

(define (cifs-mount-service mount-point)
  (simple-service
   'mount-cifs-after-networking shepherd-root-service-type
   (list (shepherd-service
          (requirement '(networking))
          (provision `(,(symbol-append 'cifs-mount- (string->symbol mount-point))))
          (documentation "Mount CIFS share after networking.")
          (start (let ((util-linux (@ (gnu packages linux) util-linux)))
                   #~(lambda _
                       (system* #$(file-append util-linux "/bin/mount")
                                #$mount-point)
                       #t)))
          (stop #~(lambda _
                    (chdir "/")
                    (umount #$mount-point)
                    #f))))))

(define %nas-filesystems
  (list
   (file-system
    (device "//192.168.178.26/Temp")
    (mount-point "/media/nas/temp")
    (mount? #f)
    (check? #f)
    (mount-may-fail? #t)
    (type "cifs")
    (options "rw,credentials=/etc/guix/smb-credentials,uid=1000,gid=998")
    (create-mount-point? #t))
   (file-system
    (device "//192.168.178.26/audiobooks")
    (mount-point "/media/nas/audiobooks")
    (mount? #f)
    (check? #f)
    (mount-may-fail? #t)
    (type "cifs")
    (options "rw,credentials=/etc/guix/smb-credentials,uid=1000,gid=998")
    (create-mount-point? #t))
   (file-system
    (device "//192.168.178.26/movies")
    (mount-point "/media/nas/movies")
    (mount? #f)
    (check? #f)
    (mount-may-fail? #t)
    (type "cifs")
    (options "rw,credentials=/etc/guix/smb-credentials,uid=1000,gid=998")
    (create-mount-point? #t))
   (file-system
    (device "//192.168.178.26/shows")
    (mount-point "/media/nas/shows")
    (mount? #f)
    (check? #f)
    (mount-may-fail? #t)
    (type "cifs")
    (options "rw,credentials=/etc/guix/smb-credentials,uid=1000,gid=998")
    (create-mount-point? #t))
   (file-system
    (device "//192.168.178.26/music")
    (mount-point "/media/nas/music")
    (mount? #f)
    (check? #f)
    (mount-may-fail? #t)
    (type "cifs")
    (options "rw,credentials=/etc/guix/smb-credentials,uid=1000,gid=998")
    (create-mount-point? #t))))

(define %nas-mount-services
  (list (cifs-mount-service "/media/nas/temp")
        (cifs-mount-service "/media/nas/audiobooks")
        (cifs-mount-service "/media/nas/movies")
        (cifs-mount-service "/media/nas/shows")
        (cifs-mount-service "/media/nas/music")))
