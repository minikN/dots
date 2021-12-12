(define-module (features emacs-xyz)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde features emacs)
  
  #:export (feature-emacs-evil)
  #:export (feature-emacs-leader-keys)
  #:export (feature-emacs-files)
  #:export (feature-emacs-syntax))

(define* (feature-emacs-evil
          #:key
          (collection? #t))
  "Configure evil-mode in Emacs."
  (define emacs-f-name 'evil)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (ensure-pred boolean? collection?)

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((setq evil-want-keybinding nil
              evil-want-fine-undo t)
        (require 'evil)
        (evil-mode 1)

        ;; Keybindings
        (evil-define-key 'normal prog-mode-map (kbd "<tab>") 'evil-jump-item)

        ,@(when collection?
          `((require 'evil-collection))))
      #:elisp-packages (append
                        (if collection?
                            (list emacs-evil-collection)
                            '())
                        (list emacs-evil)))))
  
  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-syntax)
  "Configure multiple packages for working better
with Emacs as an editor."
  (define emacs-f-name 'syntax)
  (define f-name (symbol-append 'emacs- emacs-f-name))
  
  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `(;; smartparens
        (require 'smartparens-config)
        (add-hook 'prog-mode-hook 'smartparens-mode)
        (define-key smartparens-mode-map (kbd "M-<up>") 'sp-backward-up-sexp)
        (define-key smartparens-mode-map (kbd "M-<down>") 'sp-down-sexp)
        (define-key smartparens-mode-map (kbd "M-<left>") 'sp-backward-sexp)
        (define-key smartparens-mode-map (kbd "M-<right>") 'sp-next-sexp)
        

        ;; smart-hungry-delete
        (require 'smart-hungry-delete)
        (smart-hungry-delete-add-default-hooks)
        (global-set-key (kbd "M-<backspace>") 'smart-hungry-delete-backward-char)
        (global-set-key (kbd "M-<delete>") 'smart-hungry-delete-forward-char))
      #:elisp-packages (list emacs-smartparens
                             emacs-smart-hungry-delete))))

 (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-leader-keys
          #:key
          (leader "C-SPC")
          (localleader "C-SPC C-SPC"))
  "Configure leader and localleader keys"
  (define emacs-f-name 'leader-keys)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((setq rde-leader ,leader)
        (setq rde-localleader ,localleader)
        (define-prefix-command 'rde-global-map)
        (define-key global-map (kbd rde-leader) 'rde-global-map)))))

 (feature
   (name f-name)
   (values `((emacs-leader . ,leader)
             (emacs-localleader . ,localleader)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-files
          #:key
          (keybindings? #t))
  "Configure leader and localleader keys"
  (ensure-pred boolean? keybindings?)
  
  (define emacs-f-name 'files)
  (define f-name (symbol-append 'emacs- emacs-f-name))


  (define (get-home-services config)
    (when keybindings? (require-value 'emacs-leader config))
    (list
     (elisp-configuration-service
      emacs-f-name
      `((with-eval-after-load
         ;;; TODO: Improve this. We need to load feature-emacs-leader-keys
         ;;; first, otherwise we'll get a non-prefix key error.
         'configure-leader-keys-autoloads
         ,@(when keybindings?
            `((let ((rde-leader ,(get-value 'emacs-leader config)))
                (define-key rde-global-map "f" '("Find file" . find-file))))))))))
  
  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

;;         (defun rde/delete-this-file (&optional path force-p)
;;           "Delete PATH, kill its buffers and expunge it from vc/magit cache.
;; If PATH is not specified, default to the current buffer's file.
;; If FORCE-P, delete without confirmation."
;;           (interactive
;;            (list (buffer-file-name (buffer-base-buffer))
;;                  current-prefix-arg))
;;           (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
;;                  (short-path (abbreviate-file-name path)))
;;             (unless (and path (file-exists-p path))
;;               (user-error "Buffer is not visiting any file"))
;;             (unless (file-exists-p path)
;;               (error "File doesn't exist: %s" path))
;;             (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
;;               (user-error "Aborted"))
;;             (let ((buf (current-buffer)))
;;               (unwind-protect
;;                (progn (delete-file path t) t)
;;                (if (file-exists-p path)
;;                    (error "Failed to delete %S" short-path)
;;                    (kill-this-buffer)
;;                    (message "Deleted %S" short-path))))))

;;         ;;; Source: https://github.com/hlissner/doom-emacs/blob/develop/core/autoload/files.el#L257
;;         (defun rde/copy-this-file (new-path &optional force-p)
;;           "Copy current buffer's file to NEW-PATH.
;; If FORCE-P, overwrite the destination file if it exists, without confirmation."
;;           (interactive
;;            (list (read-file-name "Copy file to: ")
;;                  current-prefix-arg))
;;           (unless (and buffer-file-name (file-exists-p buffer-file-name))
;;             (user-error "Buffer is not visiting any file"))
;;           (let ((old-path (buffer-file-name (buffer-base-buffer)))
;;                 (new-path (expand-file-name new-path)))
;;             (make-directory (file-name-directory new-path) 't)
;;             (copy-file old-path new-path (or force-p 1))
;;             (message "File copied to %S" (abbreviate-file-name new-path))))

;;         ;;; Source: https://github.com/hlissner/doom-emacs/blob/develop/core/autoload/files.el#L274
;;         (defun rde/move-this-file (new-path &optional force-p)
;;           "Move current buffer's file to NEW-PATH.
;; If FORCE-P, overwrite the destination file if it exists, without confirmation."
;;           (interactive
;;            (list (read-file-name "Move file to: ")
;;                  current-prefix-arg))
;;           (unless (and buffer-file-name (file-exists-p buffer-file-name))
;;             (user-error "Buffer is not visiting any file"))
;;           (let ((old-path (buffer-file-name (buffer-base-buffer)))
;;                 (new-path (expand-file-name new-path)))
;;             (when (directory-name-p new-path)
;;               (setq new-path (concat new-path (file-name-nondirectory old-path))))
;;             (make-directory (file-name-directory new-path) 't)
;;             (rename-file old-path new-path (or force-p 1))
;;             (set-visited-file-name new-path t t)
;;             (message "File moved to %S" (abbreviate-file-name new-path))))

;;         ;;; Source: https://github.com/hlissner/doom-emacs/blob/develop/core/autoload/files.el#L293
;;         (defun rde--sudo-file-path (file)
;;           (let ((host (or (file-remote-p file 'host) "localhost")))
;;             (concat "/" (when (file-remote-p file)
;;                           (concat (file-remote-p file 'method) ":"
;;                                   (if-let (user (file-remote-p file 'user))
;;                                           (concat user "@" host)
;;                                           host)
;;                                   "|"))
;;                     "sudo:root@" host
;;                     ":" (or (file-remote-p file 'localname)
;;                             file))))

;;         ;;; Source: https://github.com/hlissner/doom-emacs/blob/develop/core/autoload/files.el#L306
;;         (defun rde/sudo-find-file (file)
;;           "Open FILE as root."
;;           (interactive "FOpen file as root: ")
;;           (find-file (rde--sudo-file-path file)))

;;         ;;; Source: https://github.com/hlissner/doom-emacs/blob/develop/core/autoload/files.el#L312
;;         (defun rde/sudo-this-file ()
;;           "Open the current file as root."
;;           (interactive)
;;           (find-file
;;            (rde--sudo-file-path
;;             (or buffer-file-name
;;                 (when (or (derived-mode-p 'dired-mode)
;;                           (derived-mode-p 'wdired-mode))
;;                   default-directory)))))

;;         (defun rde/yank-buffer-path ()
;;           "Copy the current buffer file name to the clipboard."
;;           (interactive)
;;           (let ((filename (if (equal major-mode 'dired-mode)
;;                               default-directory
;;                               (buffer-file-name))))
;;             (when filename
;;               (kill-new filename)
;;               (message "Copied buffer file name '%s' to the clipboard." filename))))
