(define-module (config features emacs-xyz)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde features emacs)
  #:use-module (config packages)
  #:export (feature-emacs-evil
            feature-emacs-leader-keys
            feature-emacs-files
            feature-emacs-syntax
            feature-emacs-corfu))

(define* (feature-emacs-corfu
          #:key
          (emacs-corfu emacs-corfu)
          (emacs-corfu-doc #f))
  "Basic CAPF configuration using corfu."
  
  (define emacs-f-name 'corfu)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load
         'corfu
         (setq corfu-auto t
               corfu-quit-no-match 'separator
               tab-always-indent 'complete))
        ,@(if emacs-corfu-doc
            '((with-eval-after-load
               'corfu-doc
               (define-key corfu-map (kbd "M-p") 'corfu-doc-scroll-down)
               (define-key corfu-map (kbd "M-n") 'corfu-doc-scroll-up)
               (define-key corfu-map (kbd "M-d") 'corfu-doc-toggle)))
            '()))
      #:elisp-packages (append
                        (list emacs-corfu)
                        (if emacs-corfu-doc
                            (list (get-value 'emacs-corfu-doc config))
                            '())))))
  (feature
   (name f-name)
   (values (append
            `((,f-name . ,emacs-corfu))
            (if emacs-corfu-doc
                `((emacs-corfu-doc . ,emacs-corfu-doc))
                '())))
   (home-services-getter get-home-services)))

(define* (feature-emacs-evil)
  "Configure evil-mode in Emacs."
  (define emacs-f-name 'evil)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'evil))
        (setq evil-want-keybinding nil
              evil-want-fine-undo t)
        (evil-mode 1)

        ;; Keybindings
        (with-eval-after-load
         'evil
         (evil-define-key 'normal prog-mode-map (kbd "<tab>") 'evil-jump-item)
         (define-key evil-normal-state-map (kbd "M-.") nil)
         (define-key evil-normal-state-map (kbd "M-,") nil)
         (define-key evil-normal-state-map (kbd "C-.") nil)

        ;; V for evil-visual-line in magit
        ,@(when (get-value 'emacs-git config)
            `((with-eval-after-load
	       'magit
	       (define-key magit-hunk-section-map (kbd "V") 'evil-visual-line))))

        ;; Start vterm in insert mode
        ,@(when (get-value 'emacs-vterm config)
            `((add-to-list 'evil-insert-state-modes 'vterm-mode)))))
      #:elisp-packages (list emacs-evil))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-files)
  "Configure leader and localleader keys"
  (define emacs-f-name 'files)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'tramp)
        (defun rde-delete-this-file (&optional path force-p)
          "Delete PATH, kill its buffers and expunge it from vc/magit cache.
If PATH is not specified, default to the current buffer's file.
If FORCE-P, delete without confirmation."
          (interactive
           (list (buffer-file-name (buffer-base-buffer))
                 current-prefix-arg))
          (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
                 (short-path (abbreviate-file-name path)))
            (unless (and path (file-exists-p path))
              (user-error "Buffer is not visiting any file"))
            (unless (file-exists-p path)
              (error "File doesn't exist: %s" path))
            (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
              (user-error "Aborted"))
            (let ((buf (current-buffer)))
              (unwind-protect
               (progn (delete-file path t) t)
               (if (file-exists-p path)
                   (error "Failed to delete %S" short-path)
                   (kill-this-buffer)
                   (message "Deleted %S" short-path))))))

        ;;; Source: https://github.com/hlissner/doom-emacs/blob/develop/core/autoload/files.el#L257
        (defun rde-copy-this-file (new-path &optional force-p)
          "Copy current buffer's file to NEW-PATH.
If FORCE-P, overwrite the destination file if it exists, without confirmation."
          (interactive
           (list (read-file-name "Copy file to: ")
                 current-prefix-arg))
          (unless (and buffer-file-name (file-exists-p buffer-file-name))
            (user-error "Buffer is not visiting any file"))
          (let ((old-path (buffer-file-name (buffer-base-buffer)))
                (new-path (expand-file-name new-path)))
            (make-directory (file-name-directory new-path) 't)
            (copy-file old-path new-path (or force-p 1))
            (message "File copied to %S" (abbreviate-file-name new-path))))

        ;;; Source: https://github.com/hlissner/doom-emacs/blob/develop/core/autoload/files.el#L274
        (defun rde-move-this-file (new-path &optional force-p)
          "Move current buffer's file to NEW-PATH.
If FORCE-P, overwrite the destination file if it exists, without confirmation."
          (interactive
           (list (read-file-name "Move file to: ")
                 current-prefix-arg))
          (unless (and buffer-file-name (file-exists-p buffer-file-name))
            (user-error "Buffer is not visiting any file"))
          (let ((old-path (buffer-file-name (buffer-base-buffer)))
                (new-path (expand-file-name new-path)))
            (when (directory-name-p new-path)
              (setq new-path (concat new-path (file-name-nondirectory old-path))))
            (make-directory (file-name-directory new-path) 't)
            (rename-file old-path new-path (or force-p 1))
            (set-visited-file-name new-path t t)
            (message "File moved to %S" (abbreviate-file-name new-path))))

        ;;; Source: https://github.com/hlissner/doom-emacs/blob/develop/core/autoload/files.el#L293
        (defun rde--sudo-file-path (file)
          (let ((host (or (file-remote-p file 'host) "localhost")))
            (concat "/" (when (file-remote-p file)
                          (concat (file-remote-p file 'method) ":"
                                  (if-let (user (file-remote-p file 'user))
                                          (concat user "@" host)
                                          host)
                                  "|"))
                    "sudo:root@" host
                    ":" (or (file-remote-p file 'localname)
                            file))))

        ;;; Source: https://github.com/hlissner/doom-emacs/blob/develop/core/autoload/files.el#L306
        (defun rde-sudo-find-file (file)
          "Open FILE as root."
          (interactive "FOpen file as root: ")
          (find-file (rde--sudo-file-path file)))

        ;;; Source: https://github.com/hlissner/doom-emacs/blob/develop/core/autoload/files.el#L312
        (defun rde-sudo-this-file ()
          "Open the current file as root."
          (interactive)
          (find-file
           (rde--sudo-file-path
            (or buffer-file-name
                (when (or (derived-mode-p 'dired-mode)
                          (derived-mode-p 'wdired-mode))
                  default-directory)))))

        (defun rde-yank-buffer-path ()
          "Copy the current buffer file name to the clipboard."
          (interactive)
          (let ((filename (if (equal major-mode 'dired-mode)
                              default-directory
                              (buffer-file-name))))
            (when filename
              (kill-new filename)
              (message "Copied buffer file name '%s' to the clipboard." filename))))
        
        (defvar rde-files-command-map
          (let ((map (make-sparse-keymap)))
            ;; C-x C-f -> find-file
            ;; C-x d -> dired
            (define-key map (kbd "d") '("Delete this file" . rde-delete-this-file))
            (define-key map (kbd "c") '("Copy this file" . rde-copy-this-file))
            (define-key map (kbd "m") '("Move this file" . rde-move-this-file))
            (define-key map (kbd "u") '("Sudo this file" . rde-sudo-this-file))
            (define-key map (kbd "U") '("Sudo find file" . rde-sudo-find-file))
            map)
          "Keymap for working with files.")
        (fset 'rde-files-command-map rde-files-command-map)
        (define-key global-map (kbd "C-c f") '("files" . rde-files-command-map)))
      #:elisp-packages
      (append (list (get-value 'emacs-configure-rde-keymaps config))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
