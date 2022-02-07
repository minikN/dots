(define-module (personal features emacs-xyz)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde features emacs)

  #:export (feature-emacs-evil)
  #:export (feature-emacs-leader-keys)
  #:export (feature-emacs-files)
  #:export (feature-emacs-syntax)
  #:export (feature-emacs-lang-base)
  #:export (feature-emacs-lang-javascript)
  )

(define* (feature-emacs-lang-base
          #:key
          (lsp? #t))
  "Configure Emacs for programming."
  (ensure-pred boolean? lsp?)
  (define emacs-f-name 'lang-base)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((eval-when-compile
         ,@(if lsp?
               `((setq lsp-enable-folding nil
                       lsp-enable-text-document-color nil
                       lsp-enable-on-type-formatting nil
                       lsp-headerline-breadcrumb-enable nil)
                 (require 'lsp-mode))
               ;; TODO: Add consult-lsp if emacs-consult is enabled.
               ;; TODO: Add alternative to lsp-mode.
               '())))
      #:elisp-packages (append (if lsp?
                                   (list emacs-lsp-mode)
                                   '())))))
  (feature
   (name f-name)
   (values `((,f-name . #t)
             (lsp ' lsp?)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-lang-javascript)
  "Configure Emacs for javascript."
  (define emacs-f-name 'lang-javascript)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (let* ((lsp (get-value 'lsp config)))
      (list
       (elisp-configuration-service
        emacs-f-name
        `((with-eval-after-load
           'js
           (add-hook 'js-mode-hook 'js2-minor-mode)
           ,@(if lsp
                 `((add-hook 'js-mode-hook 'lsp))
                 '()))
          (with-eval-after-load
           'js2-mode
           (add-hook 'js2-minor-mode-hook 'js2-refactor-mode)
           (setq js-chain-indent t
                 js2-basic-offset 2
                 js2-skip-preprocessor-directives t
                 js2-mode-show-parse-errors nil
                 js2-mode-show-strict-warnings nil
                 js2-strict-missing-semi-warning nil
                 js2-highlight-level 3
                 js2-idle-timer-delay 0.15))
          (define-derived-mode typescript-tsx-mode web-mode "TypeScript-tsx")
          (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
          (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
          (with-eval-after-load
           'typescript-mode
           ,@(if lsp
                 `((add-hook 'typescript-mode-hook 'lsp))
                 '()))
          (with-eval-after-load
           'web-mode
           ,@(if lsp
                 `((add-hook 'typescript-tsx-mode-hook 'lsp))
                 '())))
        #:elisp-packages (list emacs-js2-mode
                               emacs-js2-refactor-el
                               emacs-typescript-mode
                               emacs-web-mode ;; TODO: Move to emacs-feature-lang-web?
                               )))))
  (feature
   (name f-name)
   (values `((,f-name . #t)
             (typescript ' typescript?)))
   (home-services-getter get-home-services)))


(define* (feature-emacs-evil)
  "Configure evil-mode in Emacs."
  (define emacs-f-name 'evil)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((eval-when-compile
         (require 'evil)
         (require 'evil-collection))
        (setq evil-want-keybinding nil
              evil-want-fine-undo t)
        (evil-mode 1)

        ;; Keybindings
        (evil-define-key 'normal prog-mode-map (kbd "<tab>") 'evil-jump-item)

        ;; V for evil-visual-line in magit
        ,@(when (get-value 'emacs-git config)
            `((with-eval-after-load
	       'magit
	       (define-key magit-hunk-section-map (kbd "V") 'evil-visual-line)))))
      #:elisp-packages (list emacs-evil
                             emacs-evil-collection))))

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
      `((eval-when-compile
         ;; smartparens
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
        (global-set-key (kbd "M-<delete>") 'smart-hungry-delete-forward-char)))
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
      `((eval-when-compile (require 'configure-rde-keymaps))
        (with-eval-after-load
         'configure-rde-keymaps
         (defvar rde-global nil "Global rde keymap.")
         (defvar rde-local nil "Local rde keymap.")

         ;; Define the global map (C-SPC)
         (define-prefix-command 'rde-global nil "Global")
         (define-key global-map (kbd ,leader) 'rde-global)

         ;; Define the local map (C-SPC C-SPC)
         (define-prefix-command 'rde-local nil "Local")
         (define-key global-map (kbd ,localleader) 'rde-local)

         ;; Assign default keymaps
         (define-key rde-global (kbd "a") 'rde-app-map)
         (define-key rde-global (kbd "t") 'rde-toggle-map)))
      #:elisp-packages
      (append (list (get-value 'emacs-configure-rde-keymaps config))))))

  (feature
   (name f-name)
   (values `((emacs-leader-keys . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-files)
  "Configure leader and localleader keys"
  (define emacs-f-name 'files)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((eval-when-compile
         (require 'configure-rde-keymaps))
        (with-eval-after-load
         'configure-leader-keys
         (require 'tramp)
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

         (defun rde/yank-buffer-path ()
           "Copy the current buffer file name to the clipboard."
           (interactive)
           (let ((filename (if (equal major-mode 'dired-mode)
                               default-directory
                               (buffer-file-name))))
             (when filename
               (kill-new filename)
               (message "Copied buffer file name '%s' to the clipboard." filename))))
         (define-prefix-command 'rde-files nil "Files")
         (define-key 'rde-global (kbd "f") '("Files" . rde-files))
         (define-key 'rde-global (kbd "C-f") '("Find file" . find-file))
         (define-key 'rde-files (kbd "d") '("Open dired" . dired))
         (define-key 'rde-files (kbd "D") '("Delete this file" . rde-delete-this-file))
         (define-key 'rde-files (kbd "c") '("Copt this file" . rde-copy-this-file))
         (define-key 'rde-files (kbd "m") '("Move this file" . rde-move-this-file))
         (define-key 'rde-files (kbd "m") '("Move this file" . rde-move-this-file))
         (define-key 'rde-files (kbd "u") '("Sudo this file" . rde-sudo-this-file))
         (define-key 'rde-files (kbd "U") '("Sudo find file" . rde-sudo-find-file))))
      #:elisp-packages
      (append (list (get-value 'emacs-configure-rde-keymaps config))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
