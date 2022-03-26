(define-module (personal features emacs-xyz)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde features emacs)
  #:use-module (personal packages)

  #:export (feature-emacs-evil)
  #:export (feature-emacs-leader-keys)
  #:export (feature-emacs-files)
  #:export (feature-emacs-syntax)
  #:export (feature-emacs-lang-base)
  #:export (feature-emacs-lang-javascript)
  )


(define* (feature-emacs-lang-base)
  "Configure Emacs for programming."
  (define emacs-f-name 'lang-base)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define emacs-configure-lsp
    ((@@ (rde features emacs) rde-emacs-configuration-package)
     'rde-lsp
     `((eval-when-compile (require 'eglot))
       ;; FIXME: Rethink keybinding structure
       (add-hook 'eglot--managed-mode-hook
                 (lambda ()
                   (local-set-key (kbd (concat rde-leader "  c")) '("Code" . rde-lsp-map))))
       (define-prefix-command 'rde-lsp-map nil "Code")
       (with-eval-after-load
        'eglot
        (define-key rde-lsp-map (kbd "S") '("Start" . eglot))
        (define-key rde-lsp-map (kbd "R") '("Reconnect" . eglot-reconnect))
        (define-key rde-lsp-map (kbd "k") '("Shutdown" . eglot-shutdown))
        (define-key rde-lsp-map (kbd "K") '("Shutdown (all)" . eglot-shutdown-all))
        (define-key rde-lsp-map (kbd "r") '("Rename symbol" . eglot-rename))
        (define-key rde-lsp-map (kbd "f") '("Format buffer or region" . eglot-format))
        (define-key rde-lsp-map (kbd "c") '("Code actions" . eglot-code-actions))
        (define-key rde-lsp-map (kbd "h") '("Help" . eldoc))))
     #:elisp-packages (list emacs-eglot)
     #:summary "Basic LSP configuration using eglot."))

  (define emacs-configure-capf
    ((@@ (rde features emacs) rde-emacs-configuration-package)
     'rde-capf
     `((eval-when-compile (require 'corfu))
       (with-eval-after-load
        'corfu
        (setq corfu-auto t
              corfu-quit-no-match 'separator
              tab-always-indent 'complete)))
     #:elisp-packages (list emacs-corfu)
     #:summary "Basic CAPF configuration using corfu."))

  (define emacs-configure-capf-doc
    ((@@ (rde features emacs) rde-emacs-configuration-package)
     'rde-capf-doc
     `((eval-when-compile (require 'corfu-doc))
       (with-eval-after-load
        'corfu-doc
        (define-key corfu-map (kbd "M-p") 'corfu-doc-scroll-down)
        (define-key corfu-map (kbd "M-n") 'corfu-doc-scroll-up)
        (define-key corfu-map (kbd "M-d") 'corfu-doc-toggle)))
       #:elisp-packages (list emacs-corfu-doc)
     #:summary "Basic configuration for showing CAPF docs using corfu-doc."))

  (feature
   (name f-name)
   (values (make-feature-values
            emacs-configure-lsp
            emacs-configure-capf
            emacs-configure-capf-doc))))

(define* (feature-emacs-lang-javascript
          #:key
          (lsp? #t)
          (capf? #t)
          (doc? #t))
  "Configure Emacs for JavaScript and TypeScript."
  (ensure-pred boolean? lsp?)
  (ensure-pred boolean? capf?)
  (ensure-pred boolean? doc?)

  (define emacs-f-name 'lang-javascript)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (let* ((leader (get-value 'rde-leader config))
           (localleader (get-value 'rde-localleader config)))
      (list
       (elisp-configuration-service
        emacs-f-name
        `((eval-when-compile
           (require 'js)
           (require 'js2-mode)
           (require 'typescript-mode)
           (require 'npm-mode))

          ;; electric-pair-mode
          (defun rde--setup-electric-pairs-for-jsx-tsx ()
            (electric-pair-local-mode)
            (setq-local electric-pair-pairs
                        (append electric-pair-pairs
                                '((60 . 62)))) ;; <, >
            (setq-local electric-pair-text-pairs electric-pair-pairs))

          ;; js-mode
          (with-eval-after-load
           'js
           (add-hook 'js-mode-hook
                     (lambda ()
                       (rde--setup-electric-pairs-for-jsx-tsx)
                       (js2-minor-mode)
                       (npm-mode))))

          ;; js2-mode
          (with-eval-after-load
           'js2-mode
           ;; (add-hook 'js2-minor-mode-hook 'js2-refactor-mode)
           (setq js-chain-indent t
                 js2-basic-offset 2
                 js2-skip-preprocessor-directives t
                 js2-mode-show-parse-errors nil
                 js2-mode-show-strict-warnings nil
                 js2-strict-missing-semi-warning nil
                 js2-highlight-level 3
                 js2-idle-timer-delay 0.15))

          ;; typescript-mode
          (with-eval-after-load
           'typescript-mode
           (add-hook 'typescript-mode-hook 'npm-mode)
           (setq typescript-indent-level 2))

          ;; typescript-tsx-mode
          (when (fboundp 'web-mode)
            (define-derived-mode typescript-tsx-mode web-mode "TypeScript[TSX]")
            (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode)))
          (with-eval-after-load
           'web-mode
           (setq web-mode-markup-indent-offset 2
                 web-mode-css-indent-offset 2
                 web-mode-code-indent-offset 2)
           (add-hook 'typescript-tsx-mode-hook
                     (lambda ()
                       (rde--setup-electric-pairs-for-jsx-tsx)
                       (npm-mode))))

          ;; Keybindings
          ;; FIXME: Rethink keybinding structure
          (require 'configure-rde-keymaps)

          ;; js2-refactor-el
          (add-hook
           'js2-refactor-mode-hook
           ,@(when localleader
               `((lambda () (js2r-add-keybindings-with-prefix (concat ,localleader " j"))))))

          (add-hook
           'npm-mode-hook
           (lambda ()
             (define-prefix-command 'rde-npm-map nil "NPM")
             (define-key 'rde-npm-map (kbd "d") '("Install (--save-dev)" . npm-mode-npm-install-save-dev))
             (define-key 'rde-npm-map (kbd "i") '("Install" . npm-mode-npm-install))
             (define-key 'rde-npm-map (kbd "l") '("List" . npm-mode-npm-list))
             (define-key 'rde-npm-map (kbd "n") '("Init" . npm-mode-npm-init))
             (define-key 'rde-npm-map (kbd "r") '("Run" . npm-mode-npm-run))
             (define-key 'rde-npm-map (kbd "s") '("Install (--save)" . npm-mode-npm-install-save))
             (define-key 'rde-npm-map (kbd "u") '("Uninstall" . npm-mode-npm-uninstall))
             (define-key 'rde-npm-map (kbd "v") '("Visit package.json" . npm-mode-visit-project-file))
             ,@(when localleader
                 `((local-set-key (kbd (concat ,localleader " n")) '("NPM" . rde-npm-map))))))

          ,@(when lsp? ;; eglot configuration
              `((require 'configure-rde-lsp)
                (eval-when-compile (require 'eglot))
                (with-eval-after-load
                 'eglot
		 (add-to-list 'eglot-server-programs
                              '(typescript-tsx-mode . ("typescript-language-server" "--stdio"))))
                (dolist (hook
                         '(js-mode-hook
                           typescript-mode-hook
                           typescript-tsx-mode-hook))
                        (add-hook hook
                                  (lambda ()
                                    (eglot-ensure)
                                    (eglot--code-action eglot-code-action-organize-imports-ts "source.organizeImports.ts")
                                    (eglot--code-action eglot-code-action-add-missing-imports-ts "source.addMissingImports.ts")
                                    (eglot--code-action eglot-code-action-removed-unused-ts "source.removedUnused.ts")
		                    (eglot--code-action eglot-code-action-fix-all-ts "source.fixAll.ts")
                                    ,@(when localleader
                                        `((local-set-key (kbd (concat ,localleader " i")) '("Add missing imports" . eglot-code-action-add-missing-imports-ts))
                                          (local-set-key (kbd (concat ,localleader " o")) '("Organize imports" . eglot-code-action-organize-imports-ts))
                                          (local-set-key (kbd (concat ,localleader " r")) '("Remove unused symbols" . eglot-code-action-removed-unused-ts))
                                          (local-set-key (kbd (concat ,localleader " f")) '("Fix all" . eglot-code-action-fix-all-ts)))))))))

          ,@(when capf? ;; corfu configuration
              `((require 'configure-rde-capf)
                (add-hook 'js-mode-hook 'corfu-mode)
                (add-hook 'typescript-mode-hook 'corfu-mode)
                (add-hook 'typescript-tsx-mode-hook 'corfu-mode)
                ,@(when doc?
                    `((require 'configure-rde-capf-doc)
                      (add-hook 'js-mode-hook 'corfu-doc-mode)
                      (add-hook 'typescript-mode-hook 'corfu-doc-mode)
                      (add-hook 'typescript-tsx-mode-hook 'corfu-doc-mode))))))
        #:elisp-packages (list emacs-js2-mode
                               ;; emacs-js2-refactor-el
                               emacs-typescript-mode
                               emacs-npm-mode
                               emacs-web-mode ;; TODO: Move to emacs-feature-lang-web?
                               (when lsp? (get-value 'emacs-configure-lsp config))
                               (when capf? (get-value 'emacs-configure-capf config))
                               (when doc? (get-value 'emacs-configure-capf-doc config))
                               (get-value 'emacs-configure-rde-keymaps config))))))
  (feature
   (name f-name)
 (values `((,f-name . #t)))
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
      `((eval-when-compile (require 'smartparens-config))
        (add-hook 'prog-mode-hook 'smartparens-mode)
        (with-eval-after-load
         'smartparens
         (define-key smartparens-mode-map (kbd "M-<up>") 'sp-backward-up-sexp)
         (define-key smartparens-mode-map (kbd "M-<down>") 'sp-down-sexp)
         (define-key smartparens-mode-map (kbd "M-<left>") 'sp-backward-sexp)
         (define-key smartparens-mode-map (kbd "M-<right>") 'sp-next-sexp))

        ;; smart-hungry-delete
        (eval-when-compile (require 'smart-hungry-delete))
        (smart-hungry-delete-add-default-hooks)
        (normal-erase-is-backspace-mode 0)
        (define-key prog-mode-map (kbd "M-DEL") 'smart-hungry-delete-forward-char)
        (define-key prog-mode-map (kbd "M-<delete>") 'smart-hungry-delete-backward-char))
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
       (defconst rde-leader ,leader "RDE leader key.")
       (defconst rde-localleader ,localleader "RDE localleader key.")

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
 (values `((emacs-leader-keys . #t)
           (rde-leader . ,leader)
           (rde-localleader . ,localleader)))
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
       (define-key 'rde-files (kbd "c") '("Copy this file" . rde-copy-this-file))
       (define-key 'rde-files (kbd "m") '("Move this file" . rde-move-this-file))
       (define-key 'rde-files (kbd "u") '("Sudo this file" . rde-sudo-this-file))
       (define-key 'rde-files (kbd "U") '("Sudo find file" . rde-sudo-find-file))))
    #:elisp-packages
    (append (list (get-value 'emacs-configure-rde-keymaps config))))))

(feature
 (name f-name)
 (values `((,f-name . #t)))
 (home-services-getter get-home-services)))
