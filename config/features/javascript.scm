(define-module (config features javascript)
  ;;#:use-module (config packages node-xyz)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages node)
  #:use-module (config packages node-xyz)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (guix gexp)
  #:use-module (ice-9 pretty-print)
  #:export (feature-javascript))

(define* (feature-javascript
          #:key
          (typescript #f)
          (typescript-language-server #f)
          (eslint-language-server #f))
  "Setup and configure environment for JavaScript."
  (ensure-pred maybe-file-like? typescript)
  (ensure-pred maybe-file-like? typescript-language-server)
  (ensure-pred maybe-file-like? eslint-language-server)

  (define (get-home-services config)
    (define emacs-f-name 'javascript)
    (define tsserver-library
      (if (any-package? typescript)
          (file-append typescript "/lib/")
          typescript))
    (define ts-lsp-executable
      (if (any-package? typescript-language-server)
          (file-append typescript-language-server
                       "/bin/typescript-language-server")
          typescript-language-server))
    (define eslint-lsp-executable
      (if (any-package? eslint-language-server)
          (file-append eslint-language-server
                       "/bin/vscode-eslint-language-server")
          eslint-language-server))
    (list
     (when (get-value 'emacs config)
       (rde-elisp-configuration-service
        emacs-f-name
        config
        `(;;; TODO: Remove after https://github.com/joaotavora/eglot/pull/871 is merged.
          (eval-when-compile (require 'eglot))
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
           ;;(add-hook 'js2-minor-mode-hook 'js2-refactor-mode)
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

          ;; npm-mode
          (with-eval-after-load
           'npm-mode
           (fset 'npm-mode-command-keymap npm-mode-command-keymap)
           (define-key npm-mode-keymap (kbd "C-c n") '("npm" . npm-mode-command-keymap)))

          ;; js2-refactor-el
          ;; TODO: add js2r keybindings
          ;; (js2r-add-keybindings-with-prefix "C-c r")
          ;; (define-key js2-refactor-mode-map (kbd "C-c r f") 'js2r-extract-function)

          (with-eval-after-load
           'eglot
           ;;; TODO: Remove after https://github.com/joaotavora/eglot/pull/871 is merged.
           (eglot--code-action eglot-code-action-organize-imports-ts "source.organizeImports.ts")
           (eglot--code-action eglot-code-action-add-missing-imports-ts "source.addMissingImports.ts")
           (eglot--code-action eglot-code-action-removed-unused-ts "source.removedUnused.ts")
	   (eglot--code-action eglot-code-action-fix-all-ts "source.fixAll.ts")
	   ,#~(add-to-list 'eglot-server-programs
                        '((js-mode
                           typescript-mode
                           typescript-tsx-mode) . ,(eglot-alternatives '((#$ts-lsp-executable
                                                                         "--tsserver-path" #$tsserver-library
                                                                         "--stdio")
                                                                        (#$eslint-lsp-executable
                                                                         "--stdio"))))))
          (dolist (hook
                   '(js-mode-hook
                     typescript-mode-hook
                     typescript-tsx-mode-hook))
                  (add-hook hook
                            (lambda ()
                              (eglot-ensure)
                              (local-set-key (kbd "C-c c i") '("Add missing imports" . eglot-code-action-add-missing-imports-ts))
                              (local-set-key (kbd "C-c c o") '("Organize imports" . eglot-code-action-organize-imports-ts))
                              (local-set-key (kbd "C-c c r") '("Remove unused symbols" . eglot-code-action-removed-unused-ts))
                              (local-set-key (kbd "C-c c f") '("Fix all" . eglot-code-action-fix-all-ts))))))
        #:elisp-packages (list emacs-js2-mode ;;emacs-js2-refactor-el
                               emacs-typescript-mode
                               emacs-npm-mode
                               emacs-web-mode
                               (get-value 'emacs-eglot config emacs-eglot))))))

  (feature
   (name 'javascript)
   (values `((javascript . #t)))
   (home-services-getter get-home-services)))
