;;; init.el --- Wasu's Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern Emacs configuration

;;; Code:

;; package.elを無効化
(setq package-enable-at-startup nil)

;; Load path
(defvar dotfiles-emacs-dir (expand-file-name "~/dotfiles/emacs/"))

(add-to-list 'load-path (expand-file-name "core" dotfiles-emacs-dir))
(add-to-list 'load-path (expand-file-name "packages" dotfiles-emacs-dir))

;; Core configuration
;; `env' and `custom' are already provided by Emacs itself, so `require'
;; would skip these local configuration files.
(load (expand-file-name "core/env.el" dotfiles-emacs-dir))
(load (expand-file-name "core/custom.el" dotfiles-emacs-dir))

;; Package management
(require 'manager)

;; Custom file (secrets)
(setq custom-file (expand-file-name "config.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Core packages
(require 'core)

;; Utils
(require 'util)

;; Packages (keep this order because later configuration refers to earlier setup)
(require 'completion)
(require 'search)
(require 'lsp)
(require 'languages)
(require 'ui)
(require 'git)
(require 'writing)
(require 'ai)
(require 'optional)
(require 'code-reading)

;; Font
(let ((font-config (expand-file-name "core/font.el" dotfiles-emacs-dir)))
  (when (file-exists-p font-config)
    (load font-config)))

;; Keymap (最後にload)
(load (expand-file-name "core/keymap.el" dotfiles-emacs-dir))

;; (add-to-list 'load-path "~/dotfiles/emacs/mypackages/live-comments")
;; (require 'live-comments)
;; (setq live-comments-gemini-api-key gemini-api-key)
;; (load (expand-file-name "mypackages/live-comments/live-comments.el" dotfiles-emacs-dir))
(let ((pkg-dir (expand-file-name "mypackages" dotfiles-emacs-dir)))
  (dolist (pkg (directory-files pkg-dir t "^[^.]"))
    (when (file-directory-p pkg)
      (add-to-list 'load-path pkg))))

(require 'code-watch)

(provide 'init)
;;; init.el ends here
