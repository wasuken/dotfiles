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


(defun dotfiles--load-if-exists (path)
  (when (file-exists-p path)
    (load path)))

;; Font
(dotfiles--load-if-exists (expand-file-name "core/font.el" dotfiles-emacs-dir))

;; Keymap (最後にload)
(load (expand-file-name "core/keymap.el" dotfiles-emacs-dir))

;; 環境にいれないもの
(dotfiles--load-if-exists (expand-file-name "core/local.el" dotfiles-emacs-dir))

(provide 'init)
;;; init.el ends here
