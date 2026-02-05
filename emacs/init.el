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

;; Core configuration (全部load)
(load (expand-file-name "core/env.el" dotfiles-emacs-dir))
(load (expand-file-name "core/custom.el" dotfiles-emacs-dir))

;; Package management
(load (expand-file-name "packages/manager.el" dotfiles-emacs-dir))

;; Custom file (secrets)
(setq custom-file (expand-file-name "config.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Core packages
(load (expand-file-name "packages/core.el" dotfiles-emacs-dir))

;; Utils
(load (expand-file-name "core/util.el" dotfiles-emacs-dir))

;; Packages (requireでOK)
(require 'completion)
(require 'search)
(require 'lsp)
(require 'languages)
(require 'ui)
(require 'git)
(require 'writing)
(require 'ai)
(require 'optional)

;; Font
(let ((font-config (expand-file-name "core/font.el" dotfiles-emacs-dir)))
  (when (file-exists-p font-config)
    (load font-config)))

;; Keymap (最後にload)
(load (expand-file-name "core/keymap.el" dotfiles-emacs-dir))

(provide 'init)
;;; init.el ends here
