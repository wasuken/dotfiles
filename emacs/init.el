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

;; Packages (全部load)
(load (expand-file-name "packages/completion.el" dotfiles-emacs-dir))
(load (expand-file-name "packages/search.el" dotfiles-emacs-dir))
(load (expand-file-name "packages/lsp.el" dotfiles-emacs-dir))
(load (expand-file-name "packages/languages.el" dotfiles-emacs-dir))
(load (expand-file-name "packages/ui.el" dotfiles-emacs-dir))
(load (expand-file-name "packages/git.el" dotfiles-emacs-dir))
(load (expand-file-name "packages/writing.el" dotfiles-emacs-dir))
(load (expand-file-name "packages/ai.el" dotfiles-emacs-dir))
(load (expand-file-name "packages/optional.el" dotfiles-emacs-dir))

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

(provide 'init)
;;; init.el ends here
