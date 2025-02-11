;; 必要に応じてロードパスを追加
;; (add-to-list 'load-path "/path/to/your/lisp/files")
(add-to-list 'load-path "~/dotfiles/2024_emacs/lib")

;; 必要に応じてrequireを追加
;; 環境系
(load "env.el")
;; パッケージ系
(load "mypackage.el")
;; システム系、パッケージ読み込み前になにかしたいときとか
(load "custom.el")
;; my config
(load "util.el")
;; キーマップ系
(load "keymap.el")
;; font
(when (file-exists-p "~/dotfiles/2024_emacs/lib/font.el")
  (load "font.el"))

;; 必要に応じてカスタム設定を追
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((eglot-x :vc-backend Git :url "https://github.com/nemethf/eglot-x")
     (eglot-booster :vc-backend Git :url
		    "https://github.com/jdtsmith/eglot-booster")
     (breadcrumb :vc-backend Git :url
		 "https://github.com/joaotavora/breadcrumb")
     (vertico-truncate :vc-backend Git :url
		       "https://github.com/jdtsmith/vertico-truncate")
     (nerd-icons-corfu :vc-backend Git :url
		       "https://github.com/LuigiPiucco/nerd-icons-corfu")
     (vc-use-package :vc-backend Git :url
		     "https://github.com/slotThe/vc-use-package"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:foreground "red" :height 4.0)))))
