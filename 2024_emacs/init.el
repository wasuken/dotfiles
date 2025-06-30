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
 '(package-selected-packages
   '(ace-jump-mode ace-window aggressive-indent auto-compile
		   auto-package-update beframe bind-key breadcrumb
		   cape cargo cl-generic cl-lib consult-eglot
		   corfu-prescient corfu-terminal dashboard ddskk
		   ddskk-posframe diff-hl difftastic
		   docker-compose-mode ef-themes eglot-booster
		   eglot-signature-eldoc-talkative eglot-tempel
		   eglot-x eldoc-box embark-consult emmet-mode erc
		   expreg external-completion faceup flx flymake
		   flymake-collection fontaine gfm-mode goggles
		   golden-ratio highlight-defined highlight-quoted
		   idlwave imenu-list jtsx let-alist lin map
		   marginalia markdown-preview-mode mermaid-mode
		   migemo minions mozc nadvice neotree
		   nerd-icons-completion nerd-icons-corfu
		   nerd-icons-dired ntlm nyan-mode orderless org
		   page-break-lines peg perfect-margin project pulsar
		   puni python pyvenv rainbow-delimiters request rg
		   rust-mode seq slime-company so-long soap-client
		   spacious-padding sql-indent sqlformat
		   string-inflection svg tabnine tempel-collection
		   tr-ime tramp treesit-auto typescript-mode undo-fu
		   undo-fu-session use-package vc-use-package
		   verilog-mode vertico-prescient vertico-truncate
		   vundo web-beautify window-tool-bar xref
		   xref-history-storage))
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
 '(aw-leading-char-face ((t (:foreground "red" :height 4.0))))
 '(line-number-current-line ((t (:inherit line-number)))))
