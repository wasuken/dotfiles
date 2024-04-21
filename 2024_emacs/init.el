;; 必要に応じてロードパスを追加
;; (add-to-list 'load-path "/path/to/your/lisp/files")
(add-to-list 'load-path "~/dotfiles/2024_emacs/lib")

;; 必要に応じてrequireを追加
;; システム系、パッケージ読み込み前になにかしたいときとか
(load "custom.el")
;; 環境系
(load "env.el")
;; パッケージ系
(load "mypackage.el")
;; my config
(load "util.el")
;; キーマップ系
(load "keymap.el")

;; 必要に応じてカスタム設定を追
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(modus-themes nerd-icons-corfu sqlformat slime-company slime cargo eglot-signature-eldoc-talkative eglot-x eglot-booster consult-eglot eglot-tempel ace-window rg difftastic magit tempel-collection tempel embark-consult embark marginalia consult corfu-prescient vertico-prescient vertico-truncate corfu-terminal tabnine which-key wgrep web-beautify vundo vertico vc-use-package undo-fu-session undo-fu typescript-mode treesit-auto transient tr-ime string-inflection sql-indent spacious-padding rust-mode reformatter rainbow-delimiters pyvenv puni pulsar prescient popon perfect-margin page-break-lines orderless nyan-mode nerd-icons-dired nerd-icons-completion mozc minions migemo markdown-mode macrostep lin jtsx jsonrpc imenu-list highlight-quoted highlight-defined goggles go-translate fontaine flymake-collection flx expreg emmet-mode eldoc-box ef-themes diff-hl dashboard corfu company cape breadcrumb beframe avy auto-package-update auto-compile apheleia aggressive-indent))
 '(package-vc-selected-packages
   '((nerd-icons-corfu :vc-backend Git :url "https://github.com/LuigiPiucco/nerd-icons-corfu")
     (vc-use-package :vc-backend Git :url "https://github.com/slotThe/vc-use-package"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:foreground "red" :height 4.0))))
 '(fringe ((t :background "#fff6d8")))
 '(header-line ((t :box (:line-width 4 :color "#f5e9cb" :style nil))))
 '(header-line-highlight ((t :box (:color "#484431"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "#fff6d8")))
 '(mode-line ((t :background "#fff6d8" :overline "#403328" :box (:line-width 6 :color "#fff6d8" :style nil))))
 '(mode-line-active ((t :background "#fff6d8" :overline "#403328" :box (:line-width 6 :color "#fff6d8" :style nil))))
 '(mode-line-highlight ((t :box (:color "#484431"))))
 '(mode-line-inactive ((t :background "#fff6d8" :overline "#c5baa6" :box (:line-width 6 :color "#fff6d8" :style nil))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "#fff6d8" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "#c7b7a6" :style nil))))
 '(tab-line-tab ((t)))
 '(tab-line-tab-active ((t)))
 '(tab-line-tab-inactive ((t)))
 '(vertical-border ((t :background "#fff6d8" :foreground "#fff6d8")))
 '(window-divider ((t (:background "#fff6d8" :foreground "#fff6d8"))))
 '(window-divider-first-pixel ((t (:background "#fff6d8" :foreground "#fff6d8"))))
 '(window-divider-last-pixel ((t (:background "#fff6d8" :foreground "#fff6d8")))))
