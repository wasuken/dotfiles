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
   '(ace-jump-mode ace-window aggressive-indent apheleia auto-compile
                   auto-package-update avy beframe breadcrumb cape
                   cargo company consult consult-eglot corfu
                   corfu-prescient corfu-terminal dashboard diff-hl
                   difftastic ef-themes eglot-booster
                   eglot-signature-eldoc-talkative eglot-tempel
                   eglot-x eldoc-box embark embark-consult emmet-mode
                   expreg flx flymake-collection fontaine go-translate
                   goggles golden-ratio golden-ratio-mode
                   highlight-defined highlight-quoted imenu-list
                   jsonrpc jtsx lin macrostep magit marginalia
                   markdown-mode migemo minions modus-themes mozc
                   nerd-icons-completion nerd-icons-corfu
                   nerd-icons-dired nyan-mode orderless
                   page-break-lines perfect-margin popon prescient
                   pulsar puni pyvenv rainbow-delimiters reformatter
                   rg rust-mode slime slime-company spacious-padding
                   sql-indent sqlformat string-inflection tabnine
                   tempel tempel-collection tr-ime transient treesit
                   treesit-auto treesitter typescript-mode undo-fu
                   undo-fu-session vc-use-package vertico
                   vertico-prescient vertico-truncate vundo
                   web-beautify wgrep which-key))
 '(package-vc-selected-packages
   '((nerd-icons-corfu :vc-backend Git :url
                       "https://github.com/LuigiPiucco/nerd-icons-corfu")
     (vc-use-package :vc-backend Git :url
                     "https://github.com/slotThe/vc-use-package"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:foreground "red" :height 4.0))))
 '(fringe ((t :background "#000e17")))
 '(header-line ((t :box (:line-width 4 :color "#1a202b" :style nil))))
 '(header-line-highlight ((t :box (:color "#afbcbf"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "#000e17")))
 '(mode-line ((t :background "#000e17" :overline "#ceeeff" :box (:line-width 6 :color "#000e17" :style nil))))
 '(mode-line-active ((t :background "#000e17" :overline "#ceeeff" :box (:line-width 6 :color "#000e17" :style nil))))
 '(mode-line-highlight ((t :box (:color "#afbcbf"))))
 '(mode-line-inactive ((t :background "#000e17" :overline "#3a4a66" :box (:line-width 6 :color "#000e17" :style nil))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "#000e17" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "#444e59" :style nil))))
 '(tab-line-tab ((t)))
 '(tab-line-tab-active ((t)))
 '(tab-line-tab-inactive ((t)))
 '(vertical-border ((t :background "#000e17" :foreground "#000e17")))
 '(window-divider ((t (:background "#000e17" :foreground "#000e17"))))
 '(window-divider-first-pixel ((t (:background "#000e17" :foreground "#000e17"))))
 '(window-divider-last-pixel ((t (:background "#000e17" :foreground "#000e17")))))
