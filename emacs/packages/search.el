;;; search.el --- Search and navigation -*- lexical-binding: t; -*-

;;; Commentary:
;; Consult, Embark, Avy等の検索・ナビゲーション機能

;;; Code:

;; Consult - 強力な検索インターフェース
(use-package consult
  :bind (;; C-c bindings
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings
         ("C-x M-:" . consult-complex-command)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ;; Custom M-# bindings
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
         :map goto-map
         ("e" . consult-compile-error)
         ("f" . consult-flymake)
         ("g" . consult-goto-line)
         ("M-g" . consult-goto-line)
         ("o" . consult-outline)
         ("m" . consult-mark)
         ("k" . consult-global-mark)
         ("i" . consult-imenu)
         ("I" . consult-imenu-multi)
         :map search-map
         ("d" . consult-fd)
         ("D" . consult-locate)
         ("g" . consult-grep)
         ("G" . consult-git-grep)
         ("r" . consult-ripgrep)
         ("l" . consult-line)
         ("L" . consult-line-multi)
         ("m" . consult-multi-occur)
         ("k" . consult-keep-lines)
         ("u" . consult-focus-lines)
         ("e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :config
  ;; Register formatting
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  ;; Preview configuration
  (consult-customize
   consult-theme :preview-key '(:debounce 1.0 any)
   consult-ripgrep consult-git-grep consult-grep
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  ;; Narrowing key
  (setq consult-narrow-key "<"))

;; カレントディレクトリでripgrep
(defun consult-ripgrep-current-directory ()
  (interactive)
  (consult-ripgrep default-directory))

(define-key search-map (kbd "R") #'consult-ripgrep-current-directory)

;; Marginalia - 補完候補に説明を追加
(use-package marginalia
  :config
  (marginalia-mode +1))

;; Embark - 補完候補に対するアクション
(use-package embark
  :bind (("C-c e ." . embark-act)
         ("C-c e ;" . embark-dwim)
         ("C-c e b" . embark-bindings))
  :config
  (setq prefix-help-command #'embark-prefix-help-command)

  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark-Consult統合
(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Avy - 画面内ジャンプ
(use-package avy)

;; Ace Window - ウィンドウ切り替え
(use-package ace-window
  :config
  (custom-set-faces
   '(aw-leading-char-face ((t (:foreground "red" :height 4.0))))))

;; Ace Jump Mode - キーボードジャンプ
(use-package ace-jump-mode
  :config
  (define-key global-map (kbd "C-'") #'ace-jump-char-mode)
  (define-key global-map (kbd "C-M-'") #'ace-jump-word-mode)
  (define-key global-map (kbd "M-'") #'ace-jump-line-mode))

;; Migemo - ローマ字検索
(use-package migemo
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))

  ;; Windows環境の場合のみ辞書パスを設定
  (when (memq system-type '(cygwin windows-nt ms-dos))
    (setq migemo-dictionary
          "C:/Users/u396/scoop/apps/cmigemo/current/cmigemo-mingw64/share/migemo/utf-8/migemo-dict"))

  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)

  (migemo-init))

;; Ripgrep
(use-package rg
  :defer t)

(provide 'search)
;;; search.el ends here
