;;; ui.el --- User interface enhancements -*- lexical-binding: t; -*-

;;; Commentary:
;; テーマ、アイコン、ダッシュボード等のUI設定

;;; Code:

;; フォント設定
(use-package fontaine
  :config
  (cond ((memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix))
         (setq fontaine-presets
               '((regular
                  :default-family "PlemolJP HS"
                  :fixed-pitch-family "PlemolJP HS"
                  :variable-pitch-family "IBM Plex Sans JP"
                  :italic-family "PlemolJP HS")
                 (large
                  :default-family "PlemolJP HS"
                  :variable-pitch-family "IBM Plex Sans JP"))))

        ((memq system-type '(cygwin windows-nt ms-dos))
         (setq fontaine-presets
               '((regular
                  :default-family "PlemolJP Console NF"
                  :fixed-pitch-family "PlemolJP Console NF"
                  :variable-pitch-family "IBM Plex Sans JP"
                  :italic-family "PlemolJP Console NF")
                 (large
                  :default-family "PlemolJP Console NF"
                  :variable-pitch-family "IBM Plex Sans JP")))))

  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))

;; アイコン
(use-package nerd-icons)

(use-package nerd-icons-completion
  :hook (after-init . nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; Verticoのアイコン表示 (completion.elから移動)
(with-eval-after-load 'vertico
  (defvar +vertico-current-arrow t)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((and +vertico-current-arrow
                                                   (not (bound-and-true-p vertico-flat-mode)))
                                              (eql t)))
    (setq cand (cl-call-next-method cand prefix suffix index start))
    (let ((arrow (nerd-icons-faicon "nf-fa-hand_o_right")))
      (if (bound-and-true-p vertico-grid-mode)
          (if (= vertico--index index)
              (concat arrow " " cand)
            (concat #("_" 0 1 (display " ")) cand))
        (if (= vertico--index index)
            (concat " " arrow " " cand)
          (concat "    " cand))))))

;; モードライン
(use-package nyan-mode
  :config
  (setq nyan-bar-length 16)        ;; 24→16に減らす
  (setq nyan-animate-nyancat nil)  ;; アニメーション無効
  (setq nyan-wavy-trail nil)       ;; 波アニメ無効
  (nyan-mode +1))

(use-package minions
  :config
  (minions-mode +1))

;; テーマ
(use-package ef-themes
  :config
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  (load-theme 'ef-night t))

;; Which Key
(use-package which-key
  :config
  (which-key-mode +1))

;; ダッシュボード
(use-package dashboard
  :config
  (setq dashboard-center-content t)
  (dashboard-setup-startup-hook))

(use-package page-break-lines
  :config
  (page-break-lines-mode +1))

;; Breadcrumb
(use-package breadcrumb
  :straight nil
  :vc ( :fetcher github :repo "joaotavora/breadcrumb")
  :config
  (breadcrumb-mode +1))

;; 括弧の色分け
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Imenu
(use-package imenu-list
  :bind ( :map my-toggle-map
          ("i" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-position 'left))

;; カーソル行ハイライト
(use-package lin
  :config
  (setq lin-face 'lin-red)
  (lin-global-mode +1))

;; カーソル位置のパルス
(use-package pulsar
  :config
  (pulsar-global-mode +1))

;; 編集箇所のハイライト
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

;; 余白調整
(use-package spacious-padding
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8))

  (setq spacious-padding-subtle-mode-line
        `( :mode-line-active 'default
           :mode-line-inactive vertical-border))

  (spacious-padding-mode +1))

;; フレーム別バッファ管理
(use-package beframe
  :config
  (defvar consult-buffer-sources)
  (declare-function consult--buffer-state "consult")

  (with-eval-after-load 'consult
    (defface beframe-buffer
      '((t :inherit font-lock-string-face))
      "Face for `consult' framed buffers.")

    (defvar beframe-consult-source
      `( :name     "Frame-specific buffers (current frame)"
         :narrow   ?F
         :category buffer
         :face     beframe-buffer
         :history  beframe-history
         :items    ,#'beframe-buffer-names
         :action   ,#'switch-to-buffer
         :state    ,#'consult--buffer-state))

    (add-to-list 'consult-buffer-sources 'beframe-consult-source))

  (beframe-mode +1))

;; Perfect Margin (デフォルトOFF)
(use-package perfect-margin
  :config
  (setq perfect-margin-ignore-filters nil)
  (perfect-margin-mode -1)
  (define-key global-map (kbd "C-c p m") 'perfect-margin-mode))

;; Golden Ratio (コメントアウト - パフォーマンス懸念)
(use-package golden-ratio
  :config
  (golden-ratio-mode))

(provide 'ui)
;;; ui.el ends here
