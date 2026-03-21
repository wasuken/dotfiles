;;; core.el --- Core packages -*- lexical-binding: t; -*-

;;; Commentary:
;; 基盤となるパッケージ群
;; 他のパッケージやutil.elから依存されるもの

;;; Code:

;; HTTP通信用 (util.elのcreate-md-link等で使用)
(use-package request)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (print "test1")
  )

;; ファイル自動保存
(use-package files
  :straight (:type built-in)
  :config
  (print "test2")
  (setq auto-save-visited-interval 30)
  (auto-save-visited-mode +1))

;; カーソル位置記憶
(use-package saveplace
  :config
  (save-place-mode +1))

;; 最近使ったファイル
(use-package recentf
  :config
  (setq recentf-max-saved-items 100)
  (recentf-mode +1))

;; ミニバッファ履歴保存
(use-package savehist
  :config
  (savehist-mode +1))

;; 括弧ハイライト
(use-package paren
  :config
  (show-paren-mode +1))

;; 括弧自動補完
(use-package elec-pair
  :config
  (electric-pair-mode +1))

;; カーソル行ハイライト
(use-package hl-line
  :config
  (global-hl-line-mode +1))

;; 外部変更の自動リロード
(use-package autorevert
  :config
  (global-auto-revert-mode +1))

;; キャメルケース対応
(use-package subword
  :config
  (global-subword-mode +1))

;; 長い行のパフォーマンス改善
(use-package so-long
  :config
  (global-so-long-mode +1))

;; 選択範囲の置き換え
(use-package delsel
  :config
  (delete-selection-mode +1))

;; Undo/Redo改善
(use-package undo-fu
  :config
  (with-eval-after-load 'evil
    (setq evil-undo-system 'undo-fu)))

(use-package undo-fu-session
  :config
  (undo-fu-session-global-mode +1))

(use-package vundo
  :config
  (global-set-key (kbd "C-x u") 'vundo))

(provide 'core)
;;; core.el ends here
