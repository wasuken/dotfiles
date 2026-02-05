;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs 27+で最初に読み込まれるファイル
;; パッケージマネージャーの競合を防ぐ

;;; Code:

;; package.elを無効化 (straight.elを使用するため)
(setq package-enable-at-startup nil)

;; GCの閾値を一時的に上げて起動高速化
(setq gc-cons-threshold most-positive-fixnum)

;; 起動後にGC閾値を戻す
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))

(provide 'early-init)
;;; early-init.el ends here
