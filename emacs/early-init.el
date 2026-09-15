;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs 27+で最初に読み込まれるファイル
;; パッケージマネージャーの競合を防ぐ

;;; Code:

;; Older Roswell/SLIME sources and private config files may omit this cookie.
;; Preserve their binding behavior while silencing only this Emacs 31 warning.
(require 'warnings)
(add-to-list 'warning-inhibit-types '(files missing-lexbind-cookie))

;; package.elを無効化 (straight.elを使用するため)
(setq package-enable-at-startup nil)

;; GCの閾値を一時的に上げて起動高速化
(setq gc-cons-threshold most-positive-fixnum)

;; 起動後にGC閾値を戻す
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 128 1024 1024))
            (setq gc-cons-percentage 0.1)))

(setq x-wait-for-event-timeout nil)  ;; Xイベント待機タイムアウト無効
(setq x-select-enable-clipboard-manager nil)  ;; クリップボードマネージャ無効
(setq select-active-regions nil)  ;; 選択→クリップボード自動同期を無効

(provide 'early-init)
;;; early-init.el ends here
