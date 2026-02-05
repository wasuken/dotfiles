;;; manager.el --- Package manager setup -*- lexical-binding: t; -*-

;;; Commentary:
;; straight.el + use-package setup
;; パッケージマネージャーの初期化とauto-update設定

;;; Code:

(setq package-enable-at-startup nil)

;; straight.elの警告を抑制
(setq warning-suppress-types '((straight)))

;; straight.elのブートストラップ
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-packageをインストール
(straight-use-package 'org)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; vc-use-package (GitHub等からインストール用)
(straight-use-package
 '(vc-use-package :type git :host github :repo "slotThe/vc-use-package"))
(require 'vc-use-package)

;; auto-compileの設定
(use-package auto-compile
  :config
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode +1)
  (auto-compile-on-save-mode +1))

;; パッケージ自動更新 (週1回)
(use-package auto-package-update
  :config
  (setq auto-package-update-interval 7)
  (auto-package-update-maybe))

(provide 'manager)
;;; manager.el ends here
