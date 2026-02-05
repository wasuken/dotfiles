;;; lsp.el --- LSP and development tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Eglot, Flymake等のLSP関連設定

;;; Code:

;; Flymake - リアルタイム構文チェック
(use-package flymake
  :hook ((prog-mode
          conf-mode) . flymake-mode)
  :config
  (setq-default flymake-no-changes-timeout 1.0))

;; Flymake Collection - 各種linter統合
(use-package flymake-collection
  :hook ((after-init . flymake-collection-hook-setup)
         ((tsx-ts-mode
           jsx-ts-mode
           jtsx-jsx-mode
           jtsx-tsx-mode
           jtsx-typescript-mode) . (lambda () (add-to-list 'flymake-diagnostic-functions #'flymake-collection-eslint)))
         (eglot-managed-mode . (lambda () (add-to-list 'flymake-diagnostic-functions #'eglot-flymake-backend)))))

;; Eglot - LSPクライアント
(use-package eglot
  :bind ( :map eglot-mode-map
          ("C-c r" . eglot-rename)
          ("C-c o" . eglot-code-action-organize-imports)
          ("C-c a" . eglot-code-actions)
          ("C-c h" . eldoc)
          ("<f6>" . xref-find-definitions))
  :config
  (setq eglot-events-buffer-config '(:size 0 :format short)
        eglot-ignored-server-capabilities '(:documentHighlightProvider)
        eglot-stay-out-of '(flymake)
        eglot-send-changes-idle-time 1.0)

  ;; LSP実行パスを再帰的に追加
  (defun my/add-directory-to-exec-path-recursively (dir)
    "Recursively add directories and their subdirectories to `exec-path`."
    (add-to-list 'exec-path dir)
    (dolist (entry (directory-files dir t "^[^.]" t))
      (when (file-directory-p entry)
        (my/add-directory-to-exec-path-recursively entry))))

  (defun my/load-lsp-exec-path ()
    (interactive)
    (my/add-directory-to-exec-path-recursively "~/.emacs.d/.cache/"))

  (my/load-lsp-exec-path))

;; Eglot-Tempel統合
(use-package eglot-tempel
  :after (eglot tempel)
  :hook (eglot-managed-mode . eglot-tempel-mode))

;; Consult-Eglot統合
(use-package consult-eglot
  :after eglot
  :bind ( :map eglot-mode-map
          ("C-c s" . consult-eglot-symbols)))

;; JSONRPC設定
(use-package jsonrpc
  :defer t
  :config
  (setq jsonrpc-default-request-timeout 3000)
  (fset #'jsonrpc--log-event #'ignore))

;; Eglot Booster - 高速化
(use-package eglot-booster
  :after eglot
  :straight nil
  :vc ( :fetcher github :repo "jdtsmith/eglot-booster")
  :config
  (eglot-booster-mode +1))

;; Eglot X - 拡張機能
(use-package eglot-x
  :straight nil
  :vc ( :fetcher github :repo "nemethf/eglot-x")
  :after eglot
  :config
  (eglot-x-setup))

;; Eldoc Box - フローティング表示
(use-package eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-mode))

;; Eldoc改善
(use-package eglot-signature-eldoc-talkative
  :after eldoc-box
  :config
  (advice-add #'eglot-signature-eldoc-function
              :override #'eglot-signature-eldoc-talkative))

;; Tempel - スニペット
(use-package tempel
  :demand t
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))
  :config
  (setq tempel-path (expand-file-name "templates" user-emacs-directory)))

(use-package tempel-collection :after tempel)

(provide 'lsp)
;;; lsp.el ends here
