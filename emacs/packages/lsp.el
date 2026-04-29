;;; lsp.el --- LSP and development tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Eglot, Flymake等のLSP関連設定

;;; Code:

(use-package tempel
  :bind (("M-*" . tempel-insert)))

(use-package eglot
  :straight (eglot :type git
                   :host github
                   :repo "joaotavora/eglot"
                   :files ("eglot.el"))
  :config
  (setq eglot-events-buffer-config '(:size 100 :format full)
        eglot-send-changes-idle-time 1.0)
  (add-to-list 'eglot-server-programs
               '((go-ts-mode go-mod-ts-mode) . ("gopls" "-remote=auto"))))

(use-package flymake
  :hook ((prog-mode conf-mode) . flymake-mode)
  :config
  (setq-default flymake-no-changes-timeout 1.0))

(use-package eglot-tempel
  :after (eglot tempel)
  :hook (eglot-managed-mode . eglot-tempel-mode))

(use-package eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-mode))

(use-package eglot-signature-eldoc-talkative
  :after eldoc-box
  :config
  (advice-add #'eglot-signature-eldoc-function
              :override #'eglot-signature-eldoc-talkative))

(use-package consult-eglot
  :after eglot
  :bind ( :map eglot-mode-map
          ("C-c s" . consult-eglot-symbols)))

(use-package jsonrpc
  :defer t
  :config
  (setq jsonrpc-default-request-timeout 30)
  (fset #'jsonrpc--log-event #'ignore))

(use-package flymake-collection
  :hook ((after-init . flymake-collection-hook-setup)
         ((tsx-ts-mode jsx-ts-mode) . 
          (lambda () (add-to-list 'flymake-diagnostic-functions #'flymake-collection-eslint)))
         (eglot-managed-mode . 
			     (lambda () (add-to-list 'flymake-diagnostic-functions #'eglot-flymake-backend)))))

(use-package cape
  :config
  (defun my/set-super-capf-eglot ()
    (setq-local completion-at-point-functions
		(list (cape-capf-super
                       (car completion-at-point-functions)
                       #'cape-dabbrev
                       #'cape-file))))
  (add-hook 'eglot-managed-mode-hook #'my/set-super-capf-eglot))

(setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
(setq eglot-server-programs
      (assoc-delete-all '(go-mode go-dot-mod-mode go-dot-work-mode
                                  go-ts-mode go-mod-ts-mode go-work-ts-mode)
                        eglot-server-programs))
(add-to-list 'eglot-server-programs
             '((go-ts-mode go-mod-ts-mode) . ("gopls" "-remote=auto")))

(use-package eglot-x
  :straight nil
  :vc ( :fetcher github :repo "nemethf/eglot-x")
  :after eglot
  :config
  (eglot-x-setup))

(provide 'lsp)
;;; lsp.el ends here
