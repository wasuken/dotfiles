;;; languages.el --- Language-specific configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; TypeScript, Rust, Python等の言語別設定

;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Tree-sitter設定
(setq treesit-language-source-alist
      '((json "https://github.com/tree-sitter/tree-sitter-json")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (python "https://github.com/tree-sitter/tree-sitter-python")))

(use-package treesit
  :straight (:type built-in)
  :config
  (setq treesit-font-lock-level 4)
  (setq go-ts-mode-indent-offset 4))

(use-package treesit-auto
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (setq treesit-auto-install t)
  (global-treesit-auto-mode +1))

;; TypeScript/JavaScript
(use-package typescript-mode)

(use-package jtsx
  :ensure t
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :commands jtsx-install-treesit-language
  :hook ((jtsx-jsx-mode . hs-minor-mode)
         (jtsx-tsx-mode . hs-minor-mode)
         (jtsx-typescript-mode . hs-minor-mode))
  :custom
  (jtsx-indent-statement-block-regarding-standalone-parent nil)
  (jtsx-jsx-element-move-allow-step-out t)
  (jtsx-enable-jsx-electric-closing-element t)
  (jtsx-enable-electric-open-newline-between-jsx-element-tags t)
  (jtsx-enable-jsx-element-tags-auto-sync t)
  (jtsx-enable-all-syntax-highlighting-features t)
  :config
  (defun jtsx-bind-keys-to-mode-map (mode-map)
    "Bind keys to MODE-MAP."
    (define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
    (define-key mode-map (kbd "C-c j o") 'jtsx-jump-jsx-opening-tag)
    (define-key mode-map (kbd "C-c j c") 'jtsx-jump-jsx-closing-tag)
    (define-key mode-map (kbd "C-c j r") 'jtsx-rename-jsx-element)
    (define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
    (define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
    (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
    (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
    (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
    (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
    (define-key mode-map (kbd "C-c j w") 'jtsx-wrap-in-jsx-element)
    (define-key mode-map (kbd "C-c j u") 'jtsx-unwrap-jsx)
    (define-key mode-map (kbd "C-c j d") 'jtsx-delete-jsx-node))

  (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
    (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

  (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
    (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

  (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
  (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map))

(use-package emmet-mode
  :hook ((html-mode
          css-mode
          js-mode
          typescript-mode) . emmet-mode))

(use-package web-beautify
  :defer t)

;; Rust
(use-package rust-mode
  :hook (rust-mode . (lambda ()
                       (setq-local tab-width 4))))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

;; Python
(use-package pyvenv
  :defer t)

;; Lisp
(use-package slime-company
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space))

(use-package slime
  :if (file-exists-p "~/.roswell/helper.el")
  :ensure slime-company
  :init (load "~/.roswell/helper.el")
  :custom (inferior-lisp-program "ros -Q run")
  :config (slime-setup '(slime-fancy slime-company)))

(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

;; SQL
(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

(use-package sqlformat
  :config
  (setq sqlformat-command "sqlfluff"))

;; その他
(use-package docker-compose-mode
  :mode (("docker-compose\\.yml\\'" . docker-compose-mode)
         ("[Cc]ompose\\.yml\\'" . docker-compose-mode)
         ("\\(?:docker\\|compose\\).+\\.yaml\\'" . docker-compose-mode)))

(use-package mermaid-mode)

;; 文字列変換
(use-package string-inflection
  :bind (("C-c C-u" . string-inflection-all-cycle))
  :config
  (defun my-string-inflection-cycle-auto ()
    "Switching by major-mode"
    (interactive)
    (cond
     ((or (eq major-mode 'typescript-mode)
          (eq major-mode 'js-mode)
          (eq major-mode 'jtsx-jsx-mode)
          (eq major-mode 'jtsx-tsx-mode))
      (string-inflection-java-style-cycle))
     ((eq major-mode 'python-mode)
      (string-inflection-python-style-cycle))
     ((eq major-mode 'rust-mode)
      (string-inflection-ruby-style-cycle))
     (t
      (string-inflection-all-cycle))))

  (global-set-key (kbd "C-c u") 'my-string-inflection-cycle-auto))

;; Puni - S式操作
(use-package puni
  :config
  (puni-global-mode +1))

;; Expreg - 賢い範囲選択
(use-package expreg
  :config
  (global-set-key (kbd "C-=") 'expreg-expand)
  (global-set-key (kbd "C--") 'expreg-contract))

(use-package inf-ruby
  :ensure t
  :hook (ruby-ts-mode . inf-ruby-minor-mode))

(provide 'languages)
;;; languages.el ends here
