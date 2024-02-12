(require 'package)
;; Add the melpa repository
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     '("gnu-devel" . "https://elpa.gnu.org/devel/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-display-line-numbers-mode)

(set-face-attribute 'region nil :background "#ccc")

(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'whitespace-cleanup)
(set-default 'truncate-lines t)
(setq create-lockfiles nil)

(electric-pair-mode 1)
(show-paren-mode 1)

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(global-set-key "\C-h" 'delete-backward-char)

;; (global-linum-mode t)
(setq linum-format "%d ")

(setq inhibit-splash-screen t)


;; gosh
(setq scheme-program-name "gosh -i")

;; ============== start org-mode ==============
(setq org-agenda-files '(
			 "/home/wasu/org/agenda.org"
			 "/home/wasu/org/remind.org"
			 "/home/wasu/org/knowledge.org"
			 )
      )

(setq org-capture-templates
      '(
	("t" "Todo" entry (file+headline "/home/wasu/org/remind.org" "■Capture")
	 "* REMIND %? (wrote on %U)")
	("k" "Knowledge" entry (file+headline "/home/wasu/org/knowledge.org" "Top")
	 "* %?\n  # Wrote on %U")
	)
      )

(define-key global-map (kbd "C-c a") 'org-agenda)

;; アジェンダ表示で下線を用いる
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
(setq hl-line-face 'underline)

;; 標準の祝日を利用しない
(setq calendar-holidays nil)

;; TODO状態
(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(i)" "WAIT(w)" "REMIND(r)" "|" "DONE(d)" "SOMEDAY(s)")))

(setq org-todo-keyword-faces
      '(("TODO" . "orange") ("DOING" . "magenta") ("WAIT" . "red") ("DONE" . "green"))
      )

;; DONEの時刻を記録
(setq org-log-done 'time)

;; タグリスト
(setq org-tag-alist
      '(
	("reading" . ?r)
	("develop" .?d)
	("plan" . ?p)
	("introspection" . ?i)
	("networking" . ?n)
	)
      )


;; ###### org-capture
;; C-cc で org-capture
(global-set-key "\C-cc" 'org-capture)


;; ##### org-clock
;; clockログを隠す
(setq org-clock-into-drawer t)

;; clocktable の体裁を整える
(defun my-org-clocktable-indent-string (level)
  (if (= level 1) ""
    (let ((str " "))
      (while (> level 2)
	(setq level (1- level)
	      str (concat str "--")))
      (concat str "-> "))))

(advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)

;; ============== end org-mode ==============

;; lib内部で利用するため
(use-package request :ensure t)
(add-to-list 'load-path "~/dotfiles/simple_emacs/lib")
(load "util.el")

;; ----------------------------------------------------------------
;; use-package
;; ----------------------------------------------------------------

(use-package mwim
  :ensure t
  :config
  (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system
	      '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package path-headerline-mode :ensure t
  :config
  (path-headerline-mode +1)
  )

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
	'("https://b.hatena.ne.jp/hotentry/it.rss"
	  "https://zenn.dev/feed"
	  "https://zenn.dev/topics/rust/feed"
	  "https://zenn.dev/topics/nextjs/feed"
	  "https://zenn.dev/topics/solidjs/feed"
	  "https://zenn.dev/topics/react/feed"
	  )))

(use-package biblio :ensure t)

(use-package neotree :ensure t
  :config
  (global-set-key (kbd "C-c t") 'neotree))

(use-package ddskk :ensure t
  :config
  (setq default-input-method "japanese-skk")
  (setq skk-large-jisyo "~/.emacs.d/SKK-JISYO.L")
  (setq skk-jisyo-list '("~/.emacs.d/SKK-JISYO.L" "~/.emacs.d/SKK-JISYO.propernoun"))
  (require 'skk-study))

(use-package ddskk-posframe
  :ensure t)

(use-package skk
  :config
  (global-set-key (kbd "<zenkaku-hankaku>") 'skk-mode)
  (global-set-key (kbd "C-<zenkaku-hankaku>") 'skk-katakana-region))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package emojify :ensure t
  :config
  (global-emojify-mode t))

(use-package rainbow-mode :ensure t
  :config
  (global-set-key (kbd "C-x c") 'rainbow-mode))

(use-package ivy :ensure t
  :config
  (ivy-mode 1))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c j" . ace-jump-mode)))

(use-package ace-window :ensure t
  :config
  (global-set-key (kbd "C-x o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-global-modes '(not markdown-mode shell-mode rust-mode))
  ;; 遅延なしにする。
  (setq company-idle-delay 0)
  ;; デフォルトは4。より少ない文字数から補完が始まる様にする。
  (setq company-minimum-prefix-length 2)
  ;; 候補の一番下でさらに下に行こうとすると一番上に戻る。
  (setq company-selection-wrap-around t)
  ;; 番号を表示する。
  (setq company-show-numbers t)
  :bind (:map company-active-map
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous)
	      ("C-s" . company-filter-candidates)
	      ("<tab>" . company-complete-selection))
  :bind (:map company-search-map
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous)))

(use-package company-tabnine
  :ensure t
  :config
  ;; (add-to-list 'company-backends #'company-tabnine)
  )

(use-package magit :ensure t)

(use-package flycheck :ensure t
  :init
  (global-flycheck-mode t))


(use-package swiper :ensure t
  :config
  (global-set-key "\C-s" 'swiper))


(use-package web-mode :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
  )

(use-package which-key :ensure t
  :config
  (which-key-mode))



(use-package org-tree-slide :ensure t
  :custom
  (org-tree-slide-slide-in-effect t)
  (org-tree-slide-activate-message "Presentation started!")
  (org-tree-slide-deactivate-message "Presentation finished!")
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " > ")
  (org-image-actual-width nil))

(use-package eglot :ensure t
  :config)

(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
	 ("C-c M-x" . consult-mode-command)
	 ("C-c h" . consult-history)
	 ("C-c k" . consult-kmacro)
	 ("C-c m" . consult-man)
	 ("C-c i" . consult-info)
	 ([remap Info-search] . consult-info)
	 ;; C-x bindings in `ctl-x-map'
	 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register access
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ;; M-g bindings in `goto-map'
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings in `search-map'
	 ("M-s d" . consult-find)
	 ("M-s D" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
	 ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package marginalia :ensure t
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
	 :map minibuffer-local-map
	 ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ;; ("C-b" . embark-bindings)
   ) ;; alternative for `describe-bindings'
  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package markdown-mode :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package golden-ratio :ensure t
  :config
  (golden-ratio-mode 1)
  (global-set-key (kbd "C-c r") 'golden-ratio)
  (setq golden-ratio-extra-commands
	'(windmove-left windmove-right windmove-down windmove-up))
  )

(defun eglot-organize-imports ()
  "Offer to execute the source.organizeImports code action."
  (interactive)
  (unless (eglot--server-capable :codeActionProvider)
    (eglot--error "Server can't execute code actions!"))
  (let* ((server (eglot--current-server-or-lose))
	 (actions (jsonrpc-request
		   server
		   :textDocument/codeAction
		   (list :textDocument (eglot--TextDocumentIdentifier))))
	 (action (cl-find-if
		  (jsonrpc-lambda (&key kind &allow-other-keys)
		    (string-equal kind "source.organizeImports" ))
		  actions)))
    (when action
      (eglot--dcase action
	(((Command) command arguments)
	 (eglot-execute-command server (intern command) arguments))
	(((CodeAction) edit command)
	 (when edit (eglot--apply-workspace-edit edit))
	 (when command
	   (eglot--dbind ((Command) command arguments) command
	     (eglot-execute-command server (intern command) arguments))))))))

(defun eglot-organize-imports-on-save ()
  (defun eglot-organize-imports-nosignal ()
    "Run eglot-organize-imports, but demote errors to messages."
    ;; Demote errors to work around
    ;; https://github.com/joaotavora/eglot/issues/411#issuecomment-749305401
    ;; so that we do not prevent subsequent save hooks from running
    ;; if we encounter a spurious error.
    (with-demoted-errors "Error: %s" (eglot-organize-imports)))
  (add-hook 'before-save-hook #'eglot-organize-imports-on-save)
  )

(use-package go-mode :ensure t
  :config
  (add-hook 'go-mode-hook #'eglot-organize-imports-on-save)
  (add-hook 'go-mode-hook #'eglot-ensure)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook
	    (lambda ()
	      (setq-default)
	      (setq tab-width 2)
	      (setq standard-indent 2)
	      (setq indent-tabs-mode nil)))
  )

(setq exec-path (cons (expand-file-name "~/bin") exec-path))
(setq exec-path (cons (expand-file-name "~/.cargo/bin") exec-path))

(use-package rust-mode
  :ensure t
  :bind (("C-c n t" . rust-test) ("C-c n f" . rust-format-buffer))
  ;; くそおそいので無効化
  ;; :custom (rust-format-on-save t)
  :config
  (setq rust-cargo-default-arguments " -- --test-threads=1")
  )


(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package lsp-mode
  :ensure t
  :hook (rust-mode . lsp)
  :bind ("C-c h" . lsp-describe-thing-at-point)
  :custom (lsp-rust-server 'rust-analyzer))

(use-package lsp-ui :ensure t)

(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install t)
  (global-treesit-auto-mode))

(use-package treesit
  :config
  (setq treesit-font-lock-level 4))

(use-package prettier
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-prettier-mode))

(use-package tree-sitter
  :ensure t
  :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode)
  (tree-sitter-require 'c)
  (add-to-list 'tree-sitter-major-mode-language-alist '(c-mode . c))
  )

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-require 'cpp)
  (add-to-list 'tree-sitter-major-mode-language-alist '(cpp-mode . c))
  )

(use-package c-mode
  :hook (c-mode . eglot))

(use-package slime
  :if (file-exists-p "~/.roswell/helper.el")
  :ensure slime-company
  :init (load "~/.roswell/helper.el")
  :custom (inferior-lisp-program "ros -Q run")
  :config (slime-setup '(slime-fancy slime-company)))

(use-package cider
  :ensure t
  :init
  (add-hook 'cider-mode-hook #'clj-refactor-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t
	cider-repl-display-in-current-window t
	cider-repl-use-clojure-font-lock t
	cider-prompt-save-file-on-load 'always-save
	cider-font-lock-dynamically '(macro core function var)
	cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))

(use-package clojure-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'subword-mode))

(add-to-list
 'treesit-language-source-alist
 '(prisma "https://github.com/victorhqc/tree-sitter-prisma"))

(use-package prisma-ts-mode
  :ensure t
  :mode ("\\.prisma\\'" . prisma-ts-mode))

(use-package docker-compose-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/mySnippets"
	  "~/.emacs.d/snippets"
	  ))
  (define-key yas-minor-mode-map (kbd "C-c y i") 'yas-insert-snippet)
  (define-key yas-minor-mode-map (kbd "C-c y n") 'yas-new-snippet)
  (define-key yas-minor-mode-map (kbd "C-c y v") 'yas-visit-snippet-file)
  (yas-global-mode 1)
  )

(use-package poly-markdown
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode)))

(use-package haskell-mode
  :ensure t
  :init
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
    (setq haskell-process-args-cabal-new-repl
	  '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
    ;; (setq haskell-process-type 'cabal-new-repl)
    (setq haskell-stylish-on-save 't)
    (setq haskell-tags-on-save 't)
    )
  :config
  (progn
    (defun insert-repl-start ()
      (interactive)
      (insert ":{"))
    (defun insert-repl-end ()
      (interactive)
      (insert ":}"))
    ;; TODO replのときだけ有効にする
    (define-key haskell-mode-map (kbd "C-c <tab> s") 'insert-repl-start)
    (define-key haskell-mode-map (kbd "C-c <tab> e") 'insert-repl-end)
    )
  )



(use-package flycheck-haskell
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
  (eval-after-load 'haskell-mode-hook 'flycheck-mode)
  )


(use-package flymake-hlint
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'flymake-hlint-load))

(use-package lsp-haskell
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'lsp)
  )

(use-package php-mode
  :ensure t)

(defun reload-config ()
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

;; (vs-dark-theme)
(load-theme 'leuven-dark t)
(set-frame-font "Noto Sans Mono-8")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("34af44a659b79c9f92db13ac7776b875a8d7e1773448a8301f97c18437a822b6" "c05fd2078f02a7585cbf9c08c854fef95c5c284d52a26a71c6f7a139e2127d34" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" default))
 '(org-agenda-files
   '("/home/wasu/org/agenda.org" "/home/wasu/org/knowledge.org"))
 '(package-selected-packages
   '(php-mode haskell haskell-mode poly-markdown yasnippet dockerfile-mode docker-compose-mode yaml prisma-ts-mode cider clojure-mode clojure yas-minor-mode leuven-theme slime-company tree-sitter-langs tree-sitter prettier treesit-auto elfeed mwim path-headerline-mode path-header-mode neotree exec-path-from-shell lsp-mode go-mode request ddskk-posframe ddskk golden-ratio markdown-mode embark-consult embark marginalia consult orderless vertico biblio company-tabnine ace-window ace-jump-mode gitignore vs-dark-theme solarized-theme dashboard org-tree-slide which-key web-mode swiper flycheck magit gitignore-mode ivy rainbow-mode emojify use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
