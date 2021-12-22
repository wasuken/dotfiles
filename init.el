(require 'package)

(setq org-agenda-files (list "~/todo/todo.org"))

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      '(("t" "TODO" entry (file+headline "~/todo/todo.org" "Inbox")
         "*** %?\n    CAPTURED_AT: %a\n    %i")))

; macOS専用処理
(when (equal system-type 'darwin)
  ; 最小化無効
  (global-set-key "\C-z" nil)
  (define-key global-map [?¥] [?\\]))

;; ウィンドウを透明にする
;; アクティブウィンドウ／非アクティブウィンドウ（alphaの値で透明度を指定）
(add-to-list 'default-frame-alist '(alpha . (0.85 0.85)))

;; メニューバーを消す
(menu-bar-mode -1)

;; ツールバーを消す
;; (tool-bar-mode -1)

;; 列数を表示する
(column-number-mode t)

;; 行数を表示する
(global-linum-mode t)
(setq linum-format "%d ")
(set-face-attribute 'linum nil
					:foreground "#6272a4"
					:height 0.9)

;; カーソルの点滅をやめる
(blink-cursor-mode 0)

;; カーソル行をハイライトする
(global-hl-line-mode t)

;; 対応する括弧を光らせる
(show-paren-mode 1)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(setq user-full-name "wasuken"
      user-mail-address "wevorence@gmail.com")

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(defconst bozhidar-savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p bozhidar-savefile-dir)
  (make-directory bozhidar-savefile-dir))

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
;; (setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 4)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)

(define-key 'help-command (kbd "C-i") #'info-display-manual)

;; misc useful keybindings
(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)
(global-set-key (kbd "s-q") #'fill-paragraph)
(global-set-key (kbd "s-x") #'execute-extended-command)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

(unless (package-installed-p 'leaf)
  (package-install 'leaf))

(require 'leaf)

(add-to-list 'load-path "~/dotfiles/me")
(load "custom.el")
(load "keybind.el")

(electric-pair-mode 1)

(leaf doom-themes
  :ensure t
  :require t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-jump-buffer all-the-icons smartparens ace-jump docker-compose-mode mwim dockerfile docker-compose nwim side-hustle orderless vertico smartparen doom-themes zop-to-char zenburn-theme yaml-mode which-key web-mode w3m vue-mode use-package undo-tree typescript-mode twittering-mode twig-mode tuareg svelte-mode super-save sayid rustic robe rjsx-mode rainbow-mode rainbow-delimiters racer quickrun python-pytest pug-mode pt protobuf-mode php-mode pdf-tools parsec ocamlformat nvm neotree multi-term mozc move-text merlin magit-popup magit lsp-ui lsp-scala lsp-ruby lsp-java lsp-haskell leaf-convert julia-mode javadoc-lookup java-imports iter2 intero imenu-anywhere howm helm-fish-completion google-translate golden-ratio go-mode fsharp-mode flycheck-pos-tip flycheck-clojure fish-mode expand-region exec-path-from-shell ess-R-data-view ensime emmet-mode elscreen elm-mode elisp-slime-nav elfeed edn easy-kill dotnet dockerfile-mode dired-subtree diff-hl csharp-mode crux counsel company-lsp clj-refactor cask-mode cargo anzu ammonite-term-repl ag ace-jump-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(leaf vertico
  :ensure t
  :require t
  :init
  (vertico-mode)
  )

(leaf orderless
  :ensure t
  :require t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(leaf savehist
  :ensure t
  :require t
  :init
  (savehist-mode 1)
  (push 'compile-command savehist-additional-variables)
  (push 'command-history savehist-ignored-variables))

(leaf whitespace
  :ensure t
  :require t
  :config
  (setq whitespace-style '(face
                           trailing
                           tabs
                           spaces
                           empty
                           space-mark
                           tab-mark
                           )))

										; 画面最大化
(set-frame-parameter nil 'fullscreen 'maximized)

(leaf eglot
  :ensure t
  :require t)

(leaf phpunit
  :ensure t
  :require t)

(leaf company
  :ensure t
  :require t
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode 1))

(leaf flycheck
  :require t
  :ensure t)

(leaf lsp-mode
  :ensure t
  :require t
  :config
  (setq lsp-prefer-flymake nil)
  :hook (php-mode . lsp)
  :commands lsp)

(leaf lsp-ui
  :ensure t
  :require t
  :config
  (setq lsp-ui-doc-enable t
		lsp-ui-doc-use-childframe t
		lsp-ui-doc-include-signature t
		lsp-ui-sideline-enable nil
		lsp-ui-flycheck-enable t
		lsp-ui-flycheck-live-reporting t
		lsp-ui-peek-enable t
		lsp-ui-peek-list-width 60
		lsp-ui-peek-peek-height 25
		lsp-ui-sideline-enable nil)
  )

(leaf company-lsp
  :ensure t
  :require t
  :commands company-lsp)

(leaf mwim
  :ensure t
  :require t
  :config
  (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line))

(leaf docker-compose-mode
  :ensure t
  :require t)

(leaf smartparens
  :ensure t
  :require t
  )

(leaf ace-jump
  :config
  (setq ace-jump-word-mode-use-query-char nil)
  (global-set-key (kbd "C-:") 'ace-jump-char-mode)
  (global-set-key (kbd "C-;") 'ace-jump-word-mode)
  (global-set-key (kbd "C-M-;") 'ace-jump-line-mode))

(leaf tab-bar
  :ensure t
  :require t
  :config
  (tab-bar-mode 1))

(leaf neotree
  :ensure t
  :require t
  :config
  (define-key global-map (kbd "C-c t") #'neotree-toggle))

(leaf ace-jump-buffer
  :ensure t
  :require t
  :config
  (global-set-key (kbd "C-x j") 'ace-jump-buffer))

(leaf ace-window
  :ensure t
  :require t
  :config
  (global-set-key (kbd "C-x o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(leaf web-mode
  :ensure t
  :require t
  :mode (
		 ("\\.ts\\'" . web-mode)
		 ("\\.tsx\\'" . web-mode)
		 ("\\.js\\'" . web-mode)
		 ("\\.jsx\\'" . web-mode)
		 )
  :config
  (setq web-mode-attr-indent-offset nil)

  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)

  (setq web-mode-auto-close-style 2)
  (setq web-mode-tag-auto-close-style 2)


  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  )

(leaf exec-path-from-shell
  :ensure t
  :require t
  :config
  (when (memq window-system
			  '(mac ns))
	(exec-path-from-shell-initialize)))

(leaf )
