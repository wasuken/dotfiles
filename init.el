(require 'package)

;;; これはあかんやろ
(defun replace-sepa (left &optional right)
  (setq right (or right left))
  (let ((cur-strs  (buffer-substring (region-beginning) (region-end))))
	(delete-region (region-beginning) (region-end))
	(insert (concatenate 'string left
						 (replace-regexp-in-string ","
												   (concatenate 'string
																right
																","
																left)
												   cur-strs)
						 right))  (print right)))
(defun replace-dc ()
  (interactive)
  (replace-sepa "\""))
(defun replace-sc ()
  (interactive)
  (replace-sepa "\'"))
(defun replace-paren ()
  (interactive)
  (replace-sepa "(" ")"))
(defun insert-> ()
  (interactive)
  (insert " -> "))
(defun insert<- ()
  (interactive)
  (insert " <- "))
(defun insert=> ()
  (interactive)
  (insert " => "))
(defun insert<= ()
  (interactive)
  (insert " <= "))

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
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa2" . "http://melpa.milkbox.net/packages/") ;なぜ二つあるんだ
        ("org" . "http://orgmode.org/elpa/")))


;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(setq user-full-name "wasu ken"
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

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(use-package lisp-mode
  :config
  (defun bozhidar-visit-ielm ()
    "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
    (interactive)
    (crux-start-or-switch-to 'ielm "*ielm*"))

  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'bozhidar-visit-ielm)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
  (define-key lisp-mode-map (kbd "C-c M-s") 'hyperspec-lookup)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package ielm
  :config
  (add-hook 'ielm-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; highlight the current line
(global-hl-line-mode +1)

(use-package avy
  :ensure t
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char))
  :config
  (setq avy-background t))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package ag
  :ensure t)

(use-package projectile
  :ensure t
  :bind ("s-p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-global-mode +1))

(use-package pt
  :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package elisp-slime-nav
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode)))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package paren
  :config
  (show-paren-mode +1))

(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; saveplace remembers your location in a file when saving files
(require 'saveplace)
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" bozhidar-savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" bozhidar-savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" bozhidar-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package rainbow-delimiters
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package inf-ruby
  :ensure t
  :config
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)))

(use-package ruby-mode
  :config
  (add-hook 'ruby-mode-hook #'subword-mode)
  (electric-pair-mode t)
  (add-hook 'ruby-mode-hook
			'(lambda ()
               (robe-mode)
               (robe-ac-setup)
               (inf-ruby-keys))))

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  :bind (("C-c C-l" . 'cider-repl-clear-buffer)))

(use-package sayid :ensure t
  :config
  (eval-after-load 'clojure-mode
	'(sayid-setup-package)))

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package clj-refactor
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-command "pandoc"))

(use-package yaml-mode
  :ensure t)

(use-package cask-mode
  :ensure t)


(use-package company
  :ensure t
  :config
  (global-company-mode)
  (push 'company-lsp company-backends)
  (add-hook 'go-mode-hook (lambda()
                            (company-mode)
                            (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
                            (setq company-idle-delay 0) ; 遅延なしにすぐ表示
                            (setq company-minimum-prefix-length 3) ; デフォルトは4
                            (setq company-selection-wrap-around t) ; 候補の最後の次は先頭に戻る
                            (setq completion-ignore-case t)
                            (setq company-dabbrev-downcase nil)
                            (global-set-key (kbd "C-M-i") 'company-complete)
                            ;; C-n, C-pで補完候補を次/前の候補を選択
                            (define-key company-active-map (kbd "C-n") 'company-select-next)
                            (define-key company-active-map (kbd "C-p") 'company-select-previous)
                            (define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
                            (define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
                            (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete) ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う
                            )))

(global-company-mode +1)

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)
         ("s-i" . imenu-anywhere)))

(use-package flyspell
  :config
  (when (eq system-type 'windows-nt)
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ("M-o" . crux-smart-open-line)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c f" . crux-recentf-ido-find-file)
         ("C-M-z" . crux-indent-defun)
         ("C-c u" . crux-view-url)
         ("C-c e" . crux-eval-and-replace)
         ("C-c w" . crux-swap-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c t" . crux-visit-term-buffer)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c I" . crux-find-user-init-file)
         ("C-c S" . crux-find-shell-init-file)
         ("s-r" . crux-recentf-ido-find-file)
         ("s-j" . crux-top-join-line)
         ("C-^" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("s-o" . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ("C-c s" . crux-ispell-word-then-abbrev)))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode t)
  (global-set-key (kbd "M-/") 'undo-tree-redo))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(when (eq system-type 'windows-nt)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super) ; Left Windows key

  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super) ; Right Windows key

  (setq w32-pass-apps-to-system nil)
  (setq w32-apps-modifier 'hyper) ; Menu/App key

  (set-frame-font "Source Code Pro 12")
  (add-to-list 'exec-path "C:/Program Files/Git/bin")
  (add-to-list 'exec-path "C:/Program Files/Git/mingw64/bin")
  (setenv "PATH" (concat "C:/Program Files/Git/bin;" "C:/Program Files/Git/mingw64/bin;" (getenv "PATH")))
  ;; needed for arc-mode
  (add-to-list 'exec-path "C:/Program Files/7-Zip"))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(use-package google-translate
  :ensure t
  :config
  (global-set-key (kbd "C-c f") 'google-translate-enja-or-jaen))

(use-package google-translate)

(defun google-translate-enja-or-jaen (&optional string)
  "Translate words in region or current position. Can also specify query with C-u"
  (interactive)
  (setq string
        (cond ((stringp string) string)
              (current-prefix-arg
               (read-string "Google Translate: "))
              ((use-region-p)
               (buffer-substring (region-beginning) (region-end)))
              (t
               (thing-at-point 'word))))
  (let* ((asciip (string-match
                  (format "\\`[%s]+\\'" "[:ascii:]’“”–")
                  string)))
    (run-at-time 0.1 nil 'deactivate-mark)
    (google-translate-translate
     (if asciip "en" "ja")
     (if asciip "ja" "en")
     string)))

(bind-key "C-t" 'google-translate-enja-or-jaen)

;; Fix error of "Failed to search TKK"
(defun google-translate--get-b-d1 ()
  ;; TKK='427110.1469889687'
  (list 427110 1469889687))

(use-package neotree
  :ensure t
  :config
  (setq neo-show-hidden-files t)
  (global-set-key (kbd "C-c t") 'neotree))

(use-package elscreen
  :ensure t
  :config
  (setq elscreen-prefix-key (kbd "C-z"))
  (elscreen-start)
;;; タブの先頭に[X]を表示しない
  (setq elscreen-tab-display-kill-screen nil)
;;; header-lineの先頭に[<->]を表示しない
  (setq elscreen-tab-display-control nil)
;;; バッファ名・モード名からタブに表示させる内容を決定する(デフォルト設定)
  (setq elscreen-buffer-to-nickname-alist
        '(("^dired-mode$" .
           (lambda ()
             (format "Dired(%s)" dired-directory)))
          ("^Info-mode$" .
           (lambda ()
             (format "Info(%s)" (file-name-nondirectory Info-current-file))))
          ("^mew-draft-mode$" .
           (lambda ()
             (format "Mew(%s)" (buffer-name (current-buffer)))))
          ("^mew-" . "Mew")
          ("^irchat-" . "IRChat")
          ("^liece-" . "Liece")
          ("^lookup-" . "Lookup")))
  (setq elscreen-mode-to-nickname-alist
        '(("[Ss]hell" . "shell")
          ("compilation" . "compile")
          ("-telnet" . "telnet")
          ("dict" . "OnlineDict")
          ("*WL:Message*" . "Wanderlust"))))


(use-package mozc
  :ensure t
  :config
  (prefer-coding-system 'utf-8))
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")

(use-package undo-tree
  :ensure t
  :config)

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("M-<f5>" . helm-find-files)
         ([f10] . helm-buffers-list)
         ([S-f10] . helm-recentf)
         ("C-x b" . helm-for-files)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring))
  :config
  (require 'helm-config)
  (helm-mode 1))

;;;多分先に変数いれとかないと駄目。辛い
(defvar howm-view-title-header "#")
(use-package howm
  :ensure t
  :config
  (setq howm-directory (concat user-emacs-directory "howm"))
  (setq howm-menu-langage 'ja)
  (setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.md")
  (setq howm-menu-file "menu.md")
  (define-key global-map (kbd "C-c ,,") 'howm-menu))

(use-package elfeed
  :ensure t
  :config
  (setf elfeed-feeds
        '("http://b.hatena.ne.jp/hotentry/it.rss"
          "http://elephant.2chblog.jp/index.rdf"
          "http://openstandia.jp/oss_info/atom.xml"
          "https://www.archlinux.org/feeds/packages/")))
(use-package w3m
  :ensure t
  :config
  (setq w3m-coding-system 'utf-8
        w3m-file-coding-system 'utf-8
        w3m-file-name-coding-system 'utf-8
        w3m-input-coding-system 'utf-8
        w3m-output-coding-system 'utf-8
        w3m-terminal-coding-system 'utf-8)
  (setq w3m-command "/usr/local/bin/w3m"))

;;; use-package では駄目だったり、後から追加したもの
(setq inferior-lisp-program "sbcl")
;; ~/.emacs.d/slimeをload-pathに追加
(unless (file-exists-p "~/.emacs.d/slime/")
  (shell-command-to-string "git clone https://github.com/slime/slime && mv -f evcxr-mode ~/.emacs.d/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/slime"))
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner))
(add-hook 'slime-repl-mode-hook #'paredit-mode)

;;backspace h
(keyboard-translate ?\C-h ?\C-?)
(global-set-key (kbd "C-h") 'delete-backward-char)
(define-key global-map (kbd "C-x ?") 'help-command)
;;;別のやつはC-c sなので注意
(define-key global-map (kbd "C-c h") 'shell)

;;; バッファ移動。
(cond ((equal system-type 'darwin)
	   (global-set-key (kbd "C->") 'other-window)
	   (global-set-key (kbd "C-<") (lambda () (interactive) (other-window -1))))
	  (t
	   (global-set-key (kbd "C->") 'other-window)
	   (global-set-key (kbd "C-<") (lambda () (interactive) (other-window -1)))))

(use-package golden-ratio
  :ensure t
  :config
  ;;; use-packageでの設定がよくわからなかった
  (golden-ratio-mode 1)
;;; 条件に応じてウィンドウの大きさを変更しない
  ;; ウィンドウの大きさを変更しないメジャーモード
  (setq golden-ratio-exclude-modes '(calendar-mode))
  ;; ウィンドウの大きさを変更しないバッファ名
  (setq golden-ratio-exclude-buffer-names '(" *Org tags*" " *Org todo*"))
  ;; ウィンドウの大きさを変更しないバッファ名の正規表現
  (setq golden-ratio-exclude-buffer-regexp '("\\*anything" "\\*helm"))

;;; ウィンドウ選択系のコマンドで作用させる
  (setq golden-ratio-extra-commands
		'(windmove-left windmove-right windmove-down windmove-up))
  )


(unless (file-exists-p "~/.emacs.d/Emacs-gosh-mode/")
  (shell-command-to-string "git clone https://github.com/mhayashi1120/Emacs-gosh-mode && mv -f Emacs-gosh-mode ~/.emacs.d/"))

;; UTF-8 に統一
(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))

(setq scheme-program-name "gosh -i")
;; (autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
;; (autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

(setq load-path (cons "~/.emacs.d/Emacs-gosh-mode/" load-path))
(require 'gosh-config)
;;; こいつはgit cloneしてきて、sudo make installしていれたので
;;; 最初は動かないので注意
(add-to-list 'auto-mode-alist '("\\.scm$'" . gosh-mode))

(add-hook 'gosh-mode-hook
          '(lambda ()
             (paredit-mode)
             (electric-pair-mode t)))

(define-key global-map (kbd "C-c g") 'gosh-run)

(use-package lsp-go
  :after (lsp-mode go-mode)
  :custom (lsp-go-language-server-flags '(
										  "-gocodecompletion"
										  "-diagnostics"
										  "-lint-tool=golint"))
  :hook (go-mode . lsp)
  :commands lsp-go-enable)

;;; golang setting
(use-package go-mode
  :ensure t
  :config
  ;; Goのパスを通す
  (add-to-list 'exec-path (expand-file-name "/usr/lib/go/bin"))
  ;; go get で入れたツールのパスを通す
  (add-to-list 'exec-path (expand-file-name "~/develop/go/bin/"))
  (add-hook 'go-mode-hook #'lsp-go-enable)
  ;; flycheck-modeを有効化してシンタックスエラーを検知
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'go-mode-hook (lambda()
                            (add-hook 'before-save-hook' 'gofmt-before-save)
                            (local-set-key (kbd "M-.") 'godef-jump)
                            (set (make-local-variable 'company-backends) '(company-go))
                            (setq indent-tabs-mode nil)    ; タブを利用
                            (setq c-basic-offset 4)    ; tabサイズを4にする
                            (setq tab-width 4)
							(add-hook 'before-save-hook 'gofmt-before-save))))

(defun godoc-get-buffer (query)
  "Get an empty buffer for a godoc QUERY."
  (let* ((buffer-name "*godoc*")
         (buffer (get-buffer buffer-name)))
    ;; Kill the existing buffer if it already exists.
    (when buffer (kill-buffer buffer))
    (get-buffer-create buffer-name)))


;;; java settings
(setenv "JDK_HOME" "/usr/lib/jvm/java-8-openjdk/")
(setenv "JAVA_HOME" "/usr/lib/jvm/java-8-openjdk/")
;;; go settings
(setenv "GOPATH" "/Users/takedamasaru/develop/go")

(put 'upcase-region 'disabled nil)

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  ;; Optional - enable lsp-mode automatically in scala files
  :hook (scala-mode . lsp)
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui)

;; Add company-lsp backend for metals
(use-package company-lsp)

;; Timeoutの変更
(if (>= emacs-major-version 22)
    (setq anthy-accept-timeout 1))

(use-package java-imports
  :ensure t
  :config
  ;; (define-key java-mode-map (kbd "M-I") 'java-imports-add-import-dwim)
  (setq java-imports-find-block-function 'java-imports-find-place-sorted-block))

(use-package javadoc-lookup
  :ensure t
  :config
  (global-set-key (kbd "C-c C-j") 'javadoc-lookup))

(add-hook 'shell-mode-hook
          (lambda ()
            (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))

;;; R言語の設定
(setq load-path (cons "/usr/share/emacs/site-lisp/ess" load-path))
(when (locate-library "ess-site")
  (require 'ess-site))

(setq auto-mode-alist
      (cons (cons "\\.[rR]$" 'R-mode) auto-mode-alist))
(autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)


;; R 起動時にワーキングディレクトリを訊ねない
(setq ess-ask-for-ess-directory nil)

(add-to-list 'auto-mode-alist '("\\.[rR]$" . R-mode))
(global-set-key (kbd "C-M-<") 'insert<-)

;; R-mode を起動する時に ess-site をロード
(autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)

;; R を起動する時に ess-site をロード
(autoload 'R "ess-site" "start R" t)

;; R-mode, iESS を起動する際に呼び出す初期化関数
(setq ess-loaded-p nil)
(defun ess-load-hook (&optional from-iess-p)
  ;; インデントの幅を 2 にする（デフォルト 2）
  (setq ess-indent-level 2)
  ;; インデントを調整
  (setq ess-arg-function-offset-new-line (list ess-indent-level))
  ;; comment-region のコメントアウトに # を使う（デフォルト##）
  (make-variable-buffer-local 'comment-add)
  (setq comment-add 0)

  ;; 最初に ESS を呼び出した時の処理
  (when (not ess-loaded-p)
    ;; 補完機能を有効にする
    (setq ess-use-auto-complete t)
    ;; helm を使いたいので IDO は邪魔
    (setq ess-use-ido nil)
    ;; キャレットがシンボル上にある場合にもエコーエリアにヘルプを表示する
    (setq ess-eldoc-show-on-symbol t)
    ;; 起動時にワーキングディレクトリを尋ねられないようにする
    (setq ess-ask-for-ess-directory nil)
    ;; # の数によってコメントのインデントの挙動が変わるのを無効にする
    (setq ess-fancy-comments nil)
    (setq ess-loaded-p t)
    (unless from-iess-p
      ;; ウィンドウが 1 つの状態で *.R を開いた場合はウィンドウを横に分割して R を表示する
      (when (one-window-p)
        (split-window-below)
        (let ((buf (current-buffer)))
          (ess-switch-to-ESS nil)
          (switch-to-buffer-other-window buf)))
      ;; R を起動する前だと auto-complete-mode が off になるので自前で on にする
      ;; cf. ess.el の ess-load-extras
      (when (and ess-use-auto-complete (require 'auto-complete nil t))
        (add-to-list 'ac-modes 'ess-mode)
        (mapcar (lambda (el) (add-to-list 'ac-trigger-commands el))
                '(ess-smart-comma smart-operator-comma skeleton-pair-insert-maybe))
        ;; auto-complete のソースを追加
        (setq ac-sources '(ac-source-acr
                           ac-source-R
                           ac-source-filename
                           ac-source-yasnippet)))))

  (if from-iess-p
      ;; R のプロセスが他になければウィンドウを分割する
      (if (> (length ess-process-name-list) 0)
          (when (one-window-p)
            (split-window-horizontally)
            (other-window 1)))
    ;; *.R と R のプロセスを結びつける
    ;; これをしておかないと補完などの便利な機能が使えない
    (ess-force-buffer-current "Process to load into: ")))

;; R-mode 起動直後の処理
(add-hook 'R-mode-hook 'ess-load-hook)
(use-package ess-R-data-view
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.erb$" . html-mode))
(prefer-coding-system 'utf-8)

(use-package python
  :ensure t
  :config
  (setq auto-mode-alist (cons '("\\.py$'" . python-mode) auto-mode-alist)))

(defun insert-current-time()
  (interactive)
  (insert (format-time-string "%Y-%m-%d(%a) %H:%M:%S" (current-time))))

(define-key global-map (kbd "C-c d") `insert-current-time)

(use-package php-mode
  :ensure t
  :config)

(use-package intero
  :ensure t)

;;; lsp-mode用
(use-package f
  :ensure t)
;;; lsp-mode用
(use-package ht
  :ensure t)

(use-package eglot
  :ensure t)
(use-package lsp-ui
  :ensure t)
(use-package lsp-haskell
  :ensure t)
(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'intero-mode)
  (add-to-list 'load-path "~/.emacs.d/elpa/lsp-ui")
  (add-to-list 'load-path "~/.emacs.d/elpa/lsp-haskell")
  (require 'lsp-mode)
  (require 'lsp-ui)
  (require 'lsp-haskell)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint))
  ;; (add-hook 'haskell-mode-hook 'eglot-ensure)
  (setf flymake-allowed-file-name-masks
		(delete '("\\.l?hs\\'" haskell-flymake-init)
				flymake-allowed-file-name-masks))
  )


(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package company
  :config
  (global-company-mode)
  (push 'company-lsp company-backends))

(define-key haskell-mode-map (kbd "C-c C->") 'insert->)
(define-key haskell-mode-map (kbd "C-c C-<") 'insert<-)
(define-key haskell-mode-map (kbd "C-c >") 'insert=>)
(define-key haskell-mode-map (kbd "C-c <") 'insert<=)

(define-key global-map (kbd "C-\"") 'replace-dc)
(define-key global-map (kbd "C-\'") 'replace-sc)
(define-key global-map (kbd "C-c (") 'replace-paren)

(defun rep-comma-to-commaspace ()
  (interactive)
  (let* ((cur-str  (buffer-substring (region-beginning) (region-end)))
		 (insert-str (replace-regexp-in-string ","
											   ", "
											   cur-str)))
	(delete-region (region-beginning) (region-end))
	(insert (replace-regexp-in-string ", +"
									  ", "
									  insert-str))))

(define-key global-map (kbd "C-, SPC") 'rep-comma-to-commaspace)

(use-package python-pytest
  :ensure t
  :config)

(use-package robe
  :ensure t
  :config)

(setq inferior-lisp-program "sbcl")
(add-to-list 'load-path "~/.emacs.d/slime/")
(require 'slime)
(slime-setup)

(load (expand-file-name "~/.roswell/helper.el"))

;;; OSX用の設定
(cond ((not (equal system-type 'darwin))
	   ;; anthy.el をロードする
	   (load-library "anthy")
	   (setq default-input-method "japanese-anthy")
	   (autoload 'boiling-rK-trans "boiling-anthy" "romaji-kanji conversion" t)
	   (autoload 'boiling-rhkR-trans "boiling-anthy" "romaji-kana conversion" t)
	   (global-set-key "\C-t" 'boiling-rK-trans)
	   (global-set-key "\M-t" 'boiling-rhkR-trans)))

;;; 演算子の左右に空白が一つ存在している状態にする
(defun one-space-left-and-right-operator ()
  (interactive)
  (let* ((cur-str (buffer-substring (region-beginning) (region-end))))
	(delete-region (region-beginning) (region-end))
	(insert (replace-regexp-in-string "\\([-¥+¥*/=<>]+\\)- \\([0-9]+\\)" "\\1 -\\2"
									  (replace-regexp-in-string "\s*\\([-¥+¥*/=<>]+\\)\s*" " \\1 " cur-str)))))

(defun replace-space ()
  (interactive)
  (let* ((cur-str (buffer-substring (region-beginning) (region-end))))
	(delete-region (region-beginning) (region-end))
	(insert (replace-regexp-in-string
			 "\s+" "" cur-str))))

(define-key global-map (kbd "C-c C-d SPC") 'replace-space)
(define-key global-map (kbd "C-c SPC") 'one-space-left-and-right-operator)
;; バックスラッシュ入力用
(define-key global-map (kbd "M-¥") '(lambda ()
									  (interactive)
									  (insert "\\")))

(define-key global-map (kbd "C-c C-e m") 'emmet-expand-line)
(define-key global-map (kbd "C-c m") 'emmet-expand-line)

(use-package rjsx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode)))

(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'eglot))

(use-package parsec
  :ensure t)

(use-package hydra
  :ensure t)

(unless (file-exists-p "~/.emacs.d/elpa/evcxr-mode/")
  (shell-command-to-string "git clone https://github.com/SerialDev/evcxr-mode && mv -f evcxr-mode ~/.emacs.d/elpa/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/evcxr-mode/"))
(require 'evcxr)

(use-package emmet-mode
  :ensure t)

(use-package rustic
  :ensure t
  :defer t
  :init
  (setq rustic-rls-pkg 'eglot)
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'"  . rust-mode))
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package quickrun
  :defer t
  :ensure t)
(use-package racer
  :defer t
  :ensure t)

(use-package typescript-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode)))

(use-package ensime
  :ensure t
  :config
  ;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
  (add-hook 'java-mode-hook 'ensime-mode)
  :hook (java-mode . ensime-mode))

(use-package company-lsp
  :ensure t)

(use-package lsp-scala
  :ensure t
  :after scala-mode
  :demand t
  :hook (scala-mode . lsp)
  :config (setq lsp-prefer-flymake nil)
  :init (setq lsp-scala-server-command "/usr/local/bin/metals-emacs"))

(add-to-list 'load-path "~/.emacs.d/scala-bootstrap-el/")
(require 'scala-bootstrap)

(add-hook 'scala-mode-hook
          '(lambda ()
             (scala-bootstrap:with-metals-installed
              (scala-bootstrap:with-bloop-server-started
               (lsp)))
			 ))

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command)

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package vue-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode)))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") '(lambda ()
								   (interactive)
								   (ace-window 0)
								   (golden-ratio)))
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package ace-jump-mode
  :ensure t
  :config
  (setq ace-jump-mode-move-keys
		(append "asdfghjkl;:]qwertyuiop@zxcvbnm,." nil))
  (setq ace-jump-word-mode-use-query-char nil)
  (global-set-key (kbd "C-:") 'ace-jump-char-mode)
  (global-set-key (kbd "C-c C-;") 'ace-jump-word-mode)
  (global-set-key (kbd "C-M-;") 'ace-jump-line-mode))

(global-set-key (kbd "C-c C-M-s") '(lambda ()
									 (interactive)
									 (split-window-right)
									 (split-window-right)))

(put 'downcase-region 'disabled nil)
(toggle-frame-maximized)

(defun fsharp-fantomas-format-region (start end)
  (interactive "r")
  (let ((source (shell-quote-argument (buffer-substring-no-properties start end)))
        (ok-buffer "*fantomas*")
        (error-buffer "*fantomas-errors*"))
    (save-window-excursion
      (shell-command-on-region
       start end (format "fantomas --indent 2 --pageWidth 99 --stdin %s --stdout" source)
       ok-buffer nil error-buffer)
      (if (get-buffer error-buffer)
          (progn
            (kill-buffer error-buffer)
            (message "Can't format region."))
        (delete-region start end)
        (insert (with-current-buffer ok-buffer
                  (s-chomp (buffer-string))))
        (delete-trailing-whitespace)
        (message "Region formatted.")))))

(defun fsharp-fantomas-format-buffer ()
  (interactive)
  (let ((origin (point)))
    (fsharp-fantomas-format-region (point-min) (point-max))
    (goto-char origin)))

(defun fsharp-save ()
  (interactive)
  (when (string= major-mode "fsharp-mode")
	(fsharp-fantomas-format-buffer))
  (save-buffers))

(use-package fsharp-mode
  :ensure t
  :config
  (setq-default fsharp-indent-offset 2)
  (setq inferior-fsharp-program "/usr/local/share/dotnet/dotnet fsi")
  (add-hook 'fsharp-mode-hook '(lambda () (eglot)))
  (define-key fsharp-mode-map (kbd "C-c C-M-f") #'fsharp-fantomas-format-buffer)
  (require 'eglot-fsharp))

(use-package dotnet
  :ensure t
  :config
  (add-hook 'fsharp-mode-hook #'dotnet-mode))

(defun eshell-clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defvar *create-md-link-url* "")

(defun create-md-link ()
  (interactive)
  (setq *create-md-link-url* (read-string "url: "))
  (request
	  *create-md-link-url*
	  :parser 'buffer-string
	  :error (function* (lambda (&key error-thrown &allow-other-keys&rest _)
						  (message "Got error: %S" error-thrown)))
	  :success (function*
				(lambda (&key data &allow-other-keys)
				  (string-match "<title>\\(.*?\\)</title>" data)
				  (insert (format "[%s](%s)" (match-string 1 data) *create-md-link-url*))))))

(global-set-key (kbd "C-c l") #'create-md-link)

(defun jq-format (beg end)
  (interactive "r")
  (shell-command-on-region beg end "jq ." nil t))

(use-package helm-fish-completion
  :ensure t)

(when (require 'helm-fish-completion nil 'noerror)
  (define-key shell-mode-map (kbd "<tab>") 'helm-fish-completion)
  (setq helm-esh-pcomplete-build-source-fn #'helm-fish-completion-make-eshell-source))
