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

;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

(unless (package-installed-p 'leaf)
  (package-install 'leaf))

(unless (package-installed-p 'leaf)
  (package-install 'leaf-convert))

(require 'leaf)
(require 'leaf-convert)

(leaf leaf-convert
  :config
  (leaf lisp-mode
	:bind ((emacs-lisp-mode-map
			("C-c C-z" . bozhidar-visit-ielm))
		   (emacs-lisp-mode-map
			("C-c C-c" . eval-defun))
		   (emacs-lisp-mode-map
			("C-c C-b" . eval-buffer))
		   (lisp-mode-map
			("C-c M-s" . hyperspec-lookup)))
	:hook ((emacs-lisp-mode-hook . eldoc-mode)
		   (emacs-lisp-mode-hook . rainbow-delimiters-mode)
		   (lisp-interaction-mode-hook . eldoc-mode)
		   (eval-expression-minibuffer-setup-hook . eldoc-mode))
	:require t
	:config
	(defun bozhidar-visit-ielm nil
	  "Switch to default `ielm' buffer.\nStart `ielm' if it's not already running."
	  (interactive)
	  (crux-start-or-switch-to 'ielm "*ielm*")))

  (leaf ielm
	:hook ((ielm-mode-hook . eldoc-mode)
		   (ielm-mode-hook . rainbow-delimiters-mode))
	:require t)

  (leaf zenburn-theme
	:ensure t
	:require t
	:config
	(load-theme 'zenburn t)))

;; highlight the current line
(global-hl-line-mode +1)

(leaf leaf-convert
  :config
  (leaf avy
	:ensure t
	:bind (("s-." . avy-goto-word-or-subword-1)
		   ("s-," . avy-goto-char))
	:config
	(with-eval-after-load 'avy
	  (setq avy-background t)))

  (leaf magit
	:ensure t
	:bind (("C-x g" . magit-status)))

  (leaf ag
	:ensure t
	:require t)

  (leaf projectile
	:ensure t
	:bind (("s-p" . projectile-command-map))
	:config
	(with-eval-after-load 'projectile
	  (setq projectile-completion-system 'ivy)
	  (projectile-global-mode 1)))

  (leaf pt
	:ensure t
	:require t)

  (leaf expand-region
	:ensure t
	:bind (("C-=" . er/expand-region)))

  (leaf elisp-slime-nav
	:ensure t
	:require t
	:config
	(dolist (hook
			 '(emacs-lisp-mode-hook ielm-mode-hook))
	  (add-hook hook #'elisp-slime-nav-mode)))

  (leaf paredit
	:ensure t
	:hook (emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook lisp-mode-hook eval-expression-minibuffer-setup-hook)
	:require t)

  (leaf paren
	:require t
	:config
	(show-paren-mode 1))

  (leaf abbrev
	:require t
	:setq ((save-abbrevs quote silently))
	:setq-default ((abbrev-mode . t)))

  (leaf uniquify
	:require t
	:setq ((uniquify-buffer-name-style quote forward)
		   (uniquify-separator . "/")
		   (uniquify-after-kill-buffer-p . t)
		   (uniquify-ignore-buffers-re . "^\\*"))))

;; saveplace remembers your location in a file when saving files
(require 'saveplace)
(leaf leaf-convert
  :commands run-ruby inf-ruby-keys
  :config
  (leaf saveplace
	:require t
	:setq-default ((save-place . t))
	:config
	(setq save-place-file (expand-file-name "saveplace" bozhidar-savefile-dir)))

  (leaf savehist
	:require t
	:config
	(setq savehist-additional-variables '(search-ring regexp-search-ring)
		  savehist-autosave-interval 60
		  savehist-file (expand-file-name "savehist" bozhidar-savefile-dir))
	(savehist-mode 1))

  (leaf recentf
	:require t
	:config
	(setq recentf-save-file (expand-file-name "recentf" bozhidar-savefile-dir)
		  recentf-max-saved-items 500
		  recentf-max-menu-items 15
		  recentf-auto-cleanup 'never)
	(recentf-mode 1))

  (leaf windmove
	:require t
	:config
	(windmove-default-keybindings))

  (leaf dired
	:pre-setq ((dired-recursive-deletes quote always)
			   (dired-recursive-copies quote always)
			   (dired-dwim-target . t))
	:init
	(put 'dired-find-alternate-file 'disabled nil)
	:require t dired-x)

  (leaf anzu
	:ensure t
	:bind (("M-%" . anzu-query-replace)
		   ("C-M-%" . anzu-query-replace-regexp))
	:config
	(with-eval-after-load 'anzu
	  (global-anzu-mode)))

  (leaf easy-kill
	:ensure t
	:bind (([remap kill-ring-save]
			. easy-kill))
	:require t)

  (leaf exec-path-from-shell
	:ensure t
	:require t
	:config
	(when (memq window-system
				'(mac ns))
	  (exec-path-from-shell-initialize)))

  (leaf move-text
	:ensure t
	:bind (([(meta shift up)]
			. move-text-up)
		   ([(meta shift down)]
			. move-text-down)))

  (leaf rainbow-delimiters
	:ensure t
	:require t)

  (leaf rainbow-mode
	:ensure t
	:hook (prog-mode-hook)
	:require t)

  (leaf whitespace
	:hook ((before-save-hook . whitespace-cleanup))
	:init
	(dolist (hook
			 '(prog-mode-hook text-mode-hook))
	  (add-hook hook #'whitespace-mode))
	:require t
	:setq ((whitespace-line-column . 80)
		   (whitespace-style quote
							 (face tabs empty trailing lines-tail))))

  (leaf inf-ruby
	:ensure t
	:hook ((ruby-mode-hook . inf-ruby-minor-mode))
	:require t)

  (add-hook 'ruby-mode-hook
			'(lambda nil
			   (inf-ruby-keys))))
(leaf leaf-convert
  :config
  (leaf ruby-mode
	:hook ((ruby-mode-hook . subword-mode))
	:require t
	:config
	(electric-pair-mode t)
	(add-hook 'ruby-mode-hook
			  '(lambda nil
				 (robe-mode)
				 (robe-ac-setup)
				 (inf-ruby-keys))))

  (leaf clojure-mode
	:ensure t
	:bind (("C-c C-l" . cider-repl-clear-buffer))
	:hook ((clojure-mode-hook . paredit-mode)
		   (clojure-mode-hook . subword-mode)
		   (clojure-mode-hook . rainbow-delimiters-mode))
	:require t)

  (leaf sayid
	:ensure t
	:require t
	:config
	(with-eval-after-load 'clojure-mode
	  (sayid-setup-package))))

(leaf leaf-convert
  :config
  (defun my-clojure-mode-hook nil
	(clj-refactor-mode 1)
	(yas-minor-mode 1)
	(cljr-add-keybindings-with-prefix "C-c C-m"))

  (leaf clj-refactor
	:ensure t
	:hook ((clojure-mode-hook . my-clojure-mode-hook))
	:require t)

  (leaf cider
	:ensure t
	:hook ((cider-mode-hook . eldoc-mode)
		   (cider-repl-mode-hook . eldoc-mode)
		   (cider-repl-mode-hook . paredit-mode)
		   (cider-repl-mode-hook . rainbow-delimiters-mode))
	:require t)

  (leaf markdown-mode
	:ensure t
	:require t
	:setq ((markdown-command . "pandoc")))

  (leaf yaml-mode
	:ensure t
	:require t)

  (leaf cask-mode
	:ensure t
	:require t)

  (leaf company
	:ensure t
	:require t
	:config
	(global-company-mode)
	(push 'company-lsp company-backends)
	(add-hook 'go-mode-hook
			  (lambda nil
				(company-mode)
				(setq company-transformers '(company-sort-by-backend-importance))
				(setq company-idle-delay 0)
				(setq company-minimum-prefix-length 3)
				(setq company-selection-wrap-around t)
				(setq completion-ignore-case t)
				(setq company-dabbrev-downcase nil)
				(global-set-key
				 (kbd "C-M-i")
				 'company-complete)
				(define-key company-active-map
				  (kbd "C-n")
				  'company-select-next)
				(define-key company-active-map
				  (kbd "C-p")
				  'company-select-previous)
				(define-key company-active-map
				  (kbd "C-s")
				  'company-filter-candidates)
				(define-key company-active-map
				  [tab]
				  'company-complete-selection)
				(define-key emacs-lisp-mode-map
				  (kbd "C-M-i")
				  'company-complete))))

  (global-company-mode 1)
  (leaf zop-to-char
	:ensure t
	:bind (("M-z" . zop-up-to-char)
		   ("M-Z" . zop-to-char)))

  (leaf imenu-anywhere
	:ensure t
	:bind (("C-c i" . imenu-anywhere)
		   ("s-i" . imenu-anywhere)))

  (leaf flyspell
	:hook (text-mode-hook
		   (prog-mode-hook . flyspell-prog-mode))
	:require t
	:config
	(when (eq system-type 'windows-nt)
	  (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
	(setq ispell-program-name "aspell"
		  ispell-extra-args '("--sug-mode=ultra")))

  (leaf flycheck
	:ensure t
	:hook ((after-init-hook . global-flycheck-mode))
	:require t)

  (leaf super-save
	:ensure t
	:require t
	:config
	(super-save-mode 1))

  (leaf crux
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
		   ([remap move-beginning-of-line]
			. crux-move-beginning-of-line)
		   ([(shift return)]
			. crux-smart-open-line)
		   ([(control shift return)]
			. crux-smart-open-line-above)
		   ([remap kill-whole-line]
			. crux-kill-whole-line)
		   ("C-c s" . crux-ispell-word-then-abbrev)))

  (leaf diff-hl
	:ensure t
	:hook ((dired-mode-hook . diff-hl-dired-mode)
		   (magit-post-refresh-hook . diff-hl-magit-post-refresh))
	:require t
	:config
	(global-diff-hl-mode 1))

  (leaf which-key
	:ensure t
	:require t
	:config
	(which-key-mode 1))

  (leaf undo-tree
	:ensure t
	:bind (("M-/" . undo-tree-redo))
	:require t
	:setq ((undo-tree-auto-save-history . t))
	:config
	(setq undo-tree-history-directory-alist `((".*" \, temporary-file-directory)))
	(global-undo-tree-mode t))

  (leaf ivy
	:ensure t
	:bind (("C-c C-r" . ivy-resume)
		   ("<f6>" . ivy-resume))
	:require t
	:setq ((ivy-use-virtual-buffers . t)
		   (enable-recursive-minibuffers . t))
	:config
	(ivy-mode 1))

  (leaf swiper
	:ensure t
	:require t
	:config
	(global-set-key "" 'swiper))

  (leaf counsel
	:ensure t
	:bind (("M-x" . counsel-M-x)
		   ("C-x C-f" . counsel-find-file)
		   ("<f1> f" . counsel-describe-function)
		   ("<f1> v" . counsel-describe-variable)
		   ("<f1> l" . counsel-find-library)
		   ("<f2> i" . counsel-info-lookup-symbol)
		   ("<f2> u" . counsel-unicode-char)
		   ("C-c g" . counsel-git)
		   ("C-c j" . counsel-git-grep)
		   ("C-c k" . counsel-ag)
		   ("C-x l" . counsel-locate)
		   (minibuffer-local-map
			("C-r" . counsel-minibuffer-history)))
	:require t))

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

(leaf google-translate
  :ensure t
  :bind (("C-c f" . google-translate-enja-or-jaen))
  :require t)

(leaf google-translate
  :require t)

(defun google-translate-json-suggestion (json)
  "Retrieve from JSON (which returns by the
`google-translate-request' function) suggestion. This function
does matter when translating misspelled word. So instead of
translation it is possible to get suggestion."
  (let ((info (aref json 7)))
    (if (and info (> (length info) 0))
        (aref info 1)
      nil)))

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

(leaf leaf-convert
  :setq ((default-input-method . "japanese-mozc"))
  :config
  (leaf neotree
	:ensure t
	:bind (("C-c t" . neotree))
	:require t
	:setq ((neo-show-hidden-files . t)))

  (leaf elscreen
	:ensure t
	:require t
	:setq ((elscreen-tab-display-kill-screen)
		   (elscreen-tab-display-control)
		   (elscreen-buffer-to-nickname-alist quote
											  (("^dired-mode$" lambda nil
												(format "Dired(%s)" dired-directory))
											   ("^Info-mode$" lambda nil
												(format "Info(%s)"
														(file-name-nondirectory Info-current-file)))
											   ("^mew-draft-mode$" lambda nil
												(format "Mew(%s)"
														(buffer-name
														 (current-buffer))))
											   ("^mew-" . "Mew")
											   ("^irchat-" . "IRChat")
											   ("^liece-" . "Liece")
											   ("^lookup-" . "Lookup")))
		   (elscreen-mode-to-nickname-alist quote
											(("[Ss]hell" . "shell")
											 ("compilation" . "compile")
											 ("-telnet" . "telnet")
											 ("dict" . "OnlineDict")
											 ("*WL:Message*" . "Wanderlust"))))
	:config
	(setq elscreen-prefix-key (kbd "C-z"))
	(elscreen-start))

  (leaf mozc
	:ensure t
	:require t
	:config
	(prefer-coding-system 'utf-8))

  (set-language-environment "Japanese")
  (leaf undo-tree
	:ensure t
	:require t)

  (leaf helm
	:ensure t
	:bind (("M-x" . helm-M-x)
		   ("M-<f5>" . helm-find-files)
		   ([f10]
			. helm-buffers-list)
		   ([S-f10]
			. helm-recentf)
		   ("C-x b" . helm-for-files)
		   ("C-x C-f" . helm-find-files)
		   ("M-y" . helm-show-kill-ring))
	:config
	(with-eval-after-load 'helm
	  (require 'helm-config)
	  (helm-mode 1))))

;;;多分先に変数いれとかないと駄目。辛い
(defvar howm-view-title-header "#")
(leaf leaf-convert
  :config
  (leaf howm
	:ensure t
	:bind (("C-c ,," . howm-menu))
	:require t
	:setq ((howm-menu-langage quote ja)
		   (howm-file-name-format . "%Y/%m/%Y-%m-%d-%H%M%S.md")
		   (howm-menu-file . "menu.md"))
	:config
	(setq howm-directory (concat user-emacs-directory "howm")))

  (leaf elfeed
	:ensure t
	:require t
	:config
	(setf elfeed-feeds '("http://b.hatena.ne.jp/hotentry/it.rss" "http://elephant.2chblog.jp/index.rdf" "http://openstandia.jp/oss_info/atom.xml" "https://www.archlinux.org/feeds/packages/")))

  (leaf w3m
	:ensure t
	:require t
	:setq ((w3m-command . "/usr/local/bin/w3m"))
	:config
	(setq w3m-coding-system 'utf-8
		  w3m-file-coding-system 'utf-8
		  w3m-file-name-coding-system 'utf-8
		  w3m-input-coding-system 'utf-8
		  w3m-output-coding-system 'utf-8
		  w3m-terminal-coding-system 'utf-8)))

(setq inferior-lisp-program "sbcl")
;; ~/.emacs.d/slimeをload-pathに追加
(unless (file-exists-p "~/.emacs.d/slime/")
  (shell-command-to-string "git clone https://github.com/slime/slime && mv -f slime ~/.emacs.d/"))
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

(leaf golden-ratio
  :ensure t
  :require t
  :setq ((golden-ratio-exclude-modes quote
									 (calendar-mode))
		 (golden-ratio-exclude-buffer-names quote
											(" *Org tags*" " *Org todo*"))
		 (golden-ratio-exclude-buffer-regexp quote
											 ("\\*anything" "\\*helm"))
		 (golden-ratio-extra-commands quote
									  (windmove-left windmove-right windmove-down windmove-up)))
  :config
  (golden-ratio-mode 1))


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

(leaf leaf-convert
  :config
  (leaf lsp-go
	:after go-mode lsp-mode
	:commands lsp lsp-go-enable
	:hook ((go-mode-hook . lsp))
	:custom ((lsp-go-language-server-flags quote
										   ("-gocodecompletion" "-diagnostics" "-lint-tool=golint"))))

  (leaf go-mode
	:ensure t
	:hook ((go-mode-hook . lsp-go-enable)
		   (go-mode-hook . flycheck-mode))
	:require t
	:config
	(add-to-list 'exec-path
				 (expand-file-name "/usr/lib/go/bin"))
	(add-to-list 'exec-path
				 (expand-file-name "~/develop/go/bin/"))
	(add-hook 'go-mode-hook
			  (lambda nil
				(add-hook 'before-save-hook ''gofmt-before-save)
				(local-set-key
				 (kbd "M-.")
				 'godef-jump)
				(set
				 (make-local-variable 'company-backends)
				 '(company-go))
				(setq indent-tabs-mode nil)
				(setq c-basic-offset 4)
				(setq tab-width 4)
				(add-hook 'before-save-hook 'gofmt-before-save)))))

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

(leaf leaf-convert
  :config
  (leaf scala-mode
	:mode ("\\.s\\(cala\\|bt\\)$"))

  (leaf sbt-mode
	:commands sbt-start sbt-command
	:config
	(with-eval-after-load 'sbt-mode
	  (substitute-key-definition 'minibuffer-complete-word 'self-insert-command minibuffer-local-completion-map)))

  (leaf flycheck
	:init
	(global-flycheck-mode)
	:require t)

  (leaf lsp-mode
	:ensure t
	:commands lsp
	:hook (scala-mode-hook)
	:config
	(with-eval-after-load 'lsp-mode
	  (setq lsp-prefer-flymake nil)))

  (leaf lsp-ui
	:require t)

  (leaf company-lsp
	:require t))

;; Timeoutの変更
(if (>= emacs-major-version 22)
    (setq anthy-accept-timeout 1))

(leaf leaf-convert
  :config
  (leaf java-imports
	:ensure t
	:require t
	:setq ((java-imports-find-block-function quote java-imports-find-place-sorted-block)))

  (leaf javadoc-lookup
	:ensure t
	:bind (("C-c C-j" . javadoc-lookup))
	:require t))

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
(leaf ess-R-data-view
  :ensure t
  :require t)
(add-to-list 'auto-mode-alist '("\\.erb$" . html-mode))
(prefer-coding-system 'utf-8)

(leaf python
  :ensure t
  :require t
  :config
  (setq auto-mode-alist (cons
						 '("\\.py$'" . python-mode)
						 auto-mode-alist)))

(defun insert-current-time()
  (interactive)
  (insert (format-time-string "%Y-%m-%d(%a) %H:%M:%S" (current-time))))

(define-key global-map (kbd "C-c d") `insert-current-time)

(leaf leaf-convert
  :config
  (leaf php-mode
	:ensure t
	:require t)

  (leaf intero
	:ensure t
	:require t)

  (leaf f
	:ensure t
	:require t)

  (leaf ht
	:ensure t
	:require t)

  (leaf eglot
	:ensure t
	:require t)

  (leaf lsp-ui
	:ensure t
	:require t)

  (leaf lsp-haskell
	:ensure t
	:require t)

  (leaf haskell-mode
	:load-path* "elpa/lsp-ui" "elpa/lsp-haskell"
	:ensure t
	:hook ((haskell-mode-hook . intero-mode)
		   (lsp-mode-hook . lsp-ui-mode)
		   (haskell-mode-hook . lsp)
		   (haskell-mode-hook . flycheck-mode))
	:require t lsp-mode lsp-ui lsp-haskell
	:config
	(flycheck-add-next-checker 'intero
							   '(warning . haskell-hlint))
	(setf flymake-allowed-file-name-masks (delete
										   '("\\.l?hs\\'" haskell-flymake-init)
										   flymake-allowed-file-name-masks)))

  (leaf lsp-ui
	:hook (lsp-mode-hook))

  (leaf company
	:require t
	:config
	(global-company-mode)
	(push 'company-lsp company-backends)))

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

(leaf leaf-convert
  :config
  (leaf python-pytest
	:ensure t
	:require t)

  (leaf robe
	:ensure t
	:require t))

(setq inferior-lisp-program "sbcl")
(add-to-list 'load-path "~/.emacs.d/slime/")
(require 'slime)
(slime-setup)

(load (expand-file-name "~/.Roswell/helper.el"))

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

(leaf leaf-convert
  :load-path* "scala-bootstrap-el/"
  :init
  (leaf emmet-mode
	:ensure t
	:require t)

  (leaf rustic
	:ensure t
	:setq ((rustic-rls-pkg quote eglot))
	:config
	(with-eval-after-load 'rustic
	  (add-to-list 'auto-mode-alist
				   '("\\.rs\\'" . rust-mode))
	  (add-hook 'rust-mode-hook #'racer-mode)
	  (add-hook 'racer-mode-hook #'eldoc-mode)))

  (leaf quickrun
	:ensure t)

  (leaf racer
	:ensure t)

  (leaf typescript-mode
	:ensure t
	:mode ("\\.ts\\'" "\\.tsx\\'")
	:require t)

  (leaf ensime
	:ensure t
	:hook (java-mode-hook)
	:config
	(with-eval-after-load 'ensime
	  (add-hook 'java-mode-hook 'ensime-mode)))

  (leaf company-lsp
	:ensure t
	:require t)

  (leaf lsp-scala
	:config
	;; (use-package-ensure-elpa 'lsp-scala
	;; 						 '(t)
	;; 						 '(:demand t))
	(with-eval-after-load 'scala-mode
	  (setq lsp-scala-server-command "/usr/local/bin/metals-emacs")
	  (require 'lsp-scala nil nil)
	  (setq lsp-prefer-flymake nil)
	  (add-hook 'scala-mode-hook #'lsp)))

  ;; :require evcxr scala-bootstrap
  :config
  (add-hook 'scala-mode-hook
			'(lambda nil
			   (scala-bootstrap:with-metals-installed
				(scala-bootstrap:with-bloop-server-started
				 (lsp)))))
  (leaf sbt-mode
	:ensure t
	:commands sbt-start sbt-command)

  (leaf sbt-mode
	:commands sbt-start sbt-command
	:config
	(with-eval-after-load 'sbt-mode
	  (substitute-key-definition 'minibuffer-complete-word 'self-insert-command minibuffer-local-completion-map)))

  (leaf vue-mode
	:ensure t
	:mode ("\\.vue\\'")
	:require t)

  (leaf ace-window
	:ensure t
	:config
	(global-set-key (kbd "C-x o") '(lambda ()
									 (interactive)
									 (ace-window 0)
									 (golden-ratio)))
	:require t
	:setq ((aw-keys quote
					(97 115 100 102 103 104 106 107 108))))

  (leaf ace-jump-mode
	:ensure t
	:bind (("C-:" . ace-jump-char-mode)
		   ("C-c C-;" . ace-jump-word-mode)
		   ("C-M-;" . ace-jump-line-mode))
	:require t
	:setq ((ace-jump-word-mode-use-query-char))
	:config
	(setq ace-jump-mode-move-keys (append "asdfghjkl;:]qwertyuiop@zxcvbnm,." nil))))

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

(leaf fsharp-mode
  :ensure t
  :bind ((fsharp-mode-map
		  ("C-c C-M-f" . fsharp-fantomas-format-buffer)))
  :pre-setq ((inferior-fsharp-program . "/usr/local/share/dotnet/dotnet fsi"))
  :init
  (add-hook 'fsharp-mode-hook
			'(lambda nil
			   (lsp)))
  :require t eglot-fsharp
  :setq-default ((fsharp-indent-offset . 2)))

(leaf dotnet
  :ensure t
  :hook (fsharp-mode-hook)
  :require t)

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

(leaf helm-fish-completion
  :ensure t
  :require t)

(when (require 'helm-fish-completion nil 'noerror)
  (define-key shell-mode-map (kbd "<tab>") 'helm-fish-completion)
  (setq helm-esh-pcomplete-build-source-fn #'helm-fish-completion-make-eshell-source))

(leaf leaf-convert
  :config
  (leaf twittering-mode
	:ensure t
	:require t)

  (leaf dired-subtree
	:ensure t
	:require t
	:config
	(define-key dired-mode-map "i" 'dired-subtree-insert)
	(define-key dired-mode-map ";" 'dired-subtree-remove))

  (leaf elm-mode
	:ensure t
	:hook ((after-init-hook . global-company-mode)
		   (elm-mode-hook . company-mode))
	:init
	(add-hook 'elm-mode-hook
			  (lambda nil
				(local-set-key
				 (kbd "C-c C-l")
				 'elm-repl-load-back)))
	:require t lsp-elm
	:config
	(add-hook 'elm-mode-hook
			  (lambda nil
				(setq-local company-backends
							'(company-lsp))
				(company-mode 1)
				(eglot)))
	(with-eval-after-load 'flycheck
	  '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))))

(global-set-key (kbd "C-c C->") #'leaf-convert-region-replace)
