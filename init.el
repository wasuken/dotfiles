(require 'package)

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(unless (package-installed-p 'bind-key)
  (package-refresh-contents)
  (package-install 'bind-key))

(require 'bind-key)

(add-to-list 'default-frame-alist '(alpha . (0.85 0.85)))

(menu-bar-mode -1)

(column-number-mode t)

(global-linum-mode t)
(defvar linum-format "%d ")
(set-face-attribute 'linum nil
					:foreground "#6272a4"
					:height 0.9)

(blink-cursor-mode 0)

(global-hl-line-mode t)

(show-paren-mode 1)

(setq user-full-name "wasu ken"
      user-mail-address "wevorence@gmail.com")

(setq load-prefer-newer t)

(setq gc-cons-threshold 50000000)

(setq large-file-warning-threshold 100000000)

(defconst bozhidar-savefile-dir (expand-file-name "savefile" user-emacs-directory))

(unless (file-exists-p bozhidar-savefile-dir)
  (make-directory bozhidar-savefile-dir))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(blink-cursor-mode -1)

(setq ring-bell-function 'ignore)

(setq inhibit-startup-screen t)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq frame-title-format "%f")

(setq-default tab-width 4)
(setq require-final-newline t)

(delete-selection-mode t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-auto-revert-mode t)

(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

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

;;; 省略とかを展開してくれるやつ。
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

(global-set-key (kbd "C-x C-b") #'ibuffer)

;;; 正規表現で見た目整えてくれるやつ
(global-set-key (kbd "C-x \\") #'align-regexp)

(define-key 'help-command (kbd "C-i") #'info-display-manual)

;; misc useful keybindings
(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)
(global-set-key (kbd "s-q") #'fill-paragraph)
(global-set-key (kbd "s-x") #'execute-extended-command)

;;; タブで補完までしてくれるやつ
(setq tab-always-indent 'complete)

(defun g-set-keys (a-lst)
  (cl-loop for x in a-lst
		   do (let ((key (if (stringp (car x))
							 (kbd (car x))
						   (car x))))
				(global-set-key key (cdr x)))))

(defun set-map-keys (a-lst)
  (cl-loop for x in a-lst
		   do (let* ((map (car x))
					 (key-f (cadr x))
					 (key (car key-f))
					 (f (cdr key-f)))
				(define-key map (kbd key) f))))

(leaf request
  :ensure t
  :require t)

(setq elmo-imap4-default-server "imap.mail.yahoo.co.jp"
	  elmo-imap4-default-user "abkt_god@yahoo.co.jp"
	  elmo-imap4-default-authenticate-type 'clear
	  elmo-imap4-default-port '993
	  elmo-imap4-default-stream-type 'ssl)

(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

(defun set-hooks (a-lst)
  (cl-loop for x in a-lst
		   do (add-hook (car x) (cdr x))))

(leaf lisp-mode
  :defvar emacs-lisp-mode-map
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

;;; Emacs Lispの対話環境
(leaf ielm
  :ensure t
  :config
  (set-hooks '((ielm-mode-hook . eldoc-mode)
			   (ielm-mode-hook . rainbow-delimiters-mode)))
  :require t)

;; highlight the current line
(global-hl-line-mode +1)

(leaf avy
  :ensure t
  :config
  (with-eval-after-load 'avy
	(setq avy-background t))
  (g-set-keys '(("C-c a w" . avy-goto-word-1)
				("C-c a c" . avy-goto-char)))
  )

(leaf magit
  :ensure t
  :config
  (g-set-keys '(("C-x g" . magit-status)
				("C-c p" . magit-push-to-remote))))

(leaf ag
  :ensure t
  :require t)

(leaf projectile
  :ensure t
  :config
  (with-eval-after-load 'projectile
	(setq projectile-completion-system 'ivy)
	(projectile-global-mode 1))
  (g-set-keys '(("M-p" . projectile-command-map))))

(leaf pt
  :ensure t
  :require t)

(leaf expand-region
  :ensure t
  :config (g-set-keys '(("C-=" . er/expand-region))))

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
  :ensure t
  :require t
  :config
  (show-paren-mode 1))

;;; 同じ名前の別階層のファイルをわかりやすくするやつ
(leaf uniquify
  :require t
  :setq '((uniquify-buffer-name-style quote forward)
		  (uniquify-separator . "/")
		  (uniquify-after-kill-buffer-p . t)
		  (uniquify-ignore-buffers-re . "^\\*")))

;;; 以前開いた位置でスタートしてくれるやつ
(leaf saveplace
  :ensure t
  :require t
  :setq-default ((save-place . t))
  :config
  (setq save-place-file (expand-file-name "saveplace" bozhidar-savefile-dir)))

;;; ミニバッファの履歴を保存してくれるやつ
(leaf savehist
  :ensure t
  :require t
  :config
  (setq savehist-additional-variables '(search-ring regexp-search-ring)
		savehist-autosave-interval 60
		savehist-file (expand-file-name "savehist" bozhidar-savefile-dir))
  (savehist-mode 1))

(leaf recentf
  :ensure t
  :require t
  :config
  (setq recentf-save-file (expand-file-name "recentf" bozhidar-savefile-dir)
		recentf-max-saved-items 500
		recentf-max-menu-items 15
		recentf-auto-cleanup 'never)
  (recentf-mode 1))

(leaf windmove
  :ensure t
  :require t
  :config
  (windmove-default-keybindings))

(leaf dired
  :pre-setq '((dired-recursive-deletes quote always)
			  (dired-recursive-copies quote always)
			  (dired-dwim-target . t))
  :init
  (put 'dired-find-alternate-file 'disabled nil)
  :require t dired-x)

(leaf anzu
  :ensure t
  :config
  (g-set-keys '(("M-%" . anzu-query-replace)
				("C-M-%" . anzu-query-replace-regexp)))
  (with-eval-after-load 'anzu
	(global-anzu-mode)))

(leaf easy-kill
  :ensure t
  ;; なんこれ
  :config (g-set-keys ' (([remap kill-ring-save]
						  . easy-kill)))
  :require t)

(leaf exec-path-from-shell
  :ensure t
  :require t
  :config
  (exec-path-from-shell-initialize))

(leaf move-text
  :ensure t
  :config
  (g-set-keys '(([(meta shift up)]
				 . move-text-up)
				([(meta shift down)]
				 . move-text-down)))
  )

(leaf rainbow-delimiters
  :ensure t
  :require t)

(leaf rainbow-mode
  :ensure t
  :hook (prog-mode-hook)
  :require t)

(leaf whitespace
  :ensure t
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
  :require t
  :config
  (set-hooks '((clojure-mode-hook . paredit-mode)
			   (clojure-mode-hook . subword-mode)
			   (clojure-mode-hook . rainbow-delimiters-mode)))
  (g-set-keys '(("C-c C-l" . cider-repl-clear-buffer))))

(leaf sayid
  :ensure t
  :require t
  :config
  (with-eval-after-load 'clojure-mode
	(sayid-setup-package)))

(leaf neotree
  :ensure t
  :require t
  :setq ((neo-show-hidden-files . t))
  :config
  (g-set-keys '(("C-c t" . neotree))))

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

(leaf undo-tree
  :ensure t
  :require t)

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
  :config (set-hooks '((cider-mode-hook . eldoc-mode)
					   (cider-repl-mode-hook . eldoc-mode)
					   (cider-repl-mode-hook . paredit-mode)
					   (cider-repl-mode-hook . rainbow-delimiters-mode)))
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
  :config (g-set-keys '(("M-z" . zop-up-to-char)
						("M-Z" . zop-to-char))))

(leaf imenu-anywhere
  :ensure t
  :config (g-set-keys '(("C-c i" . imenu-anywhere)
						("s-i" . imenu-anywhere))))

(leaf flyspell
  :hook '(text-mode-hook
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
  :config
  (g-set-keys '(("C-c o" . crux-open-with)
				("M-o" . crux-smart-open-line)
				("C-c n" . crux-cleanup-buffer-or-region)
				("C-c f" . crux-recentf-ido-find-file)
				("C-M-z" . crux-indent-defun)
				("C-c u" . crux-view-url)
				("C-c e" . crux-eval-and-replace)
				("C-c w" . crux-swap-windows)
				("C-c D" . crux-delete-file-and-buffer)
				("C-c r" . crux-rename-buffer-and-file)
				;; ("C-c t" . crux-visit-term-buffer)
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
)

(leaf diff-hl
  :ensure t
  :require t
  :config
  (set-hooks '((dired-mode-hook . diff-hl-dired-mode)
			   (magit-post-refresh-hook . diff-hl-magit-post-refresh)))
  (global-diff-hl-mode 1))

(leaf which-key
  :ensure t
  :require t
  :config
  (which-key-mode 1))

(leaf undo-tree
  :ensure t
  :require t
  :setq ((undo-tree-auto-save-history . t))
  :config
  (g-set-keys '(("M-/" . undo-tree-redo)))
  (setq undo-tree-history-directory-alist `((".*" \, temporary-file-directory)))
  (global-undo-tree-mode t))

(leaf ivy
  :ensure t
  :require t
  :setq '((ivy-use-virtual-buffers . t)
		  (enable-recursive-minibuffers . t))
  :config
  (g-set-keys '(("C-c C-r" . ivy-resume)
				("<f6>" . ivy-resume)))
  (ivy-mode 1)
  (setq ivy-virtual-abbreviate 'full)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 30)
  (setq ivy-extra-directories nil)
  (setq ivy-re-builders-alist
		'((t . ivy--regex-plus)))
  (define-key ivy-minibuffer-map [tab] 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-g") 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)
  (setq ivy-wrap t))

(load (expand-file-name "~/.emacs.d/doom/load.el"))

(leaf ivy-rich
  :ensure t 'all-the-icons-ivy 'all-the-icons
  :hook '(ivy-mode-hook . ivy-rich-mode)
  :preface
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
	  (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode)
		  icon))))
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-rich--display-transformers-list
        '(ivy-switch-buffer
		  (:columns
		   ((ivy-rich-switch-buffer-icon :width 2)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
		   :predicate
		   (lambda (cand) (get-buffer cand)))))
  )


(leaf swiper
  :ensure t
  :require t
  :config
  (global-set-key "" 'swiper))

(leaf counsel
  :ensure t
  :config
  (g-set-keys '(("M-x" . counsel-M-x)
				("C-x C-f" . counsel-find-file)
				("<f1> f" . counsel-describe-function)
				("<f1> v" . counsel-describe-variable)
				("<f1> l" . counsel-find-library)
				("<f2> i" . counsel-info-lookup-symbol)
				("<f2> u" . counsel-unicode-char)
				("C-c g" . counsel-git)
				("C-c j" . counsel-git-grep)
				("C-c k" . counsel-ag)
				("C-x l" . counsel-locate)))
  :require t)


;; (minibuffer-local-map
;;  ("C-r" . counsel-minibuffer-history))

(set-frame-font "hackgen-11")

(when (eq system-type 'windows-nt)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super) ; Left Windows key

  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super) ; Right Windows key

  (setq w32-pass-apps-to-system nil)
  (setq w32-apps-modifier 'hyper) ; Menu/App key

  (add-to-list 'exec-path "C:/Program Files/Git/bin")
  (add-to-list 'exec-path "C:/Program Files/Git/mingw64/bin")
  (setenv "PATH" (concat "C:/Program Files/Git/bin;" "C:/Program Files/Git/mingw64/bin;" (getenv "PATH")))
  ;; needed for arc-mode
  (add-to-list 'exec-path "C:/Program Files/7-Zip"))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

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

(leaf google-translate
  :ensure t
  :config (g-set-keys '(("C-c f" . google-translate-enja-or-jaen)))
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

;; Fix error of "Failed to search TKK"
(defun google-translate--get-b-d1 ()
  ;; TKK='427110.1469889687'
  (list 427110 1469889687))

(global-set-key (kbd "M-y") 'counsel-yank-pop)

;; (leaf elfeed
;;   :ensure t
;;   :require t
;;   :config
;;   (setf elfeed-feeds '("http://b.hatena.ne.jp/hotentry/it.rss" "http://elephant.2chblog.jp/index.rdf" "http://openstandia.jp/oss_info/atom.xml" "https://www.archlinux.org/feeds/packages/")))

(leaf w3m
  :ensure t
  :require t
  :setq ((w3m-command . "/usr/bin/w3m"))
  :config
  (setq w3m-coding-system 'utf-8
		w3m-file-coding-system 'utf-8
		w3m-file-name-coding-system 'utf-8
		w3m-input-coding-system 'utf-8
		w3m-output-coding-system 'utf-8
		w3m-terminal-coding-system 'utf-8))

(setq inferior-lisp-program "sbcl")
;; ~/.emacs.d/slimeをload-pathに追加
(unless (file-exists-p "~/.emacs.d/slime/")
  (shell-command-to-string "git clone https://github.com/slime/slime && mv -f slime ~/.emacs.d/"))
(cond ((file-exists-p "~/.emacs.d/slime")
	   (add-to-list 'load-path (expand-file-name "~/.emacs.d/slime"))
	   (require 'slime)
	   (slime-setup '(slime-repl slime-fancy slime-banner))
	   (add-hook 'slime-repl-mode-hook #'paredit-mode)
	   (setq inferior-lisp-program "sbcl")))

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
  (shell-command-to-string "git clone https://github.com/mhayashi1120/Emacs-gosh-mode && mv -f Emacs-gosh-mode ~/.emacs.d/")
  (setq scheme-program-name "gosh -i")

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
  )

;; UTF-8 に統一
(setq process-coding-system-alist
	  (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))

(leaf lsp-go
  :after go-mode lsp-mode
  :commands lsp lsp-go-enable
  :hook ((go-mode-hook . ensure))
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
			  (add-hook 'before-save-hook 'gofmt-before-save))))

(defun godoc-get-buffer (query)
  "Get an empty buffer for a godoc QUERY."
  (let* ((buffer-name "*godoc*")
         (buffer (get-buffer buffer-name)))
    ;; Kill the existing buffer if it already exists.
    (when buffer (kill-buffer buffer))
    (get-buffer-create buffer-name)))


;;; java settings
(setenv "JDK_HOME" "/usr/lib/jvm/java-14-openjdk/")
(setenv "JAVA_HOME" "/usr/lib/jvm/java-14-openjdk/")
;;; go settings
(setenv "GOPATH" "/Users/takedamasaru/develop/go")

(put 'upcase-region 'disabled nil)
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
  :ensure t
  :hook ((lsp-mode-hook . lsp-ui-mode))
  :require t
  :commands lsp-ui-mode)

(leaf company-lsp
  :ensure t
  :require t)

(leaf java-imports
  :ensure t
  :require t
  :setq ((java-imports-find-block-function quote java-imports-find-place-sorted-block)))

(leaf javadoc-lookup
  :ensure t
  :config (g-set-keys '(("C-c C-j" . javadoc-lookup)))
  :require t)

(leaf lsp-java
  :ensure t
  :require t)

(add-hook 'java-mode-hook #'lsp)

(add-hook 'shell-mode-hook
		  (lambda ()
            (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))

(leaf python
  :ensure t
  :require t
  :config
  (setq python-shell-interpreter "python")
  (setq auto-mode-alist (cons
						 '("\\.py$'" . python-mode)
						 auto-mode-alist)))

(defun insert-current-time()
  (interactive)
  (insert (format-time-string "%Y-%m-%d(%a) %H:%M:%S" (current-time))))

(define-key global-map (kbd "C-c d") `insert-current-time)

(leaf ac-php
  :ensure t
  :require t)

(leaf company-php
  :ensure t
  :require t)

(defun php-company-hook ()
  (company-mode t)
  (ac-php-core-eldoc-setup) ;; enable eldoc
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-ac-php-backend)
										; 定義にジャンプ
  (define-key php-mode-map  (kbd "M-.") 'ac-php-find-symbol-at-point)
										; ジャンプ先から戻る
  (define-key php-mode-map  (kbd "M-,") 'ac-php-location-stack-back))

(add-hook 'php-mode-hook 'php-company-hook)

(leaf php-mode
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
  :require t
  :config
  ;; koko
  (add-to-list 'eglot-server-programs
			   `(haskell-mode . ("/home/wasu/.local/bin/haskell-language-server-8.8.4" "--lsp")))
  )

(leaf company-ghci
  :ensure t
  :require t
  :config
  (push 'company-ghci company-backends)
  (add-hook 'haskell-mode-hook 'company-mode)
  (add-hook 'haskell-interactive-mode-hook 'company-mode))

(leaf lsp-haskell
  :ensure t
  :require t
  :config)

(leaf haskell-mode
  :ensure t
  :hook ((haskell-mode-hook . lsp)
		 (haskell-mode-hook . (lambda ()
								(flycheck-mode -1))))
  :config
  (setf haskell-mode-stylish-haskell-path
		"stylish-haskell")
  (setq haskell-process-type 'stack-ghci)
  (custom-set-variables '(haskell-stylish-on-save t))
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'linum-mode)
  (add-to-list 'exec-path "~/.local/bin")
  (setq haskell-hoogle-command "hoogle")
  (eval-after-load 'haskell-mode '(progn
									(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
									(define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
									(define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
									(define-key haskell-mode-map "\C-oh" 'haskell-hoogle)
									))
  (eval-after-load 'haskell-cabal '(progn
									 (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-ode-clear)
									 (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
									 (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))
  )

(load (expand-file-name "~/.emacs.d/myel/me.el"))

(load (expand-file-name "~/.roswell/helper.el"))

(define-key global-map (kbd "C-c C-e m") 'emmet-expand-line)
(define-key global-map (kbd "C-c m") 'emmet-expand-line)

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

;; (leaf lsp-scala
;;   :config
;;   (with-eval-after-load 'scala-mode
;; 	(setq lsp-scala-server-command "/usr/local/bin/metals-emacs")
;; 	(require 'lsp-scala nil nil)
;; 	(setq lsp-prefer-flymake nil)
;; 	(add-hook 'scala-mode-hook #'lsp))
;;   (add-hook 'scala-mode-hook
;; 			'(lambda nil
;; 			   (scala-bootstrap:with-metals-installed
;; 				(scala-bootstrap:with-bloop-server-started
;; 				 (lsp))))))

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

(leaf dumb-jump
  :ensure t
  :config
  (setq dumb-jump-mode t)
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-use-visible-window nil)
  (global-set-key (kbd "C-j") 'dumb-jump))

(leaf ace-jump-mode
  :ensure t
  :require t
  :setq ((ace-jump-word-mode-use-query-char))
  :config
  (g-set-keys '(("C-:" . ace-jump-char-mode)
				("C-c C-;" . ace-jump-word-mode)
				("C-M-;" . ace-jump-line-mode)))
  (setq ace-jump-mode-move-keys (append "asdfghjkl;:]qwertyuiop@zxcvbnm,." nil)))


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
  :after t
  :defvar fsharp-mode-map
  :ensure t
  :pre-setq ((inferior-fsharp-program . "/usr/bin/dotnet fsi"))
  :config
  (define-key fsharp-mode-map "C-c C-M-f" #'fsharp-fantomas-format-buffer)
  (require 'eglot-fsharp)
  (setq fsharp2-lsp-executable "/home/wasu/fsharp-language-server/src/FSharpLanguageServer/bin/Release/netcoreapp3.0/linux-x64/publish/FSharpLanguageServer.dll")
  (add-hook 'fsharp-mode-hook 'eglot-ensure)
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
	'(add-hook 'flycheck-mode-hook #'flycheck-elm-setup)))

(cond ((file-exists-p "~/.emacs.d/opam-user-setup.el")
	   (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")))

(leaf merlin
  :ensure t
  :require t
  :config
  (with-eval-after-load 'company
	(add-to-list 'company-backends 'merlin-company-backend)))

(leaf tuareg
  :ensure t
  :require t)

(leaf ocamlformat
  :ensure t
  :require t
  :config
  (add-hook 'before-save-hook #'ocamlformat-before-save)
  (define-key tuareg-mode-map (kbd "C-M-<tab>") #'ocamlformat))

(leaf caml
  :ensure t
  :require t
  :config
  (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
	  (autoload 'merlin-mode "merlin" nil t nil)
	  (add-hook 'tuareg-mode-hook 'merlin-mode t)
	  (add-hook 'caml-mode-hook 'merlin-mode t)
	  (setq merlin-command 'opam)
	  )))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
				   '("opam" "exec" "--" "ocamlmerlin-lsp"))
  :major-modes '(caml-mode tuareg-mode)
  :server-id 'ocamlmerlin-lsp))

(setq org-todo-keywords
	  '((sequence "TODO(t)" "SOMEDAY(s)" "WAITING(w)" "DOING(i)" "|" "CANCEL(c)" "FAILED(f)" "DONE(d)")))

(leaf yasnippet
  :ensure t
  :require t
  :config
  (g-set-keys '(("C-x y i" . yas-insert-snippet)
				("C-x y n" . yas-new-snippet)
				("C-x y v" . yas-visit-snippet-file)
				("C-x y l" . yas-describe-tables)
				("C-x y g" . yas-reload-all)))
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas-ido-prompt))
  )

(leaf migemo
  :ensure t
  :require t
  :config
  (when (and (executable-find "cmigemo")
			 (require 'migemo nil t))
	(setq migemo-command "cmigemo")
	(setq migemo-options '("-q" "--emacs"))

	(setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")

	(setq migemo-user-dictionary nil)
	(setq migemo-regex-dictionary nil)
	(setq migemo-coding-system 'utf-8-unix)
	(load-library "migemo")
	(migemo-init)))

(leaf prettier-js
  :ensure t
  :require t
  :config
  (setq prettier-js-args '("--trailing-comma" "all"
						   "--bracket-spacing" "false")))

(leaf svelte-mode
  :ensure t
  :require t
  :config
  (add-hook 'svelte-mode-hook 'prettier-js-mode))

(leaf web-mode
  :ensure t
  :require t)

(leaf dockerfile-mode
  :ensure t
  :require t)

(leaf emoji-cheat-sheet-plus
  :ensure t
  :require t
  :config
  (progn
    (add-hook 'org-mode-hook 'emoji-cheat-sheet-plus-display-mode)
    (add-hook 'markdown-mode-hook 'emoji-cheat-sheet-plus-display-mode)
    (add-hook 'magit-log-mode-hook 'emoji-cheat-sheet-plus-display-mode)
	(add-hook 'shell-mode-hook 'emoji-cheat-sheet-plus-display-mode)
    (global-set-key (kbd "C-c C-e") 'emoji-cheat-sheet-plus-insert)))
