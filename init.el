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

(defconst my:d:tmp
  (expand-file-name ".cache/emacs/" (getenv "HOME")))
(unless (file-directory-p my:d:tmp)
  (make-directory my:d:tmp))

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
   '(slime skk-hint recentf-ext embark-consult ace-jump-buffer all-the-icons smartparens ace-jump docker-compose-mode mwim dockerfile docker-compose nwim side-hustle orderless vertico smartparen doom-themes zop-to-char zenburn-theme yaml-mode which-key web-mode w3m vue-mode use-package undo-tree typescript-mode twittering-mode twig-mode tuareg svelte-mode super-save sayid rustic robe rjsx-mode rainbow-mode rainbow-delimiters racer quickrun python-pytest pug-mode pt protobuf-mode php-mode pdf-tools parsec ocamlformat nvm neotree multi-term mozc move-text merlin magit-popup magit lsp-ui lsp-scala lsp-ruby lsp-java lsp-haskell leaf-convert julia-mode javadoc-lookup java-imports iter2 intero imenu-anywhere howm helm-fish-completion google-translate golden-ratio go-mode fsharp-mode flycheck-pos-tip flycheck-clojure fish-mode expand-region exec-path-from-shell ess-R-data-view ensime emmet-mode elscreen elm-mode elisp-slime-nav elfeed edn easy-kill dotnet dockerfile-mode dired-subtree diff-hl csharp-mode crux counsel company-lsp clj-refactor cask-mode cargo anzu ammonite-term-repl ag ace-jump-mode)))
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
  (push 'command-history savehist-ignored-variables)
  :custom
  `((savehist-file
     . ,(expand-file-name "history" my:d:tmp)))
  :hook
  ((after-init-hook . savehist-mode)))

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

;; (leaf company-lsp
;;   :ensure t
;;   :require t
;;   :commands company-lsp)

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

(defun tab-move-right ()
    (interactive)
    (let* ((ix (tab-bar--current-tab-index))
           (n-tabs (length (funcall tab-bar-tabs-function)))
           (next-ix (mod (+ ix 1) n-tabs)))
        ;; use 1-based index
        (tab-bar-move-tab-to (+ 1 next-ix))))

(defun tab-move-left ()
    (interactive)
    (let* ((ix (tab-bar--current-tab-index))
           (n-tabs (length (funcall tab-bar-tabs-function)))
           (next-ix (mod (+ ix n-tabs -1) n-tabs)))
        ;; use 1-based index
        (tab-bar-move-tab-to (+ 1 next-ix))))

(leaf tab-bar
  :ensure t
  :require t
  :config
  (tab-bar-mode 1)
  (global-set-key (kbd "C-M->") 'tab-move-right)
  (global-set-key (kbd "C-M-<") 'tab-move-light)
  (global-set-key (kbd "C-c n") 'tab-bar-new-tab)
  )

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

(leaf projectile
  :ensure t
  :require t
  :config
  (projectile-mode +1)
  ;; Recommended keymap prefix on macOS
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  ;; Recommended keymap prefix on Windows/Linux
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Example configuration for Consult
(leaf consult
  :require t
  :ensure t
  ;; :map isearch-mode-map
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  (setq consult-preview-key 'any)
  (setq consult-preview-key (kbd "M-."))
  (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;; 2. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  ;; 3. vc.el (vc-root-dir)
  (setq consult-project-root-function #'vc-root-dir)
  ;; 4. locate-dominating-file
  (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )

;; Enable richer annotations using the Marginalia package
(leaf marginalia
  :require t
  :ensure t
  ;; :map minibuffer-local-map
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(leaf embark
  :ensure t
  :require t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-c ," . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(leaf embark-consult
  :require t
  :ensure t
  :after (embark consult)
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(leaf recentf
  :ensure t
  :require t
  :config
  (setq recentf-save-file "~/.emacs.d/.recentf")
  (setq recentf-max-saved-items 1000)            ;; recentf に保存するファイルの数
  (setq recentf-exclude '(".recentf"))           ;; .recentf自体は含まない
  ;;(setq recentf-auto-cleanup 'never)             ;; 保存する内容を整理
  (run-with-idle-timer 30 t '(lambda ()          ;; 30秒ごとに .recentf を保存
							   (with-suppressed-message (recentf-save-list))))
  )

(leaf recentf-ext
  :ensure t
  :require t
  )

(leaf autorevert
  :custom
  ((auto-revert-interval . 0.1))
  :hook
  (emacs-startup-hook . global-auto-revert-mode)
  )

(leaf migemo
  :if (executable-find "cmigemo")
  :ensure t
  ;; :require t
  :custom
  '((migemo-user-dictionary  . nil)
    (migemo-regex-dictionary . nil)
    (migemo-options          . '("-q" "--emacs"))
    (migemo-command          . "cmigemo")
    (migemo-coding-system    . 'utf-8-unix))
  :init
  (cond
   ((and (eq system-type 'darwin)
         (file-directory-p "/usr/local/share/migemo/utf-8/"))
    (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"))
   (t
    (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")))
  :hook
  (after-init-hook . migemo-init)
  )

(setq default-input-method "japanese-skk")
(global-set-key (kbd "C-c C-j") 'skk-mode)
(global-set-key (kbd "C-x j") 'skk-auto-fill-mode)
(setq skk-large-jisyo "/usr/share/skk/SKK-JISYO.L")

(leaf slime
  :ensure t
  :require t)
