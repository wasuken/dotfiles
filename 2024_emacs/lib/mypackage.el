(require 'package)

(add-to-list 'package-archives '("gnu-elpa-devel" . "https://elpa.gnu.org/devel/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq user-config-file
      (expand-file-name "config.el" user-emacs-directory))

(defun user-load-config ()
  "Load user configuration from file."
  (when (file-exists-p user-config-file)
    (load user-config-file)))

(user-load-config)

;; インストールする優先順位を指定
(setq package-archive-priorities
      '(("gnu-elpa-devel" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

(setq package-install-upgrade-built-in t ; built-inパッケージも更新対象にする
      package-native-compile t           ; インストール時にnative compileする
      )

(setq package-user-dir "~/.emacs.d/elpa")
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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

(use-package use-package
  :config
  (setq use-package-always-ensure t))

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(use-package auto-package-update
  :config
  (setq auto-package-update-interval 7)
  (auto-package-update-maybe))

(use-package auto-compile
  :config
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode +1)
  (auto-compile-on-save-mode +1))

(use-package files
  :ensure nil
  :config
  (setq auto-save-visited-interval 30)
  (auto-save-visited-mode +1))

(use-package saveplace
  :config
  (save-place-mode +1))

(use-package recentf
  :config
  (setq recentf-max-saved-items 100)
  (recentf-mode +1))

(use-package savehist
  :config
  (savehist-mode +1))

(use-package paren
  :config
  (show-paren-mode +1))

(use-package elec-pair
  :config
  (electric-pair-mode +1))

(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package autorevert
  :config
  (global-auto-revert-mode +1))

(use-package subword
  :config
  (global-subword-mode +1))

;; (use-package simple
;;   :config
;;   (setq-default indent-tabs-mode nil))

(use-package so-long
  :config
  (global-so-long-mode +1))

(use-package flymake
  :hook ((prog-mode
          conf-mode) . flymake-mode)
  :config
  (setq-default flymake-no-changes-timeout 1.0))

(use-package flymake-collection
  :hook ((after-init . flymake-collection-hook-setup)
         ((tsx-ts-mode
           jsx-ts-mode
           jtsx-jsx-mode
           jtsx-tsx-mode
           jtsx-typescript-mode) . (lambda () (add-to-list 'flymake-diagnostic-functions #'flymake-collection-eslint)))
         (eglot-managed-mode . (lambda () (add-to-list 'flymake-diagnostic-functions #'eglot-flymake-backend)))))

(use-package delsel
  :config
  (delete-selection-mode +1))

(use-package tr-ime
  :if IS-WINDOWS
  :config
  (setq default-input-method "W32-IME")
  (tr-ime-standard-install)
  (w32-ime-initialize))



(use-package mozc
  :ensure t
  :config
  (setq default-input-method "japanese-mozc")
  (setq mozc-candidate-style 'overlay))

;; ignore
;; (use-package ddskk
;;   :config
;;   (setq default-input-method "japanese-skk")
;;   (setq skk-large-jisyo "~/.emacs.d/SKK-JISYO.L")
;;   (setq skk-jisyo-list '("~/.emacs.d/SKK-JISYO.L" "~/.emacs.d/SKK-JISYO.propernoun"))
;;   (require 'skk-study))

;; (use-package ddskk-posframe)

;; (use-package skk
;;   :config
;;   (global-set-key (kbd "<zenkaku-hankaku>") 'skk-mode)
;;   (global-set-key (kbd "C-<zenkaku-hankaku>") 'skk-katakana-region))


(use-package fontaine
  :config
  (cond (IS-LINUX
         (setq fontaine-presets
               '((regular
                  :default-family "PlemolJP HS"
                  :fixed-pitch-family "PlemolJP HS"
                  :variable-pitch-family "IBM Plex Sans JP"
                  :italic-family "PlemolJP HS")
                 (large
                  :default-family "PlemolJP HS"
                  :variable-pitch-family "IBM Plex Sans JP"))))

        (IS-WINDOWS
         (setq fontaine-presets
               '((regular
                  :default-family "PlemolJP Console NF"
                  :fixed-pitch-family "PlemolJP Console NF"
                  :variable-pitch-family "IBM Plex Sans JP"
                  :italic-family "PlemolJP Console NF")
                 (large
                  :default-family "PlemolJP Console NF"
                  :variable-pitch-family "IBM Plex Sans JP")))))

  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))

(use-package nerd-icons)
(use-package nerd-icons-completion
  :hook (after-init . nerd-icons-completion-mode))
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-corfu
  :vc ( :fetcher github :repo "LuigiPiucco/nerd-icons-corfu")
  :after corfu nerd-icons
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nyan-mode
  :config
  (setq nyan-bar-length 24)
  (nyan-mode +1))

(use-package minions
  :config
  (minions-mode +1))

(use-package corfu
  :demand t
  :bind ( :map corfu-map
          ("TAB" . corfu-insert)
          ([tab] . corfu-insert)
          ("RET" . nil)
          ([return] . nil))
  :hook (prog-mode . (lambda ()
                       (global-set-key [remap c-indent-line-or-region] #'indent-for-tab-command)))
  :config
  (setq corfu-cycle t
        corfu-count 16
        corfu-auto t
        corfu-auto-prefix 1
        corfu-auto-delay 0
        corfu-on-exact-match nil)

  (global-corfu-mode +1)

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  )

(use-package corfu-popupinfo
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode))

(with-eval-after-load 'corfu
  (setq corfu-preselect 'prompt)

  (define-key corfu-map (kbd "TAB") 'corfu-next)
  (define-key corfu-map (kbd "<tab>") 'corfu-next)
  (define-key corfu-map (kbd "S-TAB") 'corfu-previous)
  (define-key corfu-map (kbd "<backtab>") 'corfu-previous)

  (defvar corfu--index)
  (defvar corfu-magic-insert-or-next-line
    `(menu-item "" nil :filter ,(lambda (&optional _)
                                  (when (>= corfu--index 0)
                                    'corfu-insert)))
    "If we made a selection during `corfu' completion, select it.")
  (define-key corfu-map (kbd "RET") corfu-magic-insert-or-next-line)

  (defvar corfu-magic-cancel-or-backspace
    `(menu-item "" nil :filter ,(lambda (&optional _)
                                  (when (>= corfu--index 0)
                                    'corfu-reset)))
    "If we made a selection during `corfu' completion, cancel it.")
  (define-key corfu-map (kbd "DEL") corfu-magic-cancel-or-backspace)
  (define-key corfu-map (kbd "<backspace") corfu-magic-cancel-or-backspace)
  )

;; (use-package tabnine
;;   :demand t
;;   :hook (kill-emacs . tabnine-kill-process)
;;   :bind ( :map tabnine-completion-map
;;           ("TAB" . nil)
;;           ("<tab>" . nil))
;;   :config
;;   (tabnine-start-process)
;;   (global-tabnine-mode +1))

(use-package cape
  :hook (((prog-mode
           text-mode
           conf-mode
           eglot-managed-mode) . my/set-super-capf))
  :config
  (defun my/set-super-capf (&optional arg)
    (setq-local completion-at-point-functions
                (list (cape-capf-properties
                       (cape-capf-super
                        (cape-capf-noninterruptible
                         (cape-capf-buster
                          (if arg
                              arg
                            (car completion-at-point-functions))))
                        #'tempel-complete
                        ;; #'tabnine-completion-at-point
                        #'cape-dabbrev
                        #'cape-file)
                       :sort t)))))


(use-package corfu-terminal
  :unless (display-graphic-p)
  :config
  (corfu-terminal-mode +1))

(setq tab-always-indent 'complete)

(use-package vertico
  :config
  (setq vertico-cycle t)
  (vertico-mode +1))

(use-package vertico-repeat
  :ensure nil
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind ( :map vertico-map
          ("<backspace>" . vertico-directory-delete-char)))

(use-package vertico-buffer
  :ensure nil
  :config
  (setq vertico-buffer-display-action '(display-buffer-at-bottom))
  (vertico-buffer-mode +1))

(defvar +vertico-current-arrow t)

(cl-defmethod vertico--format-candidate :around
  (cand prefix suffix index start &context ((and +vertico-current-arrow
                                                 (not (bound-and-true-p vertico-flat-mode)))
                                            (eql t)))
  (setq cand (cl-call-next-method cand prefix suffix index start))
  (let ((arrow (nerd-icons-faicon "nf-fa-hand_o_right")))
    (if (bound-and-true-p vertico-grid-mode)
        (if (= vertico--index index)
            (concat arrow " " cand)
          (concat #("_" 0 1 (display " ")) cand))
      (if (= vertico--index index)
          (concat " " arrow " " cand)
        (concat "    " cand)))))

(use-package vertico-truncate
  :vc ( :fetcher github :repo "jdtsmith/vertico-truncate")
  :config
  (vertico-truncate-mode +1))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides nil)

  (with-eval-after-load 'migemo
    ;; orderlessをmigemo対応
    (defun orderless-migemo (component)
      (let ((pattern (downcase (migemo-get-pattern component))))
        (condition-case nil
            (progn (string-match-p pattern "") pattern)
          (invalid-regexp nil))))
    (add-to-list 'orderless-matching-styles 'orderless-migemo))

  (with-eval-after-load 'corfu
    (defun orderless-fast-dispatch (word index total)
      (and (= index 0) (= total 1) (length< word 4)
           'orderless-literal-prefix))

    (orderless-define-completion-style orderless-fast
      (orderless-style-dispatchers '(orderless-fast-dispatch))
      (orderless-matching-styles '(orderless-flex)))

    (defun my/setup-corfu-for-orderless ()
      (setq-local corfu-auto-delay 0
                  corfu-auto-prefix 1
                  completion-styles '(orderless-fast)))

    (add-hook 'corfu-mode-hook #'my/setup-corfu-for-orderless)))

(use-package prescient
  :config
  (setq prescient-aggressive-file-save t)
  (prescient-persist-mode +1))

(use-package vertico-prescient
  :config
  (setq vertico-prescient-enable-filtering nil)
  (vertico-prescient-mode +1))

(use-package corfu-prescient
  :config
  (setq corfu-prescient-enable-filtering nil)
  (corfu-prescient-mode +1))

(use-package flx
  :config
  (with-eval-after-load 'prescient
    ;; 入力文字を抽出
    (defvar-local my/input-query nil)
    (defun my/store-input-query (string &rest _args)
      "store the current completion query in `my/input-query'."
      (setq my/input-query (replace-regexp-in-string " " "" string)))
    (advice-add 'completion-all-completions :before #'my/store-input-query)

    ;; ローカル変数を使用できるように再定義
    (defvar vertico--total nil)
    (defvar corfu--total nil)

    (defun my/flx-tiebreaker (c1 c2)
      (if (and (and (< vertico--total 3000)
                    (< corfu--total 3000))
               (> (length my/input-query) 0)
               (< (length c1) 100)
               (< (length c2) 100))
          (let ((query my/input-query))
            (let ((score1 (car (flx-score c1 query flx-file-cache)))
                  (score2 (car (flx-score c2 query flx-file-cache))))
              (if (and (integerp score1) (integerp score2))
                  (cond ((> score1 score2) -1)
                        ((< score1 score2) 1)
                        (t (- (length c1) (length c2))))
                0)))
        (- (length c1) (length c2))))

    (setq prescient-tiebreaker #'my/flx-tiebreaker)))


;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ([remap switch-to-buffer] . consult-buffer)                ;; orig. switch-to-buffer
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ([remap bookmark-jump] . consult-bookmark)            ;; orig. bookmark-jump
         ([remap project-switch-to-buffer] . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         :map goto-map
         ("e" . consult-compile-error)
         ("f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("g" . consult-goto-line)             ;; orig. goto-line
         ("M-g" . consult-goto-line)           ;; orig. goto-line
         ("o" . consult-outline)               ;; Alternative: consult-org-heading
         ("m" . consult-mark)
         ("k" . consult-global-mark)
         ("i" . consult-imenu)
         ("I" . consult-imenu-multi)
         :map search-map
         ("d" . consult-fd)
         ("D" . consult-locate)
         ("g" . consult-grep)
         ("G" . consult-git-grep)
         ("r" . consult-ripgrep)
         ("l" . consult-line)
         ("L" . consult-line-multi)
         ("m" . consult-multi-occsur)
         ("k" . consult-keep-lines)
         ("u" . consult-focus-lines)
         ("e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-hisstory)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :config configuration is always executed (Not lazy)
  :config

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  ;; (setq xref-show-xrefs-function #'consult-xref
  ;;       xref-show-definitions-function #'consult-xref)

  (with-eval-after-load 'meow
    (meow-leader-define-key
     '("b" . consult-buffer)))

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 1.0 any)
   consult-ripgrep consult-git-grep consult-grep
   ;; consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
    ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
    ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
    ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
    ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )


(defun consult-ripgrep-current-directory ()
  (interactive)
  (consult-ripgrep default-directory))

(define-key search-map (kbd "R") #'consult-ripgrep-current-directory)

(use-package marginalia
  :config
  (marginalia-mode +1))

(use-package embark
  :bind (("C-c e ." . embark-act)         ;; pick some comfortable binding
         ("C-c e ;" . embark-dwim)        ;; good alternative: M-.
         ("C-c e b" . embark-bindings)) ;; alternative for `describe-bindings'
  :config
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package tempel
  :demand t
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)))

(use-package tempel-collection :after tempel)

(use-package magit
  :config
  (when IS-WINDOWS
    (setq magit-refresh-status-buffer nil)
    (setq auto-revert-buffer-list-filter
          'magit-auto-revert-repository-buffer-p)
    (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
    (remove-hook 'server-switch-hook 'magit-commit-diff)
    (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)
    (define-key global-map (kbd "C-x g") 'magit)
    ))

(use-package diff-hl
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         ;; (dired-mode . diff-hl-dired-mode)
         )
  :config
  (global-diff-hl-mode +1)
  ;; (global-diff-hl-show-hunk-mouse-mode +1)
  )

(use-package difftastic
  :demand t
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))

(use-package which-key
  :config
  (which-key-mode +1))

(use-package undo-fu
  :config
  (with-eval-after-load 'evil
    (setq evil-undo-system 'undo-fu)))

(use-package undo-fu-session
  :config
  (undo-fu-session-global-mode +1))

(use-package vundo)

(use-package golden-ratio
  :config
  (golden-ratio-mode))

(use-package rg
  :defer t)

;; (use-package apheleia
;;   :config
;;   (when IS-WINDOWS
;;     (add-to-list 'apheleia-formatters
;;                  '(prettier-css
;;                    . (npx "prettier" "--stdin-filepath" filepath "--parser=css"
;;                           (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))))
;;     (add-to-list 'apheleia-formatters
;;                  '(prettier-html
;;                    . (npx "prettier" "--stdin-filepath" filepath "--parser=html"
;;                           (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))))
;;     (add-to-list 'apheleia-formatters
;;                  '(prettier-json
;;                    . (npx "prettier" "--stdin-filepath" filepath "--parser=json"
;;                           (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))))
;;     (add-to-list 'apheleia-formatters
;;                  '(prettier-typescript
;;                    . (npx "prettier" "--stdin-filepath" filepath "--parser=typescript"
;;                           (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))))
;;     )

;;   (apheleia-global-mode +1))

(use-package ef-themes
  :config
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  ;; (load-theme 'ef-melissa-light t)
  ;;   (load-theme 'ef-tritanopia-dark t)
  (load-theme 'ef-night t)
  )

;; (use-package modus-themes
;;   :config
;;   ;; (setq modus-themes-italic-constructs t
;;   ;;       modus-themes-bold-constructs nil
;;   ;;       modus-themes-mixed-fonts t
;;   ;;       modus-themes-variable-pitch-ui t
;;   ;;       modus-themes-disable-other-themes t)

;;   ;; (setq modus-themes-completions
;;   ;;       '((t . (underline))))

;;   ;; (setq modus-themes-common-palette-overrides
;;   ;;       '((fg-completion-match-0 blue)
;;   ;;         (fg-completion-match-1 magenta-warmer)
;;   ;;         (fg-completion-match-2 cyan)
;;   ;;         (fg-completion-match-3 red)
;;   ;;         (bg-completion-match-0 bg-blue-nuanced)
;;   ;;         (bg-completion-match-1 bg-magenta-nuanced)
;;   ;;         (bg-completion-match-2 bg-cyan-nuanced)
;;   ;;         (bg-completion-match-3 bg-red-nuanced)))

;;   ;; (load-theme 'modus-operandi-tinted t)
;;   (load-theme 'modus-vivendi-tinted t)
;;   )

(use-package restart-emacs
  :ensure nil
  :bind ( :map my-quit-map
          ("r" . restart-emacs)))

(use-package expreg)

(use-package puni
  :config
  (puni-global-mode +1))

(setq treesit-language-source-alist
      '((json "https://github.com/tree-sitter/tree-sitter-json")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        ))

(use-package treesit
  :ensure nil
  :config
  (setq treesit-font-lock-level 4)
  ;; xmp
  (setq load-path (cons "~/.emacs.d/site-lisp/" load-path))
  ;; (define-key ruby-mode (kbd "C-c C-d") 'xmp)
  ;; (define-key ruby-ts-mode-map (kbd "C-c C-d") 'xmp)
  (setq go-ts-mode-indent-offset 4)
  (require 'rcodetools)
  )

(use-package treesit-auto
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)

  (setq treesit-auto-install t)
  (global-treesit-auto-mode +1))


(use-package string-inflection
  :bind ( :map my-string-inflection-map
          ("a" . string-inflection-all-cycle)
          ("_" . string-inflection-underscore)
          ("p" . string-inflection-pascal-case)
          ("c" . string-inflection-camelcase)
          ("u" . string-inflection-upcase)
          ("k" . string-inflection-kebab-case)
          ("C" . string-inflection-capital-underscore))
  :config
  (defvar my-string-inflection-map (make-keymap))
  )

(use-package go-translate
  :bind ("C-c C-g" . gt-do-translate)
  :config
  (setq gt-langs '(en ja))
  (setq gt-taker-text 'word)
  (setq gt-taker-pick 'paragraph)
  (setq gt-taker-prompt 'nil)
  ;; (setq gt-default-translator (gt-translator :engines (gt-google-engine)))
  (setq gt-default-translator (gt-translator :engines (list
						       (gt-deepl-engine :key deepl-api-key)
						       )))
  )

(use-package avy
  )

(use-package ace-window
  :config
  (custom-set-faces
   '(aw-leading-char-face ((t (:foreground "red" :height 4.0))))))

(use-package ace-jump-mode
  :config
  (define-key global-map (kbd "C-'") #'ace-jump-char-mode)
  (define-key global-map (kbd "C-M-'") #'ace-jump-word-mode)
  (define-key global-map (kbd "M-'") #'ace-jump-line-mode)
  )

(use-package migemo
  :config
  ;; cmigemo(default)
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))

  ;; Set your installed path
  (when IS-WINDOWS
    (setq migemo-dictionary
          "C:/Users/u396/scoop/apps/cmigemo/current/cmigemo-mingw64/share/migemo/utf-8/migemo-dict"))

  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)

  :config
  (migemo-init))

(use-package lin
  :config
  (setq lin-face 'lin-red)
  (lin-global-mode +1))

(use-package pulsar
  :config
  (pulsar-global-mode +1))

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(use-package spacious-padding
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible and provides several examples.
  (setq spacious-padding-subtle-mode-line
        `( :mode-line-active 'default
           :mode-line-inactive vertical-border))

  (spacious-padding-mode +1))

(require 'xref)
(use-package beframe
  :config
  (defvar consult-buffer-sources)
  (declare-function consult--buffer-state "consult")

  (with-eval-after-load 'consult
    (defface beframe-buffer
      '((t :inherit font-lock-string-face))
      "Face for `consult' framed buffers.")

    (defvar beframe-consult-source
      `( :name     "Frame-specific buffers (current frame)"
         :narrow   ?F
         :category buffer
         :face     beframe-buffer
         :history  beframe-history
         :items    ,#'beframe-buffer-names
         :action   ,#'switch-to-buffer
         :state    ,#'consult--buffer-state))

    (add-to-list 'consult-buffer-sources 'beframe-consult-source))

  (beframe-mode +1))

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package perfect-margin
  :config
  (setq perfect-margin-ignore-filters nil)
  (perfect-margin-mode -1)
  (define-key global-map (kbd "C-c p m") 'perfect-margin-mode)
  )

(use-package dashboard
  :config
  (setq dashboard-center-content t)
  (dashboard-setup-startup-hook))

(use-package page-break-lines
  :config
  (page-break-lines-mode +1))

(use-package breadcrumb
  :vc ( :fetcher github :repo "joaotavora/breadcrumb")
  :config
  (breadcrumb-mode +1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package imenu-list
  :bind ( :map my-toggle-map
          ("i" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-position 'left))

(use-package eglot
  :bind ( :map eglot-mode-map
          ("C-c r" . eglot-rename)
          ("C-c o" . eglot-code-action-organize-imports)
          ("C-c a" . eglot-code-actions)
          ("C-c h" . eldoc)
          ("<f6>" . xref-find-definitions))
  :config
  (setq eglot-events-buffer-config '(:size 0  :format short)
        eglot-ignored-server-capabilities '(:documentHighlightProvider)
        eglot-stay-out-of '(flymake)
        eglot-send-changes-idle-time 1.0)

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

(use-package eglot-tempel
  :after (eglot tempel)
  :hook (eglot-managed-mode . eglot-tempel-mode))

(use-package consult-eglot
  :after eglot
  :bind ( :map eglot-mode-map
          ("C-c s" . consult-eglot-symbols)))

(use-package jsonrpc
  :defer t
  :config
  (setq jsonrpc-default-request-timeout 3000)
  (fset #'jsonrpc--log-event #'ignore))

(use-package eglot-booster
  :after eglot
  :vc ( :fetcher github :repo "jdtsmith/eglot-booster")
  :config
  (eglot-booster-mode +1))

(use-package eglot-x
  :vc ( :fetcher github :repo "nemethf/eglot-x")
  :after eglot
  :config
  (eglot-x-setup))

(use-package eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-mode))

(use-package eglot-signature-eldoc-talkative
  :after eldoc-box
  :config
  (advice-add #'eglot-signature-eldoc-function
              :override #'eglot-signature-eldoc-talkative))

(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

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
  ;; Optional customizations
  ;; (js-indent-level 2)
  ;; (typescript-ts-mode-indent-offset 2)
  ;; (jtsx-switch-indent-offset 0)
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

(use-package rust-mode
  :hook (rust-mode . (lambda ()
                       (setq-local tab-width 4))))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package pyvenv
  :defer t)

;; (use-package slime
;;   :defer t
;;   :config
;;   (setq inferior-lisp-program "sbcl"))

(use-package slime
  :if (file-exists-p "~/.roswell/helper.el")
  :ensure slime-company
  :init (load "~/.roswell/helper.el")
  :custom (inferior-lisp-program "ros -Q run")
  :config (slime-setup '(slime-fancy slime-company)))

(use-package slime-company
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space))

(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

(use-package sqlformat
  :config
  (setq sqlformat-command "sqlfluff"))

(use-package docker-compose-mode
  :mode (("docker-compose\\.yml\\'" . docker-compose-mode)
         ("compose\\.yml\\'" . docker-compose-mode)
         ("\\(?:docker\\|compose\\).+\\.yaml\\'" . docker-compose-mode)))

(use-package neotree
  :bind ("C-c t" . neotree))

(use-package mermaid-mode)

(use-package elfeed
  :config
  (global-set-key (kbd "C-x w") 'elfeed)
  (setq elfeed-feeds
	'("http://nullprogram.com/feed/"
          "https://planet.emacslife.com/atom.xml"
	  "https://zenn.dev/topics/react/feed"
	  "https://zenn.dev/topics/llm/feed"
	  "https://zenn.dev/topics/mcp/feed"
	  )))

(use-package mastodon
  ;; デフォルトでは GitHub を参照するため
  ;; 明示的に Codeberg からインストール
  :straight (mastodon :type git
                      :host codeberg
                      :repo "martianh/mastodon.el")
  :custom
  ;; 使用している Mastodon サーバ
  (mastodon-instance-url "https://mstdn.jp/")
  ;; ユーザ名
  (mastodon-active-user "wasulisp")
  ;; タイムラインでアバター画像を表示する
  (mastodon-tl--show-avatars t))
