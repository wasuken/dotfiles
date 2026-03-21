;;; completion.el --- Completion framework -*- lexical-binding: t; -*-

;;; Commentary:
;; Vertico, Corfu, Cape等の補完システム

;;; Code:

;; Corfu - インライン補完
(use-package corfu
  :demand t
  :bind ( :map corfu-map
          ;; ("TAB" . corfu-insert)
          ;; ([tab] . corfu-insert)
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
      (setq-local corfu-auto-delay 0
                  corfu-auto-prefix 1
                  completion-styles '(orderless-fast))
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

(use-package corfu-popupinfo
  :straight (:type built-in)
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
  (define-key corfu-map (kbd "<backspace>") corfu-magic-cancel-or-backspace))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :config
  (corfu-terminal-mode +1))

(use-package cape
  ;; :hook (((prog-mode
  ;;          text-mode
  ;;          conf-mode
  ;;          eglot-managed-mode) . my/set-super-capf))
  :config
  (defun my/set-super-capf-eglot ()
    (setq-local completion-at-point-functions
		(list (cape-capf-super
                       (car completion-at-point-functions)  ; eglotのcapfを先頭に
                       #'cape-dabbrev
                       #'cape-file))))

  (add-hook 'eglot-managed-mode-hook #'my/set-super-capf-eglot)
  )

(setq tab-always-indent 'complete)

;; Vertico
(use-package vertico
  :config
  (setq vertico-cycle t)
  (vertico-mode +1))

(use-package vertico-repeat
  :straight (:type built-in)
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save))

(use-package vertico-directory
  :straight (:type built-in)
  :after vertico
  :bind ( :map vertico-map
          ("<backspace>" . vertico-directory-delete-char)))

(use-package vertico-buffer
  :straight (:type built-in)
  :config
  (setq vertico-buffer-display-action '(display-buffer-at-bottom))
  ;; (vertico-buffer-mode +1)
  )

;; NOTE: nerd-iconsの矢印表示はui.elで設定 (nerd-icons読み込み後)

;; Orderless
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides nil)

  (with-eval-after-load 'migemo
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

;; Prescient
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

;; Flx
(use-package flx
  :config
  (with-eval-after-load 'prescient
    (defvar-local my/input-query nil)
    (defun my/store-input-query (string &rest _args)
      "Store the current completion query in `my/input-query'."
      (setq my/input-query (replace-regexp-in-string " " "" string)))
    (advice-add 'completion-all-completions :before #'my/store-input-query)

    (defvar vertico--total nil)
    (defvar corfu--total nil)

    (defun my/flx-tiebreaker (c1 c2)
      (if (and (and (< vertico--total 200)
                    (< corfu--total 200))
               (> (length my/input-query) 1)
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

(provide 'completion)
;;; completion.el ends here
