(setq use-file-dialog nil)
(setq inhibit-x-resources t)
(setq inhibit-startup-buffer-menu t)
(push '(fullscreen . maximized) default-frame-alist)
(tool-bar-mode -1)
(setq use-short-answers t)
(setq make-backup-files nil)
(setq backup-inhibited nil)
(setq create-lockfiles nil)
(setq ring-bell-function 'ignore)
(setq blink-matching-paren nil)
(setq vc-handled-backends '(Git))
(setq auto-mode-case-fold nil)
(setq-default bidi-display-reordering 'left-to-right)

(setq bidi-inhibit-bpa t)

(setq-default cursor-in-non-selected-windows nil)

(custom-set-faces
 '(line-number-current-line ((t (:inherit line-number)))))

;; (defun my-find-file-check-mode ()
;;   "Set major mode to fundamental-mode if an error occurs."
;;   (condition-case err
;;       (progn
;; 	(normal-mode)
;; 	(run-hooks 'find-file-hook))
;;     (error
;;      (message "Error setting major mode: %s" (error-message-string err))
;;      (fundamental-mode))))

(setq-default major-mode 'fundamental-mode)
;; (add-hook 'find-file-hook 'my-find-file-check-mode)

(add-hook 'sh-mode-hook 'sh-set-shell)

(global-display-line-numbers-mode t)
