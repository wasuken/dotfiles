;;; git.el --- Git integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Magit, diff-hl, difftastic等のGit関連パッケージ設定

;;; Code:

(use-package magit
  :bind ("C-x g" . magit)
  :config
  (when IS-WINDOWS
    (setq magit-refresh-status-buffer nil)
    (setq auto-revert-buffer-list-filter
          'magit-auto-revert-repository-buffer-p)
    (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
    (remove-hook 'server-switch-hook 'magit-commit-diff)
    (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)))

(use-package diff-hl
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode +1))

;; (use-package difftastic
;;   :demand t
;;   :bind (:map magit-blame-read-only-mode-map
;;               ("D" . difftastic-magit-show)
;;               ("S" . difftastic-magit-show))
;;   :config
;;   (eval-after-load 'magit-diff
;;     '(transient-append-suffix 'magit-diff '(-1 -1)
;;        [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
;;         ("S" "Difftastic show" difftastic-magit-show)])))

(provide 'git)
;;; git.el ends here
