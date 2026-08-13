;;; writing.el --- Writing and note-taking -*- lexical-binding: t; -*-

;;; Commentary:
;; Denote, Org, Markdown等のメモ・執筆関連設定

;;; Code:

;; Denote - メモ管理
(use-package denote
  :config
  (setq denote-directory (expand-file-name "~/denote/"))
  (setq denote-file-type 'markdown-yaml)
  (setq denote-known-keywords '("note" "intel" "log" "project" "idea" "tech"))
  (setq denote-date-format "%Y%m%dT%H%M%S")

  ;; Denoteディレクトリ全体をripgrepで検索
  (defun my/denote-grep (regexp)
    "Denoteディレクトリ全体をripgrepで検索."
    (interactive "sSearch denote notes: ")
    (consult-ripgrep denote-directory regexp))

  :bind
  (("C-c n n" . denote)
   ("C-c n f" . denote-open-or-create)
   ("C-c n i" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n g" . my/denote-grep)))

;; Denote拡張 - ライフサイクル管理(limit判定・期限一覧・空ファイルレビュー・振り分け)
(use-package denote-lifecycle
  :straight (:host github :repo "wasuken/denote-lifecycle")
  :after denote
  :commands (denote-lifecycle-list
             denote-lifecycle-review-empty
             denote-lifecycle-distribute)
  :custom
  (denote-lifecycle-default-limit 7)
  (denote-lifecycle-category-limits '(("idea" . 30) ("project" . 90) ("log" . 3)))
  (denote-lifecycle-distribution-rules
   '((denote-lifecycle-active-p . "active")
     (denote-lifecycle-old-p    . denote-fossil-intake)))
  :bind
  ("C-c n l" . denote-lifecycle-list)
  ("C-c n r" . denote-lifecycle-review-empty))

(use-package denote-fossil
  :straight (:host github :repo "wasuken/denote-fossil")
  :after denote
  :commands (denote-fossil-scan-stale
             denote-fossil-demote
             denote-fossil-demote-stale
             denote-fossil-search)
  :custom
  (denote-fossil-directory (expand-file-name "~/denote-fossil/"))
  (denote-fossil-threshold-days 30)
  :config
  (add-hook 'find-file-hook #'denote-fossil--log-access)
  :bind
  ("C-c n z" . denote-fossil-search))

;; Markdown
(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (setq markdown-command "cmark"))

;; md-mermaid - Markdown内Mermaidのインラインプレビュー
(use-package md-mermaid
  :straight (:host github :repo "ahmetus/md-mermaid")
  :after markdown-mode
  :config
  (setq md-mermaid-live--resolved-snippet
        (expand-file-name "straight/repos/md-mermaid/scripts/md_mermaid_snippet.py"
                          user-emacs-directory))
  (setenv "PATH" (concat (expand-file-name "~/.asdf/shims") ":" (getenv "PATH")))
  (setq exec-path (cons (expand-file-name "~/.asdf/shims") exec-path))
  (md-mermaid-keybindings-mode 1)
  )

;; Org
(use-package org
  :straight (:type built-in)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c C-t" . org-todo)
         ("C-c C-x C-a" . org-archive-subtree))
  :config
  (setq org-agenda-files '("~/org/work.org"
                           "~/org/hobby.org"
                           "~/org/inbox.org"))

  (setq org-todo-keywords
        '((sequence "TODO" "DOING" "|" "DONE" "CANCELED")))

  (setq org-capture-templates
        '(("w" "仕事タスク" entry (file+headline "~/org/work.org" "Inbox")
           "* TODO %?\nDEADLINE: %^t\n:PROPERTIES:\n:EFFORT: %^{見積|0:30|1:00|2:00}\n:END:\n")

          ("h" "趣味タスク" entry (file+headline "~/org/hobby.org" "Inbox")
           "* TODO %?\nSCHEDULED: %^t\n")

          ("i" "とりあえずメモ" entry (file "~/org/inbox.org")
           "* %?\n  %U\n")))

  (setq org-archive-location "~/org/archive/%s_archive::")

  ;; ファイル作成
  (defvar my/org-file-templates
    '(("~/org/work.org" . "#+TITLE: 仕事タスク\n#+STARTUP: overview\n\n* Inbox\n")
      ("~/org/hobby.org" . "#+TITLE: 趣味・プロジェクト\n#+STARTUP: overview\n\n* Inbox\n")
      ("~/org/inbox.org" . "#+TITLE: とりあえずメモ\n#+STARTUP: overview\n\n* Inbox\n")))

  (dolist (file org-agenda-files)
    (let* ((file-path (expand-file-name file))
           (dir (file-name-directory file-path)))
      (unless (file-exists-p dir)
        (make-directory dir t))
      (unless (file-exists-p file-path)
        (with-temp-buffer
          (insert (or (cdr (assoc file my/org-file-templates)) ""))
          (write-file file-path))
        (message "Created org file: %s" file-path)))))

(provide 'writing)
;;; writing.el ends here
