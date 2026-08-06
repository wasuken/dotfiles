;;; writing.el --- Writing and note-taking -*- lexical-binding: t; -*-

;;; Commentary:
;; Denote, Org, Markdown等のメモ・執筆関連設定

;;; Code:

;; Denote - メモ管理
(use-package denote
  :ensure t
  :config
  (setq denote-directory (expand-file-name "~/denote/"))
  (setq denote-file-type 'markdown-yaml)
  (setq denote-known-keywords '("note" "intel" "log" "project" "idea" "tech"))
  (setq denote-date-format "%Y%m%dT%H%M%S")

  ;; 期限設定
  (setq denote-expiration-days 7)
  (setq denote-expiration-overrides
        '(("idea" . 30)
          ("project" . 90)
          ("log" . 3)))

  ;; 期限切れ一覧・削除用のmajor-mode
  (define-derived-mode denote-expired-mode special-mode "Denote-Expired"
    "Major mode for the denote expired files list.")

  (define-key denote-expired-mode-map (kbd "d") #'denote-expired-delete-at-point)
  (define-key denote-expired-mode-map (kbd "o") #'denote-expired-open-at-point)
  (define-key denote-expired-mode-map (kbd "q") #'quit-window)

  (defun denote-expired-delete-at-point ()
    "カーソル行のファイルを削除."
    (interactive)
    (let ((file (get-text-property (line-beginning-position) 'denote-expired-file)))
      (if (not file)
          (message "この行にはファイルがありません")
        (when (yes-or-no-p (format "%s を削除しますか？ " (file-name-nondirectory file)))
          (delete-file file)
          (let ((inhibit-read-only t))
            (delete-region (line-beginning-position)
                           (min (point-max) (1+ (line-end-position)))))
          (message "削除しました: %s" (file-name-nondirectory file))))))

  (defun denote-expired-open-at-point ()
    "カーソル行のファイルを開く."
    (interactive)
    (let ((file (get-text-property (line-beginning-position) 'denote-expired-file)))
      (if file (find-file file) (message "この行にはファイルがありません"))))

  ;; 期限切れチェック
  (defun denote-check-expired ()
    "期限切れのメモを検出して警告を表示"
    (interactive)
    (let ((expired-files '())
          (current-time (current-time)))
      (dolist (file (directory-files denote-directory t "\\.md$"))
        (let* ((attrs (file-attributes file))
               (mtime (nth 5 attrs))
               (tags (denote-extract-keywords-from-path file))
               (expiry-days (or (cdr (assoc (car tags) denote-expiration-overrides))
                                denote-expiration-days))
               (expiry-time (time-add mtime (days-to-time expiry-days))))
          (when (time-less-p expiry-time current-time)
            (push (list file (time-subtract current-time expiry-time)) expired-files))))
      (if (not expired-files)
          (message "期限切れのメモはありません")
        (with-current-buffer (get-buffer-create "*Denote Expired*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "=== 期限切れメモ (%d件) ===\n\n" (length expired-files)))
            (dolist (item (sort expired-files
                                (lambda (a b) (time-less-p (cadr b) (cadr a)))))
              (let* ((file (car item))
                     (overdue (cadr item))
                     (days (/ (float-time overdue) 86400))
                     (start (point)))
                (insert (format "- %s (%.1f日超過)\n"
                                (file-name-nondirectory file)
                                days))
                (put-text-property start (point) 'denote-expired-file file)))
            (insert "\n[d] 削除 [o] 開く [q] 閉じる")
            (goto-char (point-min))
            (denote-expired-mode))
          (pop-to-buffer (current-buffer))))))

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
   ("C-c n e" . denote-check-expired)
   ("C-c n g" . my/denote-grep)))

;; Denoteテンプレート挿入
(defun my/denote-with-template (template-name)
  "Denoteで新規ファイル作成後、テンプレート挿入"
  (interactive
   (list (completing-read "Template: "
                          '("log" "think" "daily" "collect"))))
  (call-interactively #'denote)
  (tempel-insert (intern template-name)))

(global-set-key (kbd "C-c n t") #'my/denote-with-template)

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
