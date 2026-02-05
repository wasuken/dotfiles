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
      (when expired-files
        (with-current-buffer (get-buffer-create "*Denote Expired*")
          (erase-buffer)
          (insert (format "=== 期限切れメモ (%d件) ===\n\n" (length expired-files)))
          (dolist (item (sort expired-files
                              (lambda (a b) (time-less-p (cadr b) (cadr a)))))
            (let* ((file (car item))
                   (overdue (cadr item))
                   (days (/ (float-time overdue) 86400)))
              (insert (format "- %s (%.1f日超過)\n"
                              (file-name-nondirectory file)
                              days))))
          (insert "\n[d] 削除 [o] 開く [q] 閉じる")
          (goto-char (point-min))
          (view-mode)
          (display-buffer (current-buffer))))))

  ;; 起動時チェック
  (add-hook 'after-init-hook
            (lambda ()
              (run-with-idle-timer 3 nil #'denote-check-expired)))

  :bind
  (("C-c n n" . denote)
   ("C-c n f" . denote-open-or-create)
   ("C-c n i" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n e" . denote-check-expired)))

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
