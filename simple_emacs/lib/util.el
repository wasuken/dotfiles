(defun insert-current-time()
  (interactive)
  (insert (format-time-string "%Y-%m-%d(%a) %H:%M:%S" (current-time))))

(global-set-key (kbd "C->") 'other-window)
(global-set-key (kbd "C-<") (lambda () (interactive) (other-window -1)))

(defun format-yaml-lines (items indent)
  (string-join (mapcar #'(lambda (x) (format "%s- \"%s\"" indent x)) items) "
"))

(defun insert-hugo-header ()
  (interactive)
  (goto-char 0)
  (let* ((ts (format-time-string "%Y-%m-%d"))
		 (title (read-string "title: "))
		 (categories-str (read-string "categories(a,b): "))
		 (tags-str (read-string "tags(a,b): "))
		 (categories (split-string categories-str ","))
		 (tags (mapcar #'string-trim (split-string tags-str ",")))
		 )
	(insert
	 (format "---
title: \"%s\"
description:
date: %s
draft: false
categories:
%s
tags:
%s
---
" title ts (format-yaml-lines categories "  ") (format-yaml-lines tags "  ")))
	)
  )



(global-set-key (kbd "C-c b i") 'insert-hugo-header)

(defvar *create-md-link-url* "")

(defun create-md-link ()
  (interactive)
  (setq *create-md-link-url* (read-string "url: "))
  (request
	*create-md-link-url*
	:parser 'buffer-string
	:error (cl-function (lambda (&key error-thrown &allow-other-keys&rest _)
						  (message "Got error: %S" error-thrown)))
	:success (cl-function
			  (lambda (&key data &allow-other-keys)
				(string-match "<title>\\(.*?\\)</title>" data)
				(insert (format "[%s](%s)" (match-string 1 data) *create-md-link-url*))))))

(global-set-key (kbd "C-c l") #'create-md-link)
