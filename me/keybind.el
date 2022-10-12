(global-set-key (kbd "C-h") 'delete-backward-char)

(defun replace-sepa (left &optional right)
  (setq right (or right left))
  (let ((cur-strs  (buffer-substring (region-beginning) (region-end))))
	(delete-region (region-beginning) (region-end))
	(insert (concatenate 'string left
						 (replace-regexp-in-string ","
												   (concatenate 'string
																right
																","
																left)
												   cur-strs)
						 right))
	(print right)))
(defun replace-dc ()
  (interactive)
  (replace-sepa "\""))
(defun replace-sc ()
  (interactive)
  (replace-sepa "\'"))
(defun replace-paren ()
  (interactive)
  (replace-sepa "(" ")"))
(defun insert-> ()
  (interactive)
  (insert " -> "))
(defun insert<- ()
  (interactive)
  (insert " <- "))
(defun insert=> ()
  (interactive)
  (insert " => "))
(defun insert<= ()
  (interactive)
  (insert " <= "))

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



(global-set-key (kbd "C-c i h") 'insert-hugo-header)

(require 'request)

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

;; 縦サイズをcntで分割して、小さい方の割合と、残ったサイズを返却
(defun window-height-div (
						  cnt				;分割数
						  sm-ratio			;小さい方の割合
						  )
  (let* ((fh (frame-height))
		 (fh-one (/ fh cnt))
		 (small (* fh-one sm-ratio))
		 (big (* fh-one (- cnt sm-ratio))))
	`(,small ,big)
	)
  )

(defun window-adjust (f)
  (let* ((sm-big (window-height-div 3 1)))
	(funcall f sm-big)))

(defun adjust-height (hei)
  (- hei (window-height)))

(defun window-adjust-sm ()
  (interactive)
  (enlarge-window (window-adjust #'(lambda (x) (- (car x) (window-height))))))

(defun window-adjust-big ()
  (interactive)
  (enlarge-window (window-adjust #'(lambda (x) (- (cadr x) (window-height))))))

(global-set-key (kbd "C-c w s") #'window-adjust-sm)
(global-set-key (kbd "C-c w b") #'window-adjust-big)
