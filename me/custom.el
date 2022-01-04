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

(defun insert-hugo-header ()
  (interactive)
  (goto-char 0)
  (let ((ts (format-time-string "%Y-%m-%dT%T"))
		(title (read-string "title: ")))
	(insert (format "---
title: \"%s\"
date: %s
draft: false
---
" title ts))
	)
  )

(global-set-key (kbd "C-c i h") 'insert-hugo-header)
