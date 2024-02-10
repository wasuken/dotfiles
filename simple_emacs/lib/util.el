;; tab-bar

(defun insert-current-time()
  (interactive)
  (insert (format-time-string "%Y-%m-%d(%a) %H:%M:%S" (current-time))))

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


(require 'seq)

(defun get-latest-files-recursively (directory n)
  "Return the N latest files in DIRECTORY and its subdirectories, excluding directories."
  (let ((files-with-times '()))
    ;; Collect all files and their creation times
    (dolist (file (directory-files-recursively directory ".*"))
      (unless (file-directory-p file)
	(let ((file-time (nth 5 (file-attributes file))))
	  (push (cons file file-time) files-with-times))))
    ;; Sort by creation time and take the N latest files
    (mapcar 'car
	    (seq-take (sort files-with-times (lambda (a b)
					       (time-less-p (cdr b) (cdr a))))
		      n))))

(defun open-files-vertically (file-paths)
  "Open each file in FILE-PATHS in a separate vertical buffer."
  (when file-paths
    (find-file (car file-paths))
    (dolist (file (cdr file-paths))
      (split-window-right)
      (other-window 1)
      (find-file file))))

(defun new-file-in-current-dir ()
  "カレントディレクトリから再帰的に検索をおこなって、最新のファイルN個をファイルバッファとして開く"
  (interactive)
  (setq n (string-to-number (read-string "How many files would you like to open?: ")))
  (setq dirpath
	default-directory
	;; (cond ((eq major-mode 'dired-mode)
	;;        default-directory)
	;;       (t (file-name-directory buffer-file-name)))
	)
  (open-files-vertically (get-latest-files-recursively dirpath n))
  )

(defun fortune-telling ()
  "A simple fortune telling function in Emacs Lisp."
  (interactive)
  (let* ((fortune-list '("大吉" "中吉" "小吉" "凶" "大凶"))
	 (fortune (nth (random (length fortune-list)) fortune-list)))
    (message "あなたの運勢は %s です" fortune)))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))
    (princ "count-windows is not 2")))

(defun generate-hugo-diary-header-text ()
  (format "---
title: \"日記\"
description:
date: %s
draft: false
categories:
  - \"diary\"
tags:
  - \"life\"
---
" (format-time-string "%Y-%m-%d")))

(defun insert-hugo-diary-header ()
  (interactive)
  (goto-char 0)
  (insert (generate-hugo-diary-header-text))
  )

(setf *diary-directory-path* "/home/wasu/memo/diary/")

(defun generate-today-diary-file ()
  (interactive)
  ;; カレントディレクトリ
  (let* ((diary-file-path (format "%s%s"
				  *diary-directory-path*
				  (format-time-string "%Y/%m/%d.md"))))

    (if (file-exists-p diary-file-path)
	(message "already exists file.")
      (with-temp-buffer
	(insert (generate-hugo-diary-header-text))
	(write-file diary-file-path)))
    (find-file diary-file-path)
    )
  )
