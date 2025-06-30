(require 'url)
(require 'dom)
(require 'request)
(require 'yaml)

(defgroup summarize-url nil
  "Settings for summarize-url."
  :group 'tools)

(defcustom summarize-directory (expand-file-name "~/memo/summarize-lab/")
  "Directory to store summarized articles.
Default is ~/summarize-lab/"
  :type 'directory
  :group 'summarize-url)

(defun get-frequent-words (text)
  "Get top 3 most frequent meaningful words from TEXT."
  (let ((word-count (make-hash-table :test 'equal))
        (stop-words '("the" "a" "an" "and" "or" "but" "in" "on" "at" "to" "for" "is" "are" "was" "were")))
    (dolist (word (split-string text))
      (let ((word-lower (downcase word)))
        (unless (member word-lower stop-words)
          (puthash word-lower
                   (1+ (gethash word-lower word-count 0))
                   word-count))))
    (let (words)
      (maphash (lambda (k v) (push (cons k v) words)) word-count)
      (mapcar #'car
              (seq-take (sort words
                              (lambda (a b) (> (cdr a) (cdr b))))
			3)))))

(defun create-yaml-frontmatter (title tags)
  "Create YAML frontmatter with TITLE and TAGS."
  (format "---
title: \"%s\"
description:
date: %s
draft: false
categories:
  - \"article\"
  - \"summarize\"
tags:%s
---\n\n"
          title
          (format-time-string "%Y-%m-%d")
          (mapconcat (lambda (tag)
                       (format "\n  - \"%s\"" tag))
                     tags
                     "")))

;; -*- lexical-binding: t; -*-
(defun summarize-url (input-url)
  "Summarize URL and create a markdown file with YAML frontmatter."
  (interactive "sEnter URL: ")
  (let ((url input-url))  ; 外側でURLを保持
    (request
      url
      :parser 'buffer-string
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)
         (with-temp-buffer
           (insert data)
           (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
                  (title (dom-text (car (dom-by-tag dom 'title))))
                  (body-text (dom-texts (dom-by-tag dom 'body)))
                  (tags (get-frequent-words body-text))
                  (file-name (concat summarize-directory
                                     (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" title)
                                     ".md")))
             (make-directory summarize-directory t)
             (with-temp-file file-name
               (insert (create-yaml-frontmatter title tags))
               (insert (format "Source: %s\n\n" url)))
             (find-file file-name)))))
      :error
      (cl-function
       (lambda (&rest args &key error-thrown &allow-other-keys)
         (message "Error: %S" error-thrown))))))

(provide 'summarize-url)
