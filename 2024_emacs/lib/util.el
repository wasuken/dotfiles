;; tab-bar

(defun insert-current-time()
  (interactive)
  (insert (format-time-string "%Y-%m-%d(%a) %H:%M:%S" (current-time))))

(defun format-yaml-lines (items indent)
  (string-join (mapcar #'(lambda (x) (format "%s- \"%s\"" indent x)) items) "
"))

(defun generate-diary-text (title description date categories tags contents)
  (format "---
title: \"%s\"
description: %s
date: %s
draft: false
categories:
%s
tags:
%s
---

%s
" title description date categories tags contents))

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
     (generate-diary-text title
			  ""
			  ts
			  (format-yaml-lines categories "  ")
			  (format-yaml-lines tags "  ")
			  ""))
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


(setq *diary-mental-template* "
---

# mental

悪=-1, 普=0, 良=1

## 基礎表

| 項目       | 評価 |
| ---------- | ---- |
| 気分の変化 |        |
| 睡眠質     |       |
| 睡眠寝付き |        |
| 睡眠目覚め |        |
| 活動レベル |        |

## 食事

### 食欲表

| 項目 | 評価 |
| ---- | ---- |
| 朝   |      |
| 昼   |      |
| 夜   |      |

### 朝

### 昼

### 夜

## 活動内容

### 思考パターン表

| 項目 | 評価 |
| ---- | ---- |
| 朝   |      |
| 昼   |      |
| 夜   |      |

## 所感
")

(defun generate-hugo-diary-header-text ()
  (generate-diary-text "日記"
		       "日記"
		       (format-time-string "%Y-%m-%d")
		       "  - \"diary\""
		       "  - \"life\""
		       "")
  )

(defun insert-hugo-diary-header ()
  "本日の日記ヘッダを生成してカレントバッファの先頭に挿入する"
  (interactive)
  (goto-char 0)
  (insert (generate-diary-text
	   "日記"
	   ""
	   (format-time-string "%Y-%m-%d")
	   "  - \"diary\""
	   "  - \"life\""
	   ;; *diary-mental-template*
	   ""
	   ))
  )

(setf *diary-directory-path* (expand-file-name "~/memo/diary/"))

(defun generate-today-diary-file ()
  "本日の日記ファイルを生成する"
  (interactive)
  ;; カレントディレクトリ
  (let* ((diary-file-path (format "%s%s"
				  *diary-directory-path*
				  (format-time-string "%Y/%m/%d.md"))))

    (if (file-exists-p diary-file-path)
	(message "already exists file.")
      (with-temp-buffer
	(insert (generate-diary-text
		 "日記"
		 ""
		 (format-time-string "%Y-%m-%d")
		 "  - \"diary\""
		 "  - \"life\""
		 ;; *diary-mental-template*
		 ""
		 ))
	(write-file diary-file-path)))
    (find-file diary-file-path)
    )
  )

;; ruby
(defun run-ruby-test-unit ()
  (interactive)
  (let ((test-command (format "bundle exec ruby %s" (buffer-file-name))))
    (compile test-command)))


(defun simple-formatting ()
  (interactive)
  (if (use-region-p)
      (let* ((begin (region-beginning))
	     (end (region-end))
	     (region-text (buffer-substring-no-properties begin end)))
	(delete-region begin end)
	(insert (replace-regexp-in-string "[。.]" "\\&\n" region-text)))
    (message "リージョンが選択されていません。"))
  )

(defun revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  ;;(message "force-reverting value is %s" force-reverting)
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))

(defun my/save-all-buffers ()
  (save-some-buffers "!"))


(require 'request)
(require 'auth-source)

(defgroup hatena-blog nil
  "Settings for hatena-blog."
  :group 'external)

(defcustom hatena-blog-config-file
  (expand-file-name "config.el" user-emacs-directory)
  "Path to hatena blog configuration file."
  :type 'file
  :group 'hatena-blog)

(defcustom hatena-user-id nil
  "Hatena User ID (e.g. 'your-user-id')."
  :type 'string
  :group 'hatena-blog)

(defcustom hatena-blog-id nil
  "Hatena Blog ID (e.g. 'your-blog-id.hatenablog.com')."
  :type 'string
  :group 'hatena-blog)

(defcustom hatena-blog-api-key nil
  "Hatena Blog API Key."
  :type 'string
  :group 'hatena-blog)

;; 設定ファイルを読み込む関数
(defun hatena-blog-load-config ()
  "Load hatena blog configuration from file."
  (when (file-exists-p hatena-blog-config-file)
    (load hatena-blog-config-file)))

;; 初期化時に設定を読み込む
(hatena-blog-load-config)

;; 設定用の変数
;; 設定用の変数
(defvar hatena-user-id "your-user-id"
  "はてなブログのユーザーID")
(defvar hatena-blog-id "your-blog-id.hatenablog.com"
  "はてなブログのID")
(defvar hatena-blog-api-key "your-api-key"
  "はてなブログのAPIキー")

(defun xml-escape-string (string)
  "Escape XML special characters in STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "[&<>\"]" nil t)
      (replace-match
       (pcase (match-string 0)
         ("&" "&amp;")
         ("<" "&lt;")
         (">" "&gt;")
         ("\"" "&quot;"))
       t t))
    (buffer-string)))

(defun remove-hugo-frontmatter (content)
  "Remove Hugo frontmatter from content."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    ;; フロントマターが存在するか確認
    (if (looking-at "---\n")
        (progn
          ;; 2つ目の --- を探す
          (forward-line 1)
          (if (re-search-forward "^---$" nil t)
              (progn
                ;; フロントマター部分を削除して残りを返す
                (forward-line 1)
                (buffer-substring (point) (point-max)))
            ;; 2つ目の --- が見つからない場合は元の内容を返す
            content))
      ;; フロントマターがない場合は元の内容を返す
      content)))

(defun hatena-blog-post (title content &optional draft)
  "はてなブログに投稿する関数
TITLE: エントリのタイトル
CONTENT: 本文
DRAFT: 下書きとして保存する場合はt"
  (let* ((url (format "https://blog.hatena.ne.jp/%s/%s/atom/entry"
                      hatena-user-id
                      hatena-blog-id))
         (auth (format "%s:%s" hatena-user-id hatena-blog-api-key))
         (xml (format "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<entry xmlns=\"http://www.w3.org/2005/Atom\"
       xmlns:app=\"http://www.w3.org/2007/app\">
  <title>%s</title>
  <content type=\"text/markdown\">
    %s
  </content>
  <app:control>
    <app:draft>%s</app:draft>
  </app:control>
</entry>"
                      (xml-escape-string title)
                      (xml-escape-string (remove-hugo-frontmatter content))
                      (if draft "yes" "no"))))
    (request
      url
      :type "POST"
      :headers `(("Content-Type" . "application/xml")
                 ("Authorization" . ,(concat "Basic "
                                             (base64-encode-string auth))))
      :data xml
      :parser 'buffer-string
      :success (cl-function
		(lambda (&key data &allow-other-keys)
                  (message "投稿成功！")))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
		(message "エラー: %S" error-thrown))))))

(defun my-post-current-buffer-to-hatena ()
  "現在のバッファの内容をはてなブログに投稿する"
  (interactive)
  (let ((title (read-string "タイトル: "))
        (content (buffer-string))
        (draft (y-or-n-p "下書きとして保存？")))
    (hatena-blog-post title content draft)))

