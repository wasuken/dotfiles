;; tab-bar

(defun insert-current-time()
  (interactive)
  (insert (format-time-string "%Y-%m-%d(%a) %H:%M:%S" (current-time))))

(defun format-yaml-lines (items indent)
  (string-join (mapcar #'(lambda (x) (format "%s- \"%s\"" indent x)) items) "
"))

(defun generate-header-text (title description date categories tags contents)
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
     (generate-header-text title
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


(setq *diary-template* "## 📝 Daily Reflection

### 今日の出来事・感情


### 体調・気分の変化


### 食事について


### 運動・活動について


### 睡眠について


### ストレス・メンタル


### 気づき・学び


### 明日への課題・目標


### 自由記述


")

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
  (generate-header-text "日記"
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
  (insert (generate-header-text
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
(setf *weekly-directory-path* (expand-file-name "~/memo/weekly/"))

(defun get-week-dates ()
  "Get start and end dates of current week."
  (let* ((now (current-time))
         (decoded-time (decode-time now))
         (current-day (nth 6 decoded-time))  ; 0-6, 0 is Sunday
         ;; 何日前に戻るか計算（日曜起点）
         (days-to-subtract current-day)
         ;; 週初め（日曜日）の時間を計算
         (week-start (time-subtract now
                                    (seconds-to-time (* days-to-subtract 86400))))
         ;; 週末（土曜日）の時間を計算（週初めから6日後）
         (week-end (time-add week-start
                             (seconds-to-time (* 6 86400)))))
    (list (format-time-string "%Y-%m-%d" week-start)
          (format-time-string "%Y-%m-%d" week-end))))


(defun generate-weekly-file ()
  "本日の日記ファイルを生成する"
  (interactive)
  ;; カレントディレクトリ
  (let* ((week-dates (get-week-dates))
	 (weekly-file-path (format "%s%s_%s.md"
				   *weekly-directory-path*
				   (car week-dates)
				   (cadr week-dates)
				   )))

    (if (file-exists-p weekly-file-path)
	(message "already exists file.")
      (with-temp-buffer
	(insert (generate-header-text
		 "週報"
		 ""
		 (format-time-string "%Y-%m-%d")
		 "  - \"weekly\""
		 "  - \"life\""
		 ;; *weekly-mental-template*
		 ""
		 ))
	(write-file weekly-file-path)))
    (find-file weekly-file-path)
    )
  )


(setq *today-yaml-template* "date: \"YYYY-MM-DD\"  # 日付（年-月-日形式）
day_of_week: \"\"  # 曜日
weather: \"\"  # 天気
# 健康指標（1-10段階評価）
physical_condition:  # 体調（1=最悪、10=最高）
energy_level:  # エネルギーレベル（1=疲労困憊、10=活力満点）
morning_mood:  # 朝の気分（1=憂鬱、10=爽快）
focus_level:  # 集中力（1=全く集中できない、10=高度に集中）
stress_level:  # ストレスレベル（1=ストレスなし、10=極度のストレス）
achievement_feeling:  # 達成感（1=何も成し遂げられなかった、10=大きな成果を感じる）
# 睡眠データ
bedtime: \"\"  # 就寝時刻（HH:MM形式）
wake_time: \"\"  # 起床時刻（HH:MM形式）
sleep_hours:  # 睡眠時間（時間単位）
sleep_quality:  # 睡眠の質（1=最悪、10=最高）
dreams_remembered:  # 夢を覚えているか（true/false）
sleep_environment_score:  # 睡眠環境の良さ（1=最悪、10=理想的）
# 栄養データ
meals:
breakfast:  # 朝食
  time: \"\"  # 摂取時刻（HH:MM形式）
  protein_grams:  # タンパク質摂取量（グラム）
  carbs_quality:  # 炭水化物の質（1=精製糖質中心、10=複合炭水化物中心）
  description: \"\"  # 食事内容の説明
lunch:  # 昼食
  time: \"\"  # 摂取時刻（HH:MM形式）
  protein_grams:  # タンパク質摂取量（グラム）
  carbs_quality:  # 炭水化物の質（1=精製糖質中心、10=複合炭水化物中心）
  description: \"\"  # 食事内容の説明
dinner:  # 夕食
  time: \"\"  # 摂取時刻（HH:MM形式）
  protein_grams:  # タンパク質摂取量（グラム）
  carbs_quality:  # 炭水化物の質（1=精製糖質中心、10=複合炭水化物中心）
  description: \"\"  # 食事内容の説明
coffee_count:  # コーヒー摂取杯数
water_liters:  # 水分摂取量（リットル）
junk_food_consumed:  # ジャンクフード摂取の有無（true/false）
supplements_taken: []  # 摂取したサプリメントのリスト
# 運動データ
strength_training:  # 筋力トレーニング
  performed:  # 実施の有無（true/false）
  duration_minutes:  # 実施時間（分）
  intensity:  # 強度（1=軽い、10=最大強度）
  exercises: []  # 実施した種目のリスト
cardio:  # 有酸素運動
  performed:  # 実施の有無（true/false）
  duration_minutes:  # 実施時間（分）
  intensity:  # 強度（1=軽い、10=最大強度）
  type: \"\"  # 運動の種類（ランニング、サイクリングなど）
stretching:  # ストレッチ
  performed:  # 実施の有無（true/false）
  duration_minutes:  # 実施時間（分）
  type: \"\"  # ストレッチの種類（静的、動的など）
daily_steps:  # 1日の歩数
# デジタル習慣
screen_time_hours:  # スクリーンタイム（時間）
social_media_hours:  # SNS利用時間（時間）
blue_light_after_sunset:  # 日没後のブルーライト暴露（true/false）
phone_in_bedroom:  # 寝室でのスマホ使用（true/false）
# テストステロン指標（1-10段階評価）
morning_erection:  # 朝立ちの状況（1=全くなし、10=強い）
muscle_strength_feeling:  # 筋力の感覚（1=弱い、10=強い）
motivation_level:  # やる気レベル（1=無気力、10=意欲満々）
risk_taking_tendency:  # リスクを取る傾向（1=保守的、10=積極的）
social_confidence:  # 社会的自信（1=内向的、10=外向的）
competitive_drive:  # 競争心（1=競争を避ける、10=競争を求める）
")

(defun generate-today-yaml-file ()
  "本日のyamlを生成する"
  (interactive)
  ;; カレントディレクトリ
  (let* ((diary-file-path (format "%s%s"
				  *diary-directory-path*
				  (format-time-string "%Y/%m/%d.yaml"))))

    (if (file-exists-p diary-file-path)
	(message "already exists file.")
      (with-temp-buffer
	(insert *today-yaml-template*)
	(write-file diary-file-path)))
    (find-file diary-file-path)
    )
  )

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
	(insert (generate-header-text
		 "日記"
		 ""
		 (format-time-string "%Y-%m-%d")
		 "  - \"diary\""
		 "  - \"life\""
		 *diary-template*
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
