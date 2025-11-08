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


(setq *diary-template* "# 📝 Daily Reflection

## 毎日チェックリスト

- [ ] あすけん
  - [ ] 朝
  - [ ] 昼
  - [ ] 夜
- [ ] 読書15分
- [ ] LeetCode一問30分制限
- [ ] ベッドメイキング
- [ ] 歯磨き
- [ ] シャワー/風呂
- [ ] 瞑想
- [ ] 洗濯物皿洗いいずれか
- [ ] 外気を浴びる
- [ ] 日記
- [ ] ラジオ体操
- [ ] 筋トレプランク
- [ ] 筋トレメイン
- [ ] 体重計測

## 今日どうだった


## 今日よかったこと


## 明日やること

## メモ
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

(defun add-tags-to-content (content tags)
  "本文末尾にタグをハッシュタグ形式で追加する"
  (if tags
      (let ((cleaned-content (string-trim content))
            (tag-line (mapconcat (lambda (tag) (format "#%s" tag)) tags " ")))
        (format "%s\n\n%s" cleaned-content tag-line))
    content))

(defun hatena-blog-post (title content categories tags &optional draft)
  "はてなブログに投稿する関数（カテゴリ・タグ対応版）
TITLE: エントリのタイトル
CONTENT: 本文
CATEGORIES: カテゴリのリスト（文字列のリスト）
TAGS: タグのリスト（文字列のリスト、本文末尾に#形式で追加される）
DRAFT: 下書きとして保存する場合はt"
  (let* ((url (format "https://blog.hatena.ne.jp/%s/%s/atom/entry"
                      hatena-user-id
                      hatena-blog-id))
         (auth (format "%s:%s" hatena-user-id hatena-blog-api-key))
         (category-xml (if categories
                           (mapconcat (lambda (cat)
                                        (format "  <category term=\"%s\" />"
                                                (xml-escape-string cat)))
                                      categories "\n")
                         ""))
         (processed-content (add-tags-to-content 
                             (remove-hugo-frontmatter content) 
                             tags))
         (xml (format "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<entry xmlns=\"http://www.w3.org/2005/Atom\"
       xmlns:app=\"http://www.w3.org/2007/app\">
  <title>%s</title>
%s
  <content type=\"text/markdown\">
    %s
  </content>
  <app:control>
    <app:draft>%s</app:draft>
  </app:control>
</entry>"
                      (xml-escape-string title)
                      category-xml
                      (xml-escape-string processed-content)
                      (if draft "yes" "no"))))
    
    ;; デバッグ用情報出力
    (message "リクエストURL: %s" url)
    (message "認証情報: %s" auth)
    (message "送信XML: %s" xml)
    
    (request
      url
      :type "POST"
      :headers `(("Content-Type" . "application/xml")
                 ("Authorization" . ,(concat "Basic "
                                             (base64-encode-string auth))))
      :data xml
      :parser 'buffer-string
      :sync t  ; 同期実行に変更
      :timeout 30  ; タイムアウトを30秒に設定
      :success (cl-function
		(lambda (&key data response &allow-other-keys)
                  (message "投稿成功！")
                  (message "レスポンス: %s" data)))
      :error (cl-function
              (lambda (&key data error-thrown response &allow-other-keys)
		(message "エラー発生:")
                (message "Error thrown: %S" error-thrown)
                (message "Response data: %s" data)
                (when response
                  (message "Status code: %s" (request-response-status-code response))))))))

;; 同期版の簡易テスト関数
(defun hatena-blog-test-connection ()
  "はてなブログAPI接続テスト"
  (interactive)
  (let* ((url (format "https://blog.hatena.ne.jp/%s/%s/atom/entry"
                      hatena-user-id
                      hatena-blog-id))
         (auth (format "%s:%s" hatena-user-id hatena-blog-api-key)))
    (message "テスト接続開始...")
    (request
      url
      :type "GET"
      :headers `(("Authorization" . ,(concat "Basic "
                                             (base64-encode-string auth))))
      :sync t
      :timeout 30
      :success (cl-function
		(lambda (&key data response &allow-other-keys)
                  (message "接続成功！ステータス: %s" 
                           (request-response-status-code response))))
      :error (cl-function
              (lambda (&key data error-thrown response &allow-other-keys)
		(message "接続エラー:")
                (message "Error: %S" error-thrown)
                (when response
                  (message "Status: %s" (request-response-status-code response))
                  (message "Data: %s" data)))))))

(defun parse-frontmatter-categories-tags (content)
  "Hugoフロントマターからカテゴリとタグを抽出する"
  (let ((categories '())
        (tags '())
        (title nil))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (when (looking-at "---\n")
        (let ((frontmatter-end nil))
          (forward-line 1)
          (when (re-search-forward "^---$" nil t)
            (setq frontmatter-end (point))
            (goto-char (point-min))
            (forward-line 1)
            
            ;; タイトルを抽出
            (when (re-search-forward "^title: \"\\([^\"]+\\)\"" frontmatter-end t)
              (setq title (match-string 1)))
            
            ;; カテゴリを抽出（クォートあり・なし両対応）
            (goto-char (point-min))
            (forward-line 1)
            (when (re-search-forward "^categories:" frontmatter-end t)
              (forward-line 1)
              (while (and (< (point) frontmatter-end)
                          (or (looking-at "^  - \"\\([^\"]+\\)\"")
                              (looking-at "^  - \\([^\n]+\\)")))
                (push (match-string 1) categories)
                (forward-line 1)))
            
            ;; タグを抽出（クォートあり・なし両対応）
            (goto-char (point-min))
            (forward-line 1)
            (when (re-search-forward "^tags:" frontmatter-end t)
              (forward-line 1)
              (while (and (< (point) frontmatter-end)
                          (or (looking-at "^  - \"\\([^\"]+\\)\"")
                              (looking-at "^  - \\([^\n]+\\)")))
                (push (match-string 1) tags)
                (forward-line 1)))))))
    (list (reverse categories) (reverse tags) title)))

(defun my-post-current-buffer-to-hatena ()
  "現在のバッファの内容をはてなブログに投稿する（カテゴリ・タグ対応版）"
  (interactive)
  (let* ((content (buffer-string))
         (frontmatter-data (parse-frontmatter-categories-tags content))
         (detected-categories (nth 0 frontmatter-data))
         (detected-tags (nth 1 frontmatter-data))
         (detected-title (nth 2 frontmatter-data))
         (title (read-string 
                 (format "タイトル%s: "
                         (if detected-title
                             (format " [検出: %s]" detected-title)
                           ""))
                 (or detected-title "")))
         (categories-str (read-string 
                          (format "カテゴリ（カンマ区切り）%s: "
                                  (if detected-categories
                                      (format " [検出: %s]" 
                                              (string-join detected-categories ", "))
                                    ""))
                          (if detected-categories
                              (string-join detected-categories ", ")
                            "")))
         (tags-str (read-string 
                    (format "タグ（カンマ区切り）%s: "
                            (if detected-tags
                                (format " [検出: %s]" 
                                        (string-join detected-tags ", "))
                              ""))
                    (if detected-tags
                        (string-join detected-tags ", ")
                      "")))
         (categories (if (string-empty-p categories-str)
                         '()
                       (mapcar #'string-trim 
                               (split-string categories-str ","))))
         (tags (if (string-empty-p tags-str)
                   '()
                 (mapcar #'string-trim 
                         (split-string tags-str ","))))
         (draft (y-or-n-p "下書きとして保存？")))
    
    (when tags
      (message "タグ [%s] を本文末尾に #形式で追加します"
               (string-join tags ", ")))
    
    (hatena-blog-post title content categories tags draft)))

;; より簡潔な投稿用の関数
(defun my-post-to-hatena-quick ()
  "クイック投稿（フロントマター完全自動解析）"
  (interactive)
  (let* ((content (buffer-string))
         (frontmatter-data (parse-frontmatter-categories-tags content))
         (categories (nth 0 frontmatter-data))
         (tags (nth 1 frontmatter-data))
         (title (nth 2 frontmatter-data))
         (draft (y-or-n-p "下書きとして保存？")))
    
    (unless title
      (setq title (read-string "タイトル: ")))
    
    (when tags
      (message "タグ [%s] を本文末尾に #形式で追加します"
               (string-join tags ", ")))
    
    (hatena-blog-post title content categories tags draft)))
