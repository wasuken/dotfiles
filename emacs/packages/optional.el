;;; optional.el --- Optional packages -*- lexical-binding: t; -*-

;;; Commentary:
;; たまに使うパッケージ群

;;; Code:

;; IME設定
(use-package tr-ime
  :if (memq system-type '(cygwin windows-nt ms-dos))
  :config
  (setq default-input-method "W32-IME")
  (tr-ime-standard-install)
  (w32-ime-initialize))

(when (eq system-type 'gnu/linux)
  (use-package mozc
    :config
    (setq default-input-method "japanese-mozc")
    (setq mozc-candidate-style 'overlay)))

;; その他ツール
(use-package restart-emacs
  :bind ( :map my-quit-map
          ("r" . restart-emacs)))

(use-package neotree
  :bind ("C-c t" . neotree))


(defvar my/elfeed-feeds-file "~/.emacs.d/elfeed-feeds.txt"
  "1行 = URL,tag1,tag2,... 形式のフィードリストファイル.")

(defvar my/elfeed-default-feeds
  '(("http://nullprogram.com/feed/" blog tech)
    ("https://planet.emacslife.com/atom.xml" emacs blog)
    ("https://zenn.dev/topics/react/feed" zenn react)
    ("https://zenn.dev/topics/llm/feed" zenn llm)
    ("https://b.hatena.ne.jp/hotentry/it.rss" hatena it))
  "読み込み失敗時のフォールバック.")

(defun my/elfeed-parse-line (line)
  "\"URL,tag1,tag2\" を (URL tag1 tag2) に変換."
  (let ((parts (split-string line "," t "[ \t]+")))
    (when parts
      (cons (car parts)
            (mapcar #'intern (cdr parts))))))

(defun my/elfeed-load-feeds ()
  (condition-case err
      (if (file-readable-p my/elfeed-feeds-file)
          (with-temp-buffer
            (insert-file-contents my/elfeed-feeds-file)
            (let* ((lines (split-string (buffer-string) "\n" t "[ \t\r]+"))
                   (parsed (delq nil (mapcar #'my/elfeed-parse-line lines))))
              (if parsed parsed my/elfeed-default-feeds)))
        my/elfeed-default-feeds)
    (error
     (message "elfeed-feeds読み込み失敗: %s, デフォルトを使用" err)
     my/elfeed-default-feeds)))

(use-package elfeed
  :bind (:map elfeed-search-mode-map
	      ("U" . elfeed-update))
  :config
  (global-set-key (kbd "C-x w") 'elfeed)
  (setq elfeed-feeds (my/elfeed-load-feeds)))

(use-package mastodon
  :straight (mastodon :type git
                      :host codeberg
                      :repo "martianh/mastodon.el")
  :custom
  (mastodon-instance-url mastodon-instance-url)
  (mastodon-active-user mastodon-active-user)
  (mastodon-tl--show-avatars t))

(use-package habitica)

(use-package leetcode
  :config
  (setq leetcode-prefer-language "python3")
  (setq leetcode-prefer-sql "mysql"))

;; Google Translate
(use-package google-translate
  :config
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "ja")
  (global-set-key (kbd "C-c T t") 'google-translate-at-point)
  (global-set-key (kbd "C-c T T") 'google-translate-query-translate))

(provide 'optional)
;;; optional.el ends here
