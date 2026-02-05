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

(use-package mozc
  :config
  (setq default-input-method "japanese-mozc")
  (setq mozc-candidate-style 'overlay))

;; その他ツール
(use-package restart-emacs
  :bind ( :map my-quit-map
          ("r" . restart-emacs)))

(use-package neotree
  :bind ("C-c t" . neotree))

(use-package elfeed
  :config
  (global-set-key (kbd "C-x w") 'elfeed)
  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
          "https://planet.emacslife.com/atom.xml"
          "https://zenn.dev/topics/react/feed"
          "https://zenn.dev/topics/llm/feed"
          "https://b.hatena.ne.jp/hotentry/it.rss")))

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

(provide 'optional)
;;; optional.el ends here
