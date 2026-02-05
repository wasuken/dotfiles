(set-face-attribute 'default nil :family "HackGen" :height 150)
;; 絵文字用フォールバックを追加
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'prepend)
(set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)

(provide 'font)
;;; font.el ends here


