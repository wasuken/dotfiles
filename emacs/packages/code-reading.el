;;; code-reading.el --- Cross-file code reading support -*- lexical-binding: t; -*-

;;; Commentary:
;; ファイル横断のコードリーディング支援
;; bookmark.el: フロー単位で複数ファイル・複数行にマーキング

;;; Code:

(use-package bookmark
  :straight (:type built-in)
  :custom
  (bookmark-save-flag 1)
  (bookmark-default-file
   (expand-file-name ".bookmarks"
                      (or (locate-dominating-file default-directory ".git")
                          user-emacs-directory)))
  :bind
  (("C-x r m" . bookmark-set)
   ("C-x r b" . bookmark-jump)
   ("C-x r l" . bookmark-bmenu-list)))

(provide 'code-reading)
;;; code-reading.el ends here
