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

(use-package symbol-overlay
  :straight t
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-C" . symbol-overlay-remove-all))
  :hook (prog-mode . symbol-overlay-mode)
  :config
  ;; narrow-to-defun がデフォルトで disabled command 扱いなので有効化
  (put 'narrow-to-defun 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'widen 'disabled nil)

  (global-set-key (kbd "C-x n d") #'narrow-to-defun)
  (global-set-key (kbd "C-x n w") #'widen))

(provide 'code-reading)
;;; code-reading.el ends here
