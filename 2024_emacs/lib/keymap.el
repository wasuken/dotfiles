(global-set-key (kbd "C-h") #'delete-backward-char)

;; my tools keymap
(global-set-key (kbd "C-c b i") #'insert-hugo-header)
(global-set-key (kbd "C-c l")   #'create-md-link)
(global-set-key (kbd "C-x t s") #'toggle-window-split)
(global-set-key (kbd "C-c M-d") (lambda ()
				  (interactive)
				  (generate-today-diary-file)
				  (generate-today-yaml-file)))

(global-set-key (kbd "C-c M-w") #'generate-weekly-file)

;; my keymap
(global-set-key (kbd "C-c g") 'find-grep)
(global-set-key (kbd "C-<") #'(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C->") #'other-window)
(global-set-key (kbd "C-c z c") #'tab-bar-new-tab)
(global-set-key (kbd "C-c z n") #'tab-bar-switch-to-next-tab)
(global-set-key (kbd "C-c z b") #'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-c x r") #'revert-buffer-no-confirm)
(global-set-key (kbd "C-c s s") #'replace-string)


