(defun replace-sepa (left &optional right)
  (setq right (or right left))
  (let ((cur-strs  (buffer-substring (region-beginning) (region-end))))
	(delete-region (region-beginning) (region-end))
	(insert (concatenate 'string left
						 (replace-regexp-in-string ","
												   (concatenate 'string
																right
																","
																left)
												   cur-strs)
						 right))  (print right)))
(defun replace-dc ()
  (interactive)
  (replace-sepa "\""))
(defun replace-sc ()
  (interactive)
  (replace-sepa "\'"))
(defun replace-paren ()
  (interactive)
  (replace-sepa "(" ")"))
(defun insert-> ()
  (interactive)
  (insert " -> "))
(defun insert<- ()
  (interactive)
  (insert " <- "))
(defun insert=> ()
  (interactive)
  (insert " => "))
(defun insert<= ()
  (interactive)
  (insert " <= "))

