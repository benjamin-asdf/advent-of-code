




























(progn
  (goto-char (point-min))
  (narrow-to-region (point) (point-at-eol))
  (while
      (when (not (eobp)) (forward-char 1))
    (let ((highest 0))
      (message
       "%s"
       (string-to-number
	(buffer-substring (point) (1+ (point))))))))
