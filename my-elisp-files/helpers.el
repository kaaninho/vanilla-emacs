(defun buffer-convert-umlauts ()
  (interactive)
  (mark-page)
  (replace-string "ä" "ae")
  (mark-page)
  (replace-string "ü" "ue")
  (mark-page)
  (replace-string "ö" "oe")
  (mark-page)
  (replace-string "ß" "ss"))
