(defun timeclock-provide-description (desc)
  (interactive "MDescription: ")
  (let ((extant-timelog (find-buffer-visiting timeclock-file)))
    (with-current-buffer (find-file-noselect timeclock-file t)
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-max))
	  (re-search-backward "^i [12][90][901][0-9]/[01][0-9]/[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]")
	  (if-let ((p (search-forward "  " nil t)))
	    (progn
	      (goto-char (- p 2))
	      (kill-line))
	    (end-of-line))
	  (insert "  " desc))))))
