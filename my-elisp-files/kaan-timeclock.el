(load "timeclock")
(load "dash.el") ; for (-take)

(defvar work-hours-per-day 6.4)

(defun timeclock-sum-all-hours ()
  (interactive)
  (let ((days (timeclock-day-alist (timeclock-log-data)))
  (total 0.0))
    (while days
      (setq total (+ total (timeclock-day-length (cdar days)))
      days (cdr days)))
    (message (timeclock-seconds-to-string total))))

(defun timeclock-overtime ()
  (interactive)
  (let* ((days (cdr (timeclock-day-alist (timeclock-log-data))))
   (worked-days-number (length days))
   (total 0.0))
    (while days
      (setq total (+ total (timeclock-day-length (cdar days)))
      days (cdr days)))
    (message (timeclock-seconds-to-string
        (- total (* 3600 work-hours-per-day worked-days-number))))))

(defun timeclock-last-x-days-overtime (number-of-days)
  (interactive "nNumber of days: ")
  (let* ((days (-take number-of-days (cdr (timeclock-day-alist (timeclock-log-data)))))
         (worked-days-number (length days))
         (total 0.0))
    (while days
      (setq total (+ total (timeclock-day-length (cdar days)))
            days (cdr days)))
    (message (timeclock-seconds-to-string
              (- total (* 3600 work-hours-per-day worked-days-number))))))

(defun timeclock-hours-worked-today ()
  (interactive)
  (let ((today (car (timeclock-day-alist (timeclock-log-data)))))
    (message (timeclock-seconds-to-string (timeclock-day-length today)))))

(defun timeclock-hours-to-days-end ()
  (interactive)
  (let ((today (car (timeclock-day-alist (timeclock-log-data)))))
    (message (timeclock-seconds-to-string (- (* 3600 work-hours-per-day) (timeclock-day-length today))))))

(defun timeclock-open-timelog-file ()
  (interactive)
  (find-file timeclock-file)
  (let ((day (format-time-string "%d")))
    (write-file (concat "./timelog-backup/" day "-timelog")))
  (find-file timeclock-file)
  (goto-char (point-max)))


;;; For printing out elements

;; (setq theq nil)

;; (defun eat-it (c)
;;   (setq theq (cons c theq)))

;; (defun print-it (el)
;;   (print el 'eat-it)
;;   (insert (concat (nreverse theq))))

;; (print-it (car (timeclock-day-alist (timeclock-log-data))))
