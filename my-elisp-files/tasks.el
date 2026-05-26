;;; tasks.el --- Lightweight GTD for Obsidian -*- lexical-binding: t; -*-

(require 'org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup tasks nil
  "Lightweight markdown task management."
  :group 'applications)

(defcustom my/tasks-directory
  (concat obsidian-directory  "/tasks")
  "Directory containing task files."
  :type 'directory)

(defcustom my/tasks-file
  "tasks.md"
  "Main task file."
  :type 'string)

(defcustom my/archive-file
  "archive.md"
  "Archive file."
  :type 'string)

(defun my/tasks-path ()
  (expand-file-name my/tasks-file my/tasks-directory))

(defun my/archive-path ()
  (expand-file-name my/archive-file my/tasks-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/tasks--today-string ()
  (format-time-string "%Y-%m-%d"))

(defun my/tasks--read-date (&optional with-time prompt)
  "Read a date via org's calendar interface.
If WITH-TIME is non-nil, also prompt for a time.
Returns a string formatted as YYYY-MM-DD or YYYY-MM-DD HH:MM."
  (format-time-string
   (if with-time "%Y-%m-%d %H:%M" "%Y-%m-%d")
   (org-read-date with-time t nil prompt)))

(defun my/tasks--extract-date (emoji string)
  "Extract date following EMOJI in STRING.
Supports YYYY-MM-DD and YYYY-MM-DD HH:MM."
  (when (and string
             (string-match
              (concat emoji " \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(?: [0-9]\\{2\\}:[0-9]\\{2\\}\\)?\\)")
              string))
    (match-string 1 string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/tasks-capture (title)
  "Capture task into tasks.md."
  (interactive "sTask: ")
  (with-temp-buffer
    (insert (format "- [ ] %s\n  status:: inbox\n\n" title))
    (append-to-file (point-min) (point-max) (my/tasks-path)))
  (message "Task captured."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/read-tasks ()
  "Return task list as plists.
Includes :pos for buffer position (marker) in tasks.md."
  (let* ((tasks-path (my/tasks-path))
         (tasks-buf (find-file-noselect tasks-path)))
    (with-current-buffer tasks-buf
      (save-excursion
        (goto-char (point-min))
        (let (tasks)
          (while (re-search-forward
                  "^\\([[:space:]]*\\)- \\[\\([ xX]\\)\\] \\(.*?\\)\\(?: \\([📅⏳⏰].*\\)\\)?$"
                  nil
                  t)
            (let* ((start (match-beginning 0))
                   (pos (set-marker (make-marker) start tasks-buf))
                   (done (not (string= (match-string 2) " ")))
                   (title (match-string 3))
                   (meta (match-string 4))
                   (due (when meta (my/tasks--extract-date "📅" meta)))
                   (scheduled (when meta (my/tasks--extract-date "⏳" meta)))
                   (reminder (when meta (my/tasks--extract-date "⏰" meta)))
                   status
                   project)
              ;; Read indented properties
              (save-excursion
                (forward-line 1)
                (while (looking-at "^[[:space:]]+\\([a-z]+\\):: \\(.*\\)$")
                  (let ((key (match-string 1))
                        (value (match-string 2)))
                    (pcase key
                      ("status" (setq status value))
                      ("project" (setq project value)))
                    (forward-line 1))))
              (push (list
                     :title title
                     :done done
                     :status status
                     :today (string= status "today")
                     :due due
                     :scheduled scheduled
                     :reminder reminder
                     :project project
                     :pos pos)
               tasks)))
          (reverse tasks))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/tasks-by-status (status)
  (seq-filter (lambda (task)
                (and (not (plist-get task :done))
                     (string= status (plist-get task :status))))
              (my/read-tasks)))

(defun my/tasks-today ()
  (seq-filter (lambda (task)
                (and (not (plist-get task :done)) (plist-get task :today)))
              (my/read-tasks)))

(defun my/tasks-due-today ()
  (let ((today (my/tasks--today-string)))
    (seq-filter (lambda (task)
                  (and (not (plist-get task :done)) (equal today (plist-get task :due))))
                (my/read-tasks))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local my/tasks-view-query nil
  "The query function used for the current view.")

(defvar-local my/tasks-view-query-arg nil
  "The argument passed to the query function.")

(defvar my/tasks-views-cycle-list
  '((my/tasks-show-inbox . "*Inbox*")
    (my/tasks-show-today . "*Today*")
    (my/tasks-show-next . "*Next*")
    (my/tasks-show-waiting . "*Waiting*")
    (my/tasks-show-someday . "*Someday*"))
  "List of views to cycle through.")

(defun my/tasks-view-cycle ()
  "Cycle to the next view in `my/tasks-views-cycle-list'."
  (interactive)
  (let* ((current-name (buffer-name))
         (names (mapcar #'cdr my/tasks-views-cycle-list))
         (pos (cl-position current-name names :test #'string=))
         (next-pos (if pos (mod (1+ pos) (length names)) 0))
         (next-fn (car (nth next-pos my/tasks-views-cycle-list))))
    (funcall next-fn)))

(defvar my/tasks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'my/tasks-view-open-at-point)
    (define-key map (kbd "t") #'my/tasks-view-toggle-today)
    (define-key map (kbd "d") #'my/tasks-view-set-due)
    (define-key map (kbd "s") #'my/tasks-view-set-scheduled)
    (define-key map (kbd "r") #'my/tasks-view-set-reminder)
    (define-key map (kbd "x") #'my/tasks-view-mark-done)
    (define-key map (kbd "A") #'my/tasks-view-archive-at-point)
    (define-key map (kbd "g") #'my/tasks-view-refresh)
    (define-key map (kbd "v") #'my/tasks-view-cycle)
    (define-key map (kbd "i") #'my/tasks-show-inbox)
    (define-key map (kbd "T") #'my/tasks-show-today)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `my/tasks-mode'.")

(define-derived-mode my/tasks-mode special-mode "Tasks"
  "Major mode for interactive task lists."
  (setq-local line-prefix "  ")
  (setq-local cursor-type 'bar))

(defun my/tasks--render-buffer (name query-fn &optional arg)
  "Render tasks in buffer NAME using QUERY-FN and ARG."
  (let* ((tasks (if arg (funcall query-fn arg) (funcall query-fn)))
         (buf (get-buffer-create name)))
    (with-current-buffer buf
      (my/tasks-mode)
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq my/tasks-view-query query-fn)
      (setq my/tasks-view-query-arg arg)
      (insert (format "# %s\n\n" name))
      (if tasks
          (dolist (task tasks)
            (let* ((done (plist-get task :done))
                   (due (plist-get task :due))
                   (scheduled (plist-get task :scheduled))
                   (reminder (plist-get task :reminder))
                   (pos (plist-get task :pos))
                   (meta (concat
                          (when scheduled (format " ⏳ %s" scheduled))
                          (when due (format " 📅 %s" due))
                          (when reminder (format " ⏰ %s" reminder))))
                   (line (format "- [%s] %s%s\n"
                                 (if done "x" " ")
                                 (plist-get task :title)
                                 (or meta "")))
                   (start (point)))
              (insert line)
              (add-text-properties start (1- (point)) (list 'my/task-pos pos))))
        (insert "No tasks.\n"))
      (goto-char (point-min))
      (setq buffer-read-only t))
    (switch-to-buffer buf)))

(defun my/tasks-show-today ()
  (interactive)
  (my/tasks--render-buffer "*Today*" #'my/tasks-today))

(defun my/tasks-show-next ()
  (interactive)
  (my/tasks--render-buffer "*Next*" #'my/tasks-by-status "next"))

(defun my/tasks-show-waiting ()
  (interactive)
  (my/tasks--render-buffer "*Waiting*" #'my/tasks-by-status "waiting"))

(defun my/tasks-show-someday ()
  (interactive)
  (my/tasks--render-buffer "*Someday*" #'my/tasks-by-status "someday"))

(defun my/tasks-show-inbox ()
  (interactive)
  (my/tasks--render-buffer "*Inbox*" #'my/tasks-by-status "inbox"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive View Actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/tasks-view-refresh ()
  "Refresh the current tasks view."
  (interactive)
  (when my/tasks-view-query
    (let ((pt (point))
          (query my/tasks-view-query)
          (arg my/tasks-view-query-arg)
          (name (buffer-name)))
      (my/tasks--render-buffer name query arg)
      (goto-char (min pt (point-max))))))

(defun my/tasks-view--remote-action (fn &optional no-refresh &rest args)
  "Execute FN with ARGS at task position in source file.
If NO-REFRESH is non-nil, do not call my/tasks-view-refresh."
  (let ((pos (or (get-text-property (point) 'my/task-pos)
                 (save-excursion
                   (beginning-of-line)
                   (get-text-property (point) 'my/task-pos))
                 (save-excursion
                   (end-of-line)
                   (get-text-property (1- (point)) 'my/task-pos)))))
    (if (not (and pos (markerp pos) (marker-buffer pos)))
        (message "No task at point.")
      (with-current-buffer (marker-buffer pos)
        (save-excursion
          (goto-char pos)
          (apply fn args)
          (save-buffer)))
      (unless no-refresh
        (my/tasks-view-refresh)))))

(defun my/tasks-view-toggle-today ()
  "Toggle today status from view."
  (interactive)
  (my/tasks-view--remote-action #'my/tasks-toggle-today))

(defun my/tasks-view-mark-done ()
  "Mark task at point as done from view.
Updates visually in current buffer and persists to source file."
  (interactive)
  (let ((pos (or (get-text-property (point) 'my/task-pos)
                 (save-excursion (beginning-of-line) (get-text-property (point) 'my/task-pos)))))
    (if (not pos)
        (message "No task at point.")
      ;; Update in the source file first
      (my/tasks-view--remote-action #'my/tasks-mark-done t)
      ;; Update visually in the view buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (beginning-of-line)
          (let ((start (point)))
            (cond
             ((looking-at "\\([[:space:]]*- \\[\\) \\(\\]\\)")
              (replace-match "\\1x\\2"))
             ((looking-at "\\([[:space:]]*- \\[\\)[xX]\\(\\]\\)")
              (replace-match "\\1 \\2")))
            ;; Re-apply the property to the whole line to keep it interactive
            (add-text-properties start (line-end-position) (list 'my/task-pos pos))))))))

(defun my/tasks-view-archive-at-point ()
  "Archive task at point from view."
  (interactive)
  (my/tasks-view--remote-action #'my/tasks-archive-at-point))

(defun my/tasks-view-set-due (date)
  "Set due date from view."
  (interactive (list (my/tasks--read-date nil "Due: ")))
  (my/tasks-view--remote-action #'my/tasks-set-due nil date))

(defun my/tasks-view-set-scheduled (date)
  "Set scheduled date from view."
  (interactive (list (my/tasks--read-date nil "Scheduled: ")))
  (my/tasks-view--remote-action #'my/tasks-set-scheduled nil date))

(defun my/tasks-view-set-reminder (date)
  "Set reminder from view."
  (interactive (list (my/tasks--read-date t "Reminder: ")))
  (my/tasks-view--remote-action #'my/tasks-set-reminder nil date))

(defun my/tasks-view-open-at-point ()
  "Follow wiki-link in task or jump to source."
  (interactive)
  (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (pos (get-text-property (point) 'my/task-pos)))
    (cond
     ((string-match "\\[\\[\\(.*?\\)\\]\\]" line)
      (let* ((target (match-string 1 line))
             ;; Obsidian usually resolves links relative to vault root
             (file (expand-file-name (concat target ".md") obsidian-directory)))
        (if (file-exists-p file)
            (find-file file)
          ;; Try searching in subdirectories if not found at root
          (let ((matches (directory-files-recursively obsidian-directory (regexp-quote (concat target ".md")))))
            (if matches
                (find-file (car matches))
              (message "Could not find file: %s" target))))))
     (pos (find-file (my/tasks-path))
          (goto-char pos))
     (t (message "Nothing to open.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Date Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/tasks--update-meta (emoji date)
  "Update or add EMOJI with DATE in current buffer at point."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "^[[:space:]]*- \\[\\([ xX]\\)\\] \\(.*?\\)\\(?: \\([📅⏳⏰].*\\)\\)?$" nil t)
        (let* ((done (match-string 1))
               (title (match-string 2))
               (meta (or (match-string 3) ""))
               (new-meta (save-match-data
                           (if (string-match (concat emoji " [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(?: [0-9]\\{2\\}:[0-9]\\{2\\}\\)?") meta)
                               (replace-regexp-in-string
                                (concat emoji " [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(?: [0-9]\\{2\\}:[0-9]\\{2\\}\\)?")
                                (format "%s %s" emoji date)
                                meta t t)
                             (concat meta (if (string-empty-p meta) "" " ") emoji " " date)))))
          (replace-match (format "- [%s] %s %s" done title (string-trim new-meta)) t t))
      (message "Could not find task line at point."))))

(defun my/tasks-set-due (date)
  "Set due date 📅 for current task."
  (interactive (list (my/tasks--read-date nil "Due: ")))
  (my/tasks--update-meta "📅" date))

(defun my/tasks-set-scheduled (date)
  "Set scheduled date ⏳ for current task."
  (interactive (list (my/tasks--read-date nil "Scheduled: ")))
  (my/tasks--update-meta "⏳" date))

(defun my/tasks-set-reminder (date)
  "Set reminder ⏰ for current task."
  (interactive (list (my/tasks--read-date t "Reminder: ")))
  (my/tasks--update-meta "⏰" date))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mark task for today
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/tasks-toggle-today ()
  "Toggle status:: today on current task."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "^- \\[")
      (re-search-backward "^- \\[" nil t))
    (let ((task-start (point)))
      ;; Find end of task block
      (forward-line 1)
      (while (looking-at "^[[:space:]]+")
        (forward-line 1))
      (let ((task-end (point)))
        (goto-char task-start)
        (if (re-search-forward "^[[:space:]]+status:: today" task-end t)
            (replace-match "  status:: next")
          (if (re-search-forward "^[[:space:]]+status:: \\([a-z]+\\)" task-end t)
              (replace-match "  status:: today")
            ;; Not found, add it after the task line
            (goto-char task-start)
            (forward-line 1)
            (insert "  status:: today\n")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Archive & Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/tasks-mark-done ()
  "Mark the task at point as done in the current buffer."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at "\\([[:space:]]*- \\[\\) \\(\\]\\)")
      (replace-match "\\1x\\2"))
     ((looking-at "\\([[:space:]]*- \\[\\)[xX]\\(\\]\\)")
      (replace-match "\\1 \\2"))
     (t (message "No checkbox found on this line.")))))

(defun my/tasks-archive-at-point ()
  "Archive the task at point (including properties and a timestamp)."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^- \\[\\([ x]\\)\\]")
      (let ((start (point)))
        (forward-line 1)
        (while (looking-at "^[[:space:]]+")
          (forward-line 1))
        (let* ((end (point))
               (content (buffer-substring start end))
               (today (my/tasks--today-string)))
          (with-temp-buffer
            (insert content)
            ;; Add archived-at property if it doesn't exist
            (goto-char (point-min))
            (forward-line 1)
            (insert (format "  archived-at:: %s\n" today))
            (append-to-file (point-min) (point-max) (my/archive-path)))
          (delete-region start end)
          (message "Task archived with timestamp."))))))

(defun my/archive-done ()
  "Move all completed tasks and their properties to archive.md."
  (interactive)
  (let ((count 0))
    (with-current-buffer (find-file-noselect (my/tasks-path))
      (goto-char (point-min))
      (while (re-search-forward "^- \\[x\\]" nil t)
        (goto-char (match-beginning 0))
        (my/tasks-archive-at-point)
        (setq count (1+ count)))
      (save-buffer))
    (message "Archived %d tasks." count)
    (when (derived-mode-p 'my/tasks-mode)
      (my/tasks-view-refresh))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Suggested keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c t c") #'my/tasks-capture)
(global-set-key (kbd "C-c t t") #'my/tasks-show-today)
(global-set-key (kbd "C-c t n") #'my/tasks-show-next)
(global-set-key (kbd "C-c t i") #'my/tasks-show-inbox)
(global-set-key (kbd "C-c t s") #'my/tasks-show-someday)
(global-set-key (kbd "C-c t w") #'my/tasks-show-waiting)
(global-set-key (kbd "C-c t a") #'my/archive-done)
(global-set-key (kbd "C-c t o") #'my/tasks-toggle-today)
(global-set-key (kbd "C-c t x") #'my/tasks-mark-done)
(global-set-key (kbd "C-c t d") #'my/tasks-set-due)
(global-set-key (kbd "C-c t S") #'my/tasks-set-scheduled)
(global-set-key (kbd "C-c t r") #'my/tasks-set-reminder)

(provide 'tasks)
