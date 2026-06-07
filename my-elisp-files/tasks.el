;;; tasks.el --- One-file-per-task GTD with YAML frontmatter -*- lexical-binding: t; -*-

(require 'org)
(require 'subr-x)
(require 'seq)
(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup tasks nil
  "One-file-per-task GTD with YAML frontmatter."
  :group 'applications)

(defcustom my/tasks-directory
  (concat obsidian-directory "/tasks")
  "Directory containing active task files (one task per file)."
  :type 'directory)

(defcustom my/tasks-archive-directory
  (concat obsidian-directory "/tasks/archive")
  "Directory where completed tasks are moved."
  :type 'directory)

(defcustom my/tasks-statuses
  '("inbox" "next" "today" "waiting" "someday")
  "Allowed status values (excluding `done', which is set on archive)."
  :type '(repeat string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/tasks--today-string ()
  (format-time-string "%Y-%m-%d"))

(defun my/tasks--now-string ()
  (format-time-string "%Y-%m-%d %H:%M"))

(defun my/tasks--read-date (&optional with-time prompt)
  "Read a date via org's calendar interface."
  (format-time-string
   (if with-time "%Y-%m-%d %H:%M" "%Y-%m-%d")
   (org-read-date with-time t nil prompt)))

(defun my/tasks--slugify (title)
  "Make a filesystem-friendly slug from TITLE."
  (let ((s (downcase (or title ""))))
    (setq s (replace-regexp-in-string "ä" "ae" s))
    (setq s (replace-regexp-in-string "ö" "oe" s))
    (setq s (replace-regexp-in-string "ü" "ue" s))
    (setq s (replace-regexp-in-string "ß" "ss" s))
    (setq s (replace-regexp-in-string "[^a-z0-9]+" "-" s))
    (setq s (replace-regexp-in-string "^-+\\|-+$" "" s))
    (if (string-empty-p s) "task" s)))

(defun my/tasks--unique-path (dir slug &optional prefix)
  "Return a unique .md path in DIR for SLUG, with optional filename PREFIX."
  (let* ((prefix (or prefix ""))
         (path (expand-file-name (concat prefix slug ".md") dir))
         (i 2))
    (while (file-exists-p path)
      (setq path (expand-file-name (format "%s%s-%d.md" prefix slug i) dir))
      (setq i (1+ i)))
    path))

(defun my/tasks--ensure-dir (dir)
  (unless (file-directory-p dir) (make-directory dir t)))

(defun my/tasks--yaml-quote (value)
  "Quote VALUE for YAML if it starts with a reserved character."
  (cond
   ((null value) nil)
   ((string-match-p "\\`[\"'\\[{>|*&!%@`]" value)
    (format "\"%s\"" (replace-regexp-in-string "\"" "\\\\\"" value)))
   (t value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frontmatter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/tasks--parse-frontmatter (file)
  "Parse YAML frontmatter and H1 title from FILE. Return a plist."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (props title)
      (when (looking-at "^---\n")
        (forward-line 1)
        (while (and (not (looking-at "^---")) (not (eobp)))
          (when (looking-at "^\\([a-z][a-z0-9_-]*\\): *\\(.*?\\) *$")
            (let ((key (intern (concat ":" (match-string 1))))
                  (val (match-string 2)))
              (when (string-match "\\`\"\\(.*\\)\"\\'" val)
                (setq val (replace-regexp-in-string "\\\\\"" "\"" (match-string 1 val))))
              (setq props (plist-put props key val))))
          (forward-line 1))
        (when (looking-at "^---") (forward-line 1)))
      (when (re-search-forward "^# \\(.*\\)$" nil t)
        (setq title (string-trim (match-string 1))))
      (plist-put props :title (or title (file-name-base file))))))

(defun my/tasks--update-property (file key value)
  "Set KEY to VALUE in FILE's YAML frontmatter. Add if missing, remove if empty."
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (unless (looking-at "^---\n")
          (error "No frontmatter in %s" file))
        (forward-line 1)
        (let* ((closing (save-excursion
                          (when (re-search-forward "^---$" nil t)
                            (line-beginning-position))))
               (found (re-search-forward
                       (format "^%s: *.*$" (regexp-quote key)) closing t)))
          (cond
           ((and (or (null value) (string-empty-p value)) found)
            (delete-region (line-beginning-position) (1+ (line-end-position))))
           (found
            (replace-match (format "%s: %s" key (my/tasks--yaml-quote value)) t t))
           ((and value (not (string-empty-p value)))
            (goto-char closing)
            (insert (format "%s: %s\n" key (my/tasks--yaml-quote value)))))))
      (save-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read all tasks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/read-tasks (&optional dir)
  "Return tasks in DIR (default `my/tasks-directory') as plists with :file."
  (let ((dir (or dir my/tasks-directory)))
    (my/tasks--ensure-dir dir)
    (let ((files (seq-filter
                  (lambda (f)
                    (and (not (file-directory-p f))
                         (not (string-prefix-p "." (file-name-nondirectory f)))))
                  (directory-files dir t "\\.md\\'"))))
      (delq nil
            (mapcar (lambda (f)
                      (let ((task (my/tasks--parse-frontmatter f)))
                        (when (plist-get task :status)
                          (plist-put task :file f))))
                    files)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/tasks-by-status (status)
  (seq-filter (lambda (task) (string= status (plist-get task :status)))
              (my/read-tasks)))

(defun my/tasks-archived ()
  "Return all tasks in the archive directory."
  (my/read-tasks my/tasks-archive-directory))

(defun my/tasks-today ()
  (my/tasks-by-status "today"))

(defun my/tasks-due-today ()
  (let ((today (my/tasks--today-string)))
    (seq-filter (lambda (task) (equal today (plist-get task :due)))
                (my/read-tasks))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/tasks-capture (title)
  "Create a new task file with TITLE in `my/tasks-directory'."
  (interactive "sTask: ")
  (my/tasks--ensure-dir my/tasks-directory)
  (let* ((slug (my/tasks--slugify title))
         (path (my/tasks--unique-path my/tasks-directory slug)))
    (with-temp-buffer
      (insert "---\n")
      (insert "status: inbox\n")
      (insert (format "created: %s\n" (my/tasks--now-string)))
      (insert "---\n\n")
      (insert (format "# %s\n\n" title))
      (write-region (point-min) (point-max) path))
    (message "Captured: %s" (file-name-nondirectory path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local my/tasks-view-query nil
  "Query function used for the current view.")

(defvar-local my/tasks-view-query-arg nil
  "Argument passed to the query function.")

(defvar-local my/tasks-pending-changes nil
  "Alist of (FILE . NEW-STATUS) status changes queued in this view.
Applied (committed to disk) on the next refresh or reopen.")

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

(defvar my/tasks-mode-map (make-sparse-keymap)
  "Keymap for `my/tasks-mode'.")

;; Rebind on every load so re-evaluating this file picks up new bindings
;; (defvar above doesn't re-init an already-bound keymap).
(let ((map my/tasks-mode-map))
  (setcdr map nil)
  (define-key map (kbd "RET") #'my/tasks-view-open-at-point)
  (define-key map (kbd "t") #'my/tasks-toggle-today)
  (define-key map (kbd "d") #'my/tasks-set-due)
  (define-key map (kbd "s") #'my/tasks-set-scheduled)
  (define-key map (kbd "r") #'my/tasks-set-reminder)
  (define-key map (kbd "p") #'my/tasks-set-status)
  (define-key map (kbd "x") #'my/tasks-mark-done)
  (define-key map (kbd "u") #'my/tasks-unarchive)
  (define-key map (kbd "m") #'my/tasks-open-mail)
  (define-key map (kbd "g") #'my/tasks-view-refresh)
  (define-key map (kbd "v") #'my/tasks-view-cycle)
  (define-key map (kbd "i") #'my/tasks-show-inbox)
  (define-key map (kbd "T") #'my/tasks-show-today)
  (define-key map (kbd "A") #'my/tasks-show-archive)
  (define-key map (kbd "q") #'quit-window))

(define-derived-mode my/tasks-mode special-mode "Tasks"
  "Major mode for interactive task lists."
  (setq-local line-prefix "  ")
  (setq-local cursor-type 'bar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface my/tasks-header-face
  '((t :inherit outline-1 :weight bold :height 1.4))
  "Face for the view header.")

(defface my/tasks-rule-face
  '((t :inherit shadow))
  "Face for the rule under the header.")

(defface my/tasks-title-face
  '((t :weight semi-bold))
  "Face for task titles.")

(defface my/tasks-bullet-face
  '((t :inherit outline-1 :weight normal))
  "Face for the leading bullet character.
Inherits the header colour so the bullet matches `my/tasks-header-face'.")

(defface my/tasks-pending-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for the pending-status annotation in views (e.g. `→ next').")

(defface my/tasks-date-face
  '((t :inherit shadow))
  "Face for dates.")

(defface my/tasks-overdue-face
  '((t :inherit error :weight bold))
  "Face for overdue due/scheduled dates.")

(defface my/tasks-due-today-face
  '((t :inherit warning :weight bold))
  "Face for due/scheduled dates that fall on today.")

(defface my/tasks-project-face
  '((t :inherit font-lock-string-face :slant italic))
  "Face for the project link.")

(defun my/tasks--date-face (date)
  "Pick the right face for DATE (YYYY-MM-DD[ HH:MM])."
  (when date
    (let* ((today (my/tasks--today-string))
           (d (substring date 0 (min 10 (length date)))))
      (cond
       ((string< d today) 'my/tasks-overdue-face)
       ((string= d today) 'my/tasks-due-today-face)
       (t 'my/tasks-date-face)))))

(defun my/tasks--archive-path-p (file)
  "Return non-nil if FILE lives inside `my/tasks-archive-directory'."
  (string-prefix-p
   (file-name-as-directory (expand-file-name my/tasks-archive-directory))
   (expand-file-name file)))

(defun my/tasks--commit-pending-changes (pending)
  "Apply each (FILE . NEW-STATUS) pair in PENDING to disk."
  (dolist (change pending)
    (let ((file (car change))
          (new-status (cdr change)))
      (when (file-exists-p file)
        (cond
         ;; Active → archive
         ((and (string= new-status "done")
               (not (my/tasks--archive-path-p file)))
          (my/tasks--archive-file file))
         ;; Archive → active
         ((and (not (string= new-status "done"))
               (my/tasks--archive-path-p file))
          (my/tasks--unarchive-file file new-status))
         ;; In-place status update
         (t
          (my/tasks--update-property file "status" new-status)))))))

(defun my/tasks--draw-view-content (display-title tasks query-fn arg)
  "Insert view content with TASKS into current buffer.
Uses the buffer-local `my/tasks-pending-changes' for annotations."
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq my/tasks-view-query query-fn)
  (setq my/tasks-view-query-arg arg)
  (insert (propertize display-title 'face 'my/tasks-header-face))
  (insert "\n")
  (insert (propertize (make-string (max 8 (length display-title)) ?─)
                      'face 'my/tasks-rule-face))
  (insert "\n\n")
  (if tasks
      (dolist (task tasks)
        (let* ((title (plist-get task :title))
               (due (plist-get task :due))
               (scheduled (plist-get task :scheduled))
               (reminder (plist-get task :reminder))
               (project (plist-get task :project))
               (file (plist-get task :file))
               (pending (cdr (assoc file my/tasks-pending-changes)))
               (start (point)))
          (insert (propertize "▸ " 'face 'my/tasks-bullet-face))
          (insert (propertize title 'face 'my/tasks-title-face))
          (when scheduled
            (insert (propertize (format "  ⏳ %s" scheduled)
                                'face (my/tasks--date-face scheduled))))
          (when due
            (insert (propertize (format "  📅 %s" due)
                                'face (my/tasks--date-face due))))
          (when reminder
            (insert (propertize (format "  ⏰ %s" reminder)
                                'face 'my/tasks-date-face)))
          (when project
            (insert (propertize (format "  %s" project)
                                'face 'my/tasks-project-face)))
          (when pending
            (insert (propertize (format "  → %s" pending)
                                'face 'my/tasks-pending-face)))
          (insert "\n")
          (add-text-properties start (1- (point)) (list 'my/task-file file))))
    (insert (propertize "Keine Tasks." 'face 'shadow))
    (insert "\n"))
  (goto-char (or (next-single-property-change (point-min) 'my/task-file)
                 (point-min)))
  (setq buffer-read-only t))

(defun my/tasks--render-buffer (name query-fn &optional arg)
  "Render NAME view via QUERY-FN ARG, committing any pending changes first."
  (let* ((existing (get-buffer name))
         (pending (when existing
                    (buffer-local-value 'my/tasks-pending-changes existing))))
    (my/tasks--commit-pending-changes pending))
  (let* ((tasks (if arg (funcall query-fn arg) (funcall query-fn)))
         (buf (get-buffer-create name))
         (display-title (replace-regexp-in-string "\\*" "" name)))
    (with-current-buffer buf
      (my/tasks-mode)
      (setq my/tasks-pending-changes nil)
      (my/tasks--draw-view-content display-title tasks query-fn arg))
    (switch-to-buffer buf)))

(defun my/tasks--redraw-view ()
  "Redraw the current view (uncommitted pending changes are kept and shown)."
  (when my/tasks-view-query
    (let* ((pt (point))
           (display-title (replace-regexp-in-string "\\*" "" (buffer-name)))
           (tasks (if my/tasks-view-query-arg
                      (funcall my/tasks-view-query my/tasks-view-query-arg)
                    (funcall my/tasks-view-query))))
      (my/tasks--draw-view-content display-title tasks
                                   my/tasks-view-query my/tasks-view-query-arg)
      (goto-char (min pt (point-max))))))

(defun my/tasks--queue-status-change (file new-status)
  "Queue a status change for FILE → NEW-STATUS in the current view buffer."
  (let ((change (assoc file my/tasks-pending-changes)))
    (if change
        (setcdr change new-status)
      (push (cons file new-status) my/tasks-pending-changes)))
  (my/tasks--redraw-view))

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

(defun my/tasks-show-archive ()
  "Show archived tasks."
  (interactive)
  (my/tasks--render-buffer "*Archive*" #'my/tasks-archived))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View Actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/tasks-view-refresh ()
  "Refresh the current tasks view (commits pending changes first)."
  (interactive)
  (when my/tasks-view-query
    (let ((pt (point)))
      (my/tasks--render-buffer (buffer-name)
                               my/tasks-view-query
                               my/tasks-view-query-arg)
      (goto-char (min pt (point-max))))))

(defun my/tasks--file-at-point ()
  "Return the task file referenced by the line at point."
  (or (get-text-property (point) 'my/task-file)
      (save-excursion (beginning-of-line)
                      (get-text-property (point) 'my/task-file))
      (save-excursion (end-of-line)
                      (when (> (point) (point-min))
                        (get-text-property (1- (point)) 'my/task-file)))))

(defun my/tasks--current-task-file ()
  "Return file of task at point (view) or current buffer file if it's a task.
Excludes files inside the archive directory — operate on those via `my/tasks-unarchive'."
  (or (my/tasks--file-at-point)
      (when (and buffer-file-name
                 (string-prefix-p
                  (file-name-as-directory (expand-file-name my/tasks-directory))
                  (expand-file-name buffer-file-name))
                 (not (my/tasks--archive-path-p buffer-file-name)))
        buffer-file-name)))

(defun my/tasks-view-open-at-point ()
  "Open the task file at point."
  (interactive)
  (let ((file (my/tasks--file-at-point)))
    (if file (find-file file) (message "No task at point."))))

(defun my/tasks--apply-and-redraw (fn &rest args)
  "Apply FN with (file . ARGS) to task at point, then redraw the view.
Redraws (not refreshes) — keeps pending status changes intact."
  (let ((file (my/tasks--current-task-file)))
    (unless file (user-error "No task at point"))
    (apply fn file args)
    (when (derived-mode-p 'my/tasks-mode)
      (my/tasks--redraw-view))))

(defun my/tasks--set-or-queue-status (file new-status)
  "In a view, queue NEW-STATUS for FILE; otherwise apply immediately.
\"done\" archives the file when applied immediately."
  (cond
   ((derived-mode-p 'my/tasks-mode)
    (my/tasks--queue-status-change file new-status))
   ((string= new-status "done")
    (my/tasks--archive-file file))
   (t
    (my/tasks--update-property file "status" new-status))))

(defun my/tasks-set-due (date)
  "Set due date for task at point."
  (interactive (list (my/tasks--read-date nil "Due: ")))
  (my/tasks--apply-and-redraw #'my/tasks--update-property "due" date))

(defun my/tasks-set-scheduled (date)
  "Set scheduled date for task at point."
  (interactive (list (my/tasks--read-date nil "Scheduled: ")))
  (my/tasks--apply-and-redraw #'my/tasks--update-property "scheduled" date))

(defun my/tasks-set-reminder (date)
  "Set reminder for task at point."
  (interactive (list (my/tasks--read-date t "Reminder: ")))
  (my/tasks--apply-and-redraw #'my/tasks--update-property "reminder" date))

(defun my/tasks-set-status (status)
  "Set status for task at point.
In a view buffer the change is queued and applied on next refresh/reopen."
  (interactive (list (completing-read "Status: " my/tasks-statuses nil t)))
  (let ((file (my/tasks--current-task-file)))
    (unless file (user-error "No task at point"))
    (my/tasks--set-or-queue-status file status)))

(defun my/tasks-toggle-today ()
  "Toggle status:today on the task at point.
In a view buffer the change is queued and applied on next refresh/reopen."
  (interactive)
  (let ((file (my/tasks--current-task-file)))
    (unless file (user-error "No task at point"))
    (let* ((current (or (cdr (assoc file my/tasks-pending-changes))
                        (plist-get (my/tasks--parse-frontmatter file) :status)))
           (new (if (string= current "today") "next" "today")))
      (my/tasks--set-or-queue-status file new))))

(defun my/tasks-mark-done ()
  "Mark task at point as done and move it to the archive directory.
In a view buffer the change is queued and applied on next refresh/reopen."
  (interactive)
  (let ((file (my/tasks--current-task-file)))
    (unless file (user-error "No task at point"))
    (my/tasks--set-or-queue-status file "done")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Archive
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/tasks--archive-file (file)
  "Set status=done, write archived-at, move FILE to archive directory."
  (my/tasks--ensure-dir my/tasks-archive-directory)
  (my/tasks--update-property file "status" "done")
  (my/tasks--update-property file "archived-at" (my/tasks--now-string))
  (let* ((today (my/tasks--today-string))
         (base (file-name-base file))
         (target (my/tasks--unique-path
                  my/tasks-archive-directory base (concat today "-")))
         (buf (get-file-buffer file)))
    (when buf
      (with-current-buffer buf (save-buffer))
      (kill-buffer buf))
    (rename-file file target)
    (message "Archived: %s" (file-name-nondirectory target))))

(defun my/tasks--current-archived-file ()
  "Return current buffer's file path if it lives inside the archive directory."
  (when (and buffer-file-name
             (string-prefix-p
              (file-name-as-directory
               (expand-file-name my/tasks-archive-directory))
              (expand-file-name buffer-file-name)))
    buffer-file-name))

(defun my/tasks--unarchive-file (file new-status)
  "Restore archived FILE to active tasks with NEW-STATUS. Returns target path."
  (my/tasks--ensure-dir my/tasks-directory)
  (my/tasks--update-property file "archived-at" nil)
  (my/tasks--update-property file "status" new-status)
  (let* ((base (file-name-nondirectory file))
         (stripped (replace-regexp-in-string
                    "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-" "" base))
         (slug (file-name-base stripped))
         (target (my/tasks--unique-path my/tasks-directory slug))
         (buf (get-file-buffer file)))
    (when buf
      (with-current-buffer buf (save-buffer))
      (kill-buffer buf))
    (rename-file file target)
    target))

(defun my/tasks-unarchive (status)
  "Move the archived task at point back into the active tasks directory.
STATUS is the new status (defaults to `inbox')."
  (interactive
   (list (completing-read "New status: " my/tasks-statuses nil t nil nil "inbox")))
  (let ((file (or (my/tasks--current-archived-file)
                  (my/tasks--file-at-point)
                  (read-file-name
                   "Archived task: "
                   (file-name-as-directory
                    (expand-file-name my/tasks-archive-directory))
                   nil t))))
    (unless (and file (file-exists-p file))
      (user-error "File not found"))
    (unless (my/tasks--archive-path-p file)
      (user-error "Not an archived file: %s" file))
    (let ((target (my/tasks--unarchive-file file status)))
      (message "Un-archived: %s" (file-name-nondirectory target))
      (when (derived-mode-p 'my/tasks-mode)
        (my/tasks-view-refresh)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mu4e integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare-function mu4e-message-at-point "mu4e" ())
(declare-function mu4e-message-field "mu4e" (msg field))
(declare-function mu4e-view-message-with-message-id "mu4e-view" (msgid))
(declare-function mu4e-view-message-with-msgid "mu4e" (msgid))

(defun my/tasks--mu4e-from-email (from)
  "Extract an email address from FROM (mu4e's :from field, multiple versions).
Falls back to the name if no email is present."
  (cond
   ((stringp from) from)
   ((and (consp from) (consp (car from)))
    (let ((first (car from)))
      (cond
       ;; new mu4e: plist (:name "..." :email "...")
       ((keywordp (car first))
        (or (plist-get first :email)
            (plist-get first :name)
            ""))
       ;; old mu4e: cons (name . email)
       ((stringp (cdr first)) (cdr first))
       ((stringp (car first)) (car first))
       (t ""))))
   (t "")))

(defun my/tasks--any-task-file ()
  "Return task file at point or in buffer, including archived files."
  (or (my/tasks--current-task-file)
      (my/tasks--current-archived-file)))

(defun my/tasks-capture-from-mu4e ()
  "Capture a task linked to the current mu4e message.
Saves the Message-ID in frontmatter so `my/tasks-open-mail' can jump back."
  (interactive)
  (let ((msg (and (fboundp 'mu4e-message-at-point)
                  (mu4e-message-at-point))))
    (unless msg (user-error "Not in a mu4e buffer"))
    (let* ((msgid (mu4e-message-field msg :message-id))
           (subject (or (mu4e-message-field msg :subject) ""))
           (from-email (my/tasks--mu4e-from-email
                        (mu4e-message-field msg :from)))
           (title (read-string "Task title: "))
           (slug (my/tasks--slugify title)))
      (my/tasks--ensure-dir my/tasks-directory)
      (let ((path (my/tasks--unique-path my/tasks-directory slug)))
        (with-temp-buffer
          (insert "---\n")
          (insert "status: inbox\n")
          (insert (format "created: %s\n" (my/tasks--now-string)))
          (insert (format "mu4e-msgid: %s\n" (my/tasks--yaml-quote msgid)))
          (insert "---\n\n")
          (insert (format "# %s\n\n" title))
          (insert "- aus E-Mail\n")
          (unless (string-empty-p from-email)
            (insert (format "  - von: %s\n" from-email)))
          (insert (format "  - Betreff: %s\n" subject))
          (write-region (point-min) (point-max) path))
        (message "Captured: %s" (file-name-nondirectory path))))))

(defun my/tasks-open-mail ()
  "Open the mu4e message linked to the task at point or in current buffer.
Supports both new (`mu4e-view-message-with-message-id') and old
\(`mu4e-view-message-with-msgid') mu4e function names."
  (interactive)
  (let* ((file (my/tasks--any-task-file))
         (msgid (when file
                  (plist-get (my/tasks--parse-frontmatter file)
                             :mu4e-msgid))))
    (cond
     ((not file) (user-error "No task at point"))
     ((not msgid) (user-error "No `mu4e-msgid' in this task"))
     (t
      (require 'mu4e)
      (cond
       ((fboundp 'mu4e-view-message-with-message-id)
        (mu4e-view-message-with-message-id msgid))
       ((fboundp 'mu4e-view-message-with-msgid)
        (mu4e-view-message-with-msgid msgid))
       (t (user-error "No mu4e function found to view messages by ID")))))))

(with-eval-after-load 'mu4e
  (when (boundp 'mu4e-view-mode-map)
    (define-key mu4e-view-mode-map (kbd "C-c t c") #'my/tasks-capture-from-mu4e))
  (when (boundp 'mu4e-headers-mode-map)
    (define-key mu4e-headers-mode-map (kbd "C-c t c") #'my/tasks-capture-from-mu4e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c t c") #'my/tasks-capture)
(global-set-key (kbd "C-c t t") #'my/tasks-show-today)
(global-set-key (kbd "C-c t n") #'my/tasks-show-next)
(global-set-key (kbd "C-c t i") #'my/tasks-show-inbox)
(global-set-key (kbd "C-c t s") #'my/tasks-show-someday)
(global-set-key (kbd "C-c t w") #'my/tasks-show-waiting)
(global-set-key (kbd "C-c t o") #'my/tasks-toggle-today)
(global-set-key (kbd "C-c t x") #'my/tasks-mark-done)
(global-set-key (kbd "C-c t u") #'my/tasks-unarchive)
(global-set-key (kbd "C-c t A") #'my/tasks-show-archive)
(global-set-key (kbd "C-c t d") #'my/tasks-set-due)
(global-set-key (kbd "C-c t S") #'my/tasks-set-scheduled)
(global-set-key (kbd "C-c t r") #'my/tasks-set-reminder)
(global-set-key (kbd "C-c t p") #'my/tasks-set-status)
(global-set-key (kbd "C-c t m") #'my/tasks-open-mail)

(provide 'tasks)
;;; tasks.el ends here
