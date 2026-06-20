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

(defcustom my/tasks-contexts
  '("@home" "@work" "@phone" "@computer" "@errands" "@reading")
  "Allowed GTD contexts. Stored on a task as the YAML list `contexts:'."
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

(defconst my/tasks--scalar-property-keys
  '(:status :due :scheduled :reminder :project :created :archived-at
    :mu4e-msgid)
  "Frontmatter keys treated as scalar strings even when YAML stores a list.
Obsidian's Properties UI can write a logically-scalar field as a
one-element block list; this normalises that back to a string.")

(defun my/tasks--coerce-scalar-properties (props)
  "Collapse single-item list values of `my/tasks--scalar-property-keys'
in PROPS down to their first item. Returns the (possibly updated) plist."
  (dolist (key my/tasks--scalar-property-keys)
    (let ((val (plist-get props key)))
      (when (consp val)
        (setq props (plist-put props key (car val))))))
  props)

(defun my/tasks--unquote-yaml (s)
  "Strip surrounding single or double quotes from a YAML scalar S."
  (let ((s (string-trim (or s ""))))
    (cond
     ((and (>= (length s) 2)
           (string-prefix-p "\"" s) (string-suffix-p "\"" s))
      (replace-regexp-in-string "\\\\\"" "\""
                                (substring s 1 (1- (length s)))))
     ((and (>= (length s) 2)
           (string-prefix-p "'" s) (string-suffix-p "'" s))
      (substring s 1 (1- (length s))))
     (t s))))

(defun my/tasks--parse-flow-list (raw)
  "Parse YAML inline list \"[a, b, c]\" from RAW.
Return the list of unquoted items, or nil if RAW is not a flow list.
\"[]\" returns nil (empty list)."
  (when (string-match "\\`\\[\\(.*\\)\\]\\'" raw)
    (let ((inner (string-trim (match-string 1 raw))))
      (unless (string-empty-p inner)
        (mapcar #'my/tasks--unquote-yaml
                (split-string inner "," t " *"))))))

(defun my/tasks--parse-frontmatter (file)
  "Parse YAML frontmatter and H1 title from FILE. Return a plist.
Scalar properties become strings; YAML arrays (both inline \"[a, b]\"
and block style \"  - a\\n  - b\") become Lisp lists of strings."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (props title)
      (when (looking-at "^---\n")
        (forward-line 1)
        (while (and (not (looking-at "^---")) (not (eobp)))
          (if (looking-at "^\\([a-z][a-z0-9_-]*\\): *\\(.*?\\) *$")
              (let ((key (intern (concat ":" (match-string 1))))
                    (raw (match-string 2)))
                (forward-line 1)
                (cond
                 ;; Empty value → look for block-style list under it
                 ((string-empty-p raw)
                  (let (items)
                    (while (looking-at "^[[:space:]]+- *\\(.*?\\) *$")
                      (push (my/tasks--unquote-yaml (match-string 1)) items)
                      (forward-line 1))
                    (setq props (plist-put props key
                                           (if items (nreverse items) "")))))
                 ;; Inline flow list "[a, b]"
                 ((string-prefix-p "[" raw)
                  (setq props (plist-put props key
                                         (my/tasks--parse-flow-list raw))))
                 ;; Scalar
                 (t
                  (setq props (plist-put props key
                                         (my/tasks--unquote-yaml raw))))))
            (forward-line 1)))
        (when (looking-at "^---") (forward-line 1)))
      (when (re-search-forward "^# \\(.*\\)$" nil t)
        (setq title (string-trim (match-string 1))))
      (setq props (plist-put props :title
                             (or title (file-name-base file))))
      (my/tasks--coerce-scalar-properties props))))

(defun my/tasks--insert-list-block (key values)
  "Insert KEY: as a YAML block list of VALUES at point."
  (insert (format "%s:\n" key))
  (dolist (v values)
    (insert (format "  - %s\n" (my/tasks--yaml-quote v)))))

(defun my/tasks--update-list-property (file key values)
  "Replace KEY in FILE's frontmatter with a YAML block list of VALUES.
Empty/nil VALUES removes the property entirely. Replaces an existing
scalar or block-list value."
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
          (if found
              (let ((line-start (line-beginning-position))
                    (delete-end (1+ (line-end-position))))
                (save-excursion
                  (forward-line 1)
                  (while (and (< (point) (or closing (point-max)))
                              (looking-at "^[[:space:]]+- "))
                    (forward-line 1)
                    (setq delete-end (point))))
                (delete-region line-start delete-end)
                (when values
                  (goto-char line-start)
                  (my/tasks--insert-list-block key values)))
            (when values
              (goto-char closing)
              (my/tasks--insert-list-block key values)))))
      (save-buffer))))

(defun my/tasks--update-property (file key value)
  "Set KEY to VALUE in FILE's YAML frontmatter. Add if missing, remove if empty.
When the key currently holds a block-style list (indented `- item' lines),
those lines are removed/replaced along with the matched header line."
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
          (if found
              (let ((line-start (line-beginning-position))
                    (delete-end (1+ (line-end-position))))
                ;; Extend deletion across following block-list items
                (save-excursion
                  (forward-line 1)
                  (while (and (< (point) (or closing (point-max)))
                              (looking-at "^[[:space:]]+- "))
                    (forward-line 1)
                    (setq delete-end (point))))
                (delete-region line-start delete-end)
                (when (and value (not (string-empty-p value)))
                  (goto-char line-start)
                  (insert (format "%s: %s\n" key (my/tasks--yaml-quote value)))))
            (when (and value (not (string-empty-p value)))
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

(defun my/tasks-by-context (ctx)
  "Return all active tasks that include CTX in their `contexts'."
  (seq-filter (lambda (task)
                (let ((cs (plist-get task :contexts)))
                  (and (listp cs) (member ctx cs))))
              (my/read-tasks)))

(defun my/tasks--file-contains-p (file query)
  "Return non-nil if FILE's contents contain QUERY (case-insensitive)."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((case-fold-search t))
      (goto-char (point-min))
      (search-forward query nil t))))

(defun my/tasks-search-results (query)
  "Return all tasks (active + archived) whose contents contain QUERY.
Search is case-insensitive substring over the whole markdown file —
frontmatter, title and body."
  (when (and query (not (string-empty-p (string-trim query))))
    (let ((all (append (my/read-tasks my/tasks-directory)
                       (when (file-directory-p my/tasks-archive-directory)
                         (my/read-tasks my/tasks-archive-directory))))
          results)
      (dolist (task all)
        (let ((file (plist-get task :file)))
          (when (and file (my/tasks--file-contains-p file query))
            (push task results))))
      (nreverse results))))

(defun my/tasks-today ()
  (my/tasks-by-status "today"))

(defun my/tasks-due-today ()
  (let ((today (my/tasks--today-string)))
    (seq-filter (lambda (task) (equal today (plist-get task :due)))
                (my/read-tasks))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst my/tasks-capture-buffer-name "*Tasks Capture*"
  "Name of the buffer used for interactive task capture.")

(defvar my/tasks-capture-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'my/tasks-capture-finalize)
    (define-key map (kbd "C-c C-k") #'my/tasks-capture-abort)
    map)
  "Keymap for `my/tasks-capture-mode'.")

(define-minor-mode my/tasks-capture-mode
  "Minor mode for the task capture buffer.
Provides \\[my/tasks-capture-finalize] to save and \\[my/tasks-capture-abort] to abort."
  :lighter " Capture"
  :keymap my/tasks-capture-mode-map)

(defun my/tasks--write-task-file (title body)
  "Write a new task file with TITLE and BODY. Return the resulting path."
  (my/tasks--ensure-dir my/tasks-directory)
  (let* ((slug (my/tasks--slugify title))
         (path (my/tasks--unique-path my/tasks-directory slug)))
    (with-temp-buffer
      (insert "---\n")
      (insert "status: inbox\n")
      (insert (format "created: %s\n" (my/tasks--now-string)))
      (insert "---\n\n")
      (insert (format "# %s\n" title))
      (unless (string-empty-p body)
        (insert "\n" body "\n"))
      (write-region (point-min) (point-max) path))
    path))

(defun my/tasks-capture ()
  "Open a buffer to compose a new task.
Edit title (the H1) and body, then commit with \\[my/tasks-capture-finalize]
or abort with \\[my/tasks-capture-abort]."
  (interactive)
  (my/tasks--ensure-dir my/tasks-directory)
  (let* ((existing (get-buffer my/tasks-capture-buffer-name))
         (buf (or existing
                  (get-buffer-create my/tasks-capture-buffer-name))))
    (unless existing
      (with-current-buffer buf
        (if (fboundp 'markdown-mode) (markdown-mode) (text-mode))
        (my/tasks-capture-mode 1)
        (insert "# \n\n")
        (setq header-line-format
              (substitute-command-keys
               " New task — finish with \\[my/tasks-capture-finalize], abort with \\[my/tasks-capture-abort]"))))
    (pop-to-buffer buf)
    (unless existing
      (goto-char (point-min))
      (end-of-line))))

(defun my/tasks--capture-parse ()
  "Parse the current capture buffer. Return (TITLE . BODY)."
  (save-excursion
    (goto-char (point-min))
    (unless (looking-at "^# \\(.*?\\)[[:space:]]*$")
      (user-error "First line must start with `# ' followed by the title"))
    (let ((title (string-trim (match-string 1))))
      (when (string-empty-p title)
        (user-error "Title is empty"))
      (forward-line 1)
      (let ((body (string-trim
                   (buffer-substring-no-properties (point) (point-max)))))
        (cons title body)))))

(defun my/tasks-capture-finalize ()
  "Write the captured task to disk and close the capture buffer."
  (interactive)
  (unless my/tasks-capture-mode
    (user-error "Not in a tasks capture buffer"))
  (let* ((parsed (my/tasks--capture-parse))
         (path (my/tasks--write-task-file (car parsed) (cdr parsed))))
    (quit-window t)
    (message "Captured: %s" (file-name-nondirectory path))))

(defun my/tasks-capture-abort ()
  "Abort task capture, discarding the buffer contents."
  (interactive)
  (unless my/tasks-capture-mode
    (user-error "Not in a tasks capture buffer"))
  (quit-window t)
  (message "Capture aborted"))

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

(defvar-local my/tasks-view-context-filter nil
  "If non-nil, restrict the current view to tasks containing this context.
Cleared by passing an empty string to `my/tasks-view-filter-context'.")

(defvar-local my/tasks-expanded-files nil
  "List of task file paths whose body is inlined under their entry.
Toggled with TAB. Reset on full re-render (e.g. `g', view switch).")

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
  (define-key map (kbd "TAB") #'my/tasks-toggle-expand-at-point)
  (define-key map (kbd "<tab>") #'my/tasks-toggle-expand-at-point)
  (define-key map (kbd "t") #'my/tasks-toggle-today)
  (define-key map (kbd "d") #'my/tasks-set-due)
  (define-key map (kbd "s") #'my/tasks-set-scheduled)
  (define-key map (kbd "r") #'my/tasks-set-reminder)
  (define-key map (kbd "p") #'my/tasks-set-status)
  (define-key map (kbd "x") #'my/tasks-mark-done)
  (define-key map (kbd "u") #'my/tasks-unarchive)
  (define-key map (kbd "m") #'my/tasks-open-mail)
  (define-key map (kbd "k") #'my/tasks-set-contexts)
  (define-key map (kbd "f") #'my/tasks-view-filter-context)
  (define-key map (kbd "/") #'my/tasks-search)
  (define-key map (kbd "g") #'my/tasks-view-refresh)
  (define-key map (kbd "v") #'my/tasks-view-cycle)
  (define-key map (kbd "i") #'my/tasks-show-inbox)
  (define-key map (kbd "T") #'my/tasks-show-today)
  (define-key map (kbd "A") #'my/tasks-show-archive)
  (define-key map (kbd "I") #'my/tasks-process-inbox)
  (define-key map (kbd "?") #'my/tasks-view-help)
  (define-key map (kbd "q") #'quit-window))

(defconst my/tasks--view-hint
  "RET·open  t·today  x·done  p·status  k·ctx  d·due  /·search  ?·help  q·quit"
  "Compact key-hint shown right-aligned in the header line of tasks views.")

(defun my/tasks--header-line ()
  "Return the right-aligned key-hint string for the header line."
  (concat
   (propertize " " 'display
               `(space :align-to (- right ,(+ 1 (length my/tasks--view-hint)))))
   my/tasks--view-hint))

(defun my/tasks-view-help ()
  "Show full key bindings for `my/tasks-mode' in a help window."
  (interactive)
  (with-help-window "*Tasks Help*"
    (princ "Tasks View — Keybindings\n\n")
    (princ "Per-task:\n")
    (princ "  RET   open the task file\n")
    (princ "  TAB   toggle inline body expansion\n")
    (princ "  t     toggle today\n")
    (princ "  d / s / r   set due / scheduled / reminder\n")
    (princ "  p     set status (completing-read)\n")
    (princ "  k     set contexts (multi-select)\n")
    (princ "  x     mark done (archive)\n")
    (princ "  u     un-archive\n")
    (princ "  m     open the linked mu4e message\n\n")
    (princ "Filter / search:\n")
    (princ "  f     filter current view by a context\n")
    (princ "  /     search all tasks (active + archive)\n\n")
    (princ "Navigation:\n")
    (princ "  g     refresh (commits pending status changes)\n")
    (princ "  v     cycle inbox → today → next → waiting → someday\n")
    (princ "  i     show inbox\n")
    (princ "  T     show today\n")
    (princ "  A     show archive\n")
    (princ "  I     start inbox-processing wizard\n")
    (princ "  q     quit (bury buffer)\n")
    (princ "  ?     this help")))

(define-derived-mode my/tasks-mode special-mode "Tasks"
  "Major mode for interactive task lists."
  (setq-local line-prefix "  ")
  (setq-local cursor-type 'bar)
  (setq-local header-line-format '(:eval (my/tasks--header-line))))

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

(defface my/tasks-context-face
  '((t :inherit font-lock-builtin-face))
  "Face for context chips like `@work'.")

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

(defun my/tasks--read-body (file)
  "Return FILE's body text (everything after YAML frontmatter and H1 title).
Trimmed; returns empty string when there is no body."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (looking-at "^---\n")
      (forward-line 1)
      (when (re-search-forward "^---$" nil t)
        (forward-line 1)))
    (while (and (not (eobp)) (looking-at "^[[:space:]]*$"))
      (forward-line 1))
    (when (looking-at "^# .*$")
      (forward-line 1))
    (while (and (not (eobp)) (looking-at "^[[:space:]]*$"))
      (forward-line 1))
    (string-trim (buffer-substring-no-properties (point) (point-max)))))

(defun my/tasks--goto-task-by-file (file)
  "Move point to FILE's task line in the current view. Return non-nil on success."
  (when file
    (let ((found nil))
      (goto-char (point-min))
      (while (and (not (eobp)) (not found))
        (when (equal (get-text-property (point) 'my/task-file) file)
          (setq found (point)))
        (unless found (forward-line 1)))
      (when found (goto-char found) t))))

(defun my/tasks--draw-view-content (display-title tasks query-fn arg)
  "Insert view content with TASKS into current buffer.
Honours the buffer-local context filter (`my/tasks-view-context-filter')
and renders pending annotations plus per-task context chips."
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq my/tasks-view-query query-fn)
  (setq my/tasks-view-query-arg arg)
  (when my/tasks-view-context-filter
    (setq tasks
          (seq-filter (lambda (task)
                        (member my/tasks-view-context-filter
                                (plist-get task :contexts)))
                      tasks)))
  (insert (propertize display-title 'face 'my/tasks-header-face))
  (when my/tasks-view-context-filter
    (insert (propertize "  ·  " 'face 'my/tasks-rule-face))
    (insert (propertize my/tasks-view-context-filter
                        'face 'my/tasks-context-face)))
  (insert "\n")
  (let ((rule-len (max 8 (+ (length display-title)
                            (if my/tasks-view-context-filter
                                (+ 5 (length my/tasks-view-context-filter))
                              0)))))
    (insert (propertize (make-string rule-len ?─)
                        'face 'my/tasks-rule-face)))
  (insert "\n\n")
  (if tasks
      (dolist (task tasks)
        (let* ((title (plist-get task :title))
               (due (plist-get task :due))
               (scheduled (plist-get task :scheduled))
               (reminder (plist-get task :reminder))
               (project (plist-get task :project))
               (contexts (plist-get task :contexts))
               (file (plist-get task :file))
               (pending (cdr (assoc file my/tasks-pending-changes)))
               (expanded (member file my/tasks-expanded-files))
               (start (point)))
          (insert (propertize (if expanded "▾ " "▸ ")
                              'face 'my/tasks-bullet-face))
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
          (when (listp contexts)
            (dolist (c contexts)
              (insert (propertize (format "  %s" c)
                                  'face 'my/tasks-context-face))))
          (when-let ((archived-at (plist-get task :archived-at)))
            (insert (propertize (format "  📦 %s" archived-at)
                                'face 'my/tasks-date-face)))
          (when pending
            (insert (propertize (format "  → %s" pending)
                                'face 'my/tasks-pending-face)))
          (insert "\n")
          (add-text-properties start (1- (point)) (list 'my/task-file file))
          (when expanded
            (let* ((body (my/tasks--read-body file))
                   (body-start (point)))
              (unless (string-empty-p body)
                (insert (replace-regexp-in-string "^" "    " body))
                (insert "\n")
                (add-text-properties body-start (point)
                                     (list 'my/task-file file)))))))
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
           (current-file (my/tasks--file-at-point))
           (display-title (replace-regexp-in-string "\\*" "" (buffer-name)))
           (tasks (if my/tasks-view-query-arg
                      (funcall my/tasks-view-query my/tasks-view-query-arg)
                    (funcall my/tasks-view-query))))
      (my/tasks--draw-view-content display-title tasks
                                   my/tasks-view-query my/tasks-view-query-arg)
      (unless (my/tasks--goto-task-by-file current-file)
        (goto-char (min pt (point-max)))))))

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

(defun my/tasks-show-context (ctx)
  "Show all active tasks that include CTX in their `contexts'."
  (interactive
   (list (completing-read "Context: " my/tasks-contexts nil t)))
  (my/tasks--render-buffer (format "*Context: %s*" ctx)
                           #'my/tasks-by-context ctx))

(defun my/tasks-search (query)
  "Grep all tasks (active + archive) for QUERY and show matches as a view."
  (interactive "sSearch tasks: ")
  (when (string-empty-p (string-trim query))
    (user-error "Empty search query"))
  (my/tasks--render-buffer (format "*Search: %s*" query)
                           #'my/tasks-search-results query))

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

(defun my/tasks-toggle-expand-at-point ()
  "Toggle inline expansion of the task at point.
When expanded, the task's body (everything after frontmatter and H1) is
shown indented underneath the entry and the bullet flips from ▸ to ▾."
  (interactive)
  (let ((file (my/tasks--file-at-point)))
    (unless file (user-error "No task at point"))
    (setq my/tasks-expanded-files
          (if (member file my/tasks-expanded-files)
              (delete file my/tasks-expanded-files)
            (cons file my/tasks-expanded-files)))
    (my/tasks--redraw-view)))

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

(defun my/tasks-set-contexts ()
  "Replace the contexts list on the task at point (multi-select)."
  (interactive)
  (let ((file (my/tasks--current-task-file)))
    (unless file (user-error "No task at point"))
    (let* ((current (plist-get (my/tasks--parse-frontmatter file) :contexts))
           (initial (when (and (listp current) current)
                      (mapconcat #'identity current ",")))
           (raw (completing-read-multiple
                 "Contexts: " my/tasks-contexts nil t initial))
           (contexts (delete "" (or raw '()))))
      (my/tasks--update-list-property file "contexts" contexts)
      (when (derived-mode-p 'my/tasks-mode)
        (my/tasks--redraw-view)))))

(defun my/tasks-view-filter-context ()
  "Filter the current view by a context.
Empty input clears the filter; setting a new value triggers a redraw."
  (interactive)
  (unless (derived-mode-p 'my/tasks-mode)
    (user-error "Not in a tasks view"))
  (let ((ctx (completing-read
              "Filter by context (empty to clear): "
              my/tasks-contexts nil nil)))
    (setq my/tasks-view-context-filter
          (if (string-empty-p ctx) nil ctx))
    (my/tasks--redraw-view)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inbox-Processing Wizard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/tasks--update-title (file new-title)
  "Replace the H1 in FILE with NEW-TITLE.
Inserts a new H1 line just after the closing `---' of the frontmatter
if no H1 is found."
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "^# .*$" nil t)
            (replace-match (format "# %s" new-title) t t)
          ;; No H1: skip past the closing frontmatter ---, then insert.
          (goto-char (point-min))
          (when (looking-at "^---\n")
            (forward-line 1)
            (when (re-search-forward "^---$" nil t)
              (forward-line 1)
              (insert (format "\n# %s\n" new-title))))))
      (save-buffer))))

(defun my/tasks--wizard-rename (file new-title)
  "Rename FILE's task to NEW-TITLE if it differs from current and is non-empty.
Returns t when the title was actually changed."
  (let ((current (plist-get (my/tasks--parse-frontmatter file) :title)))
    (cond
     ((null new-title) nil)
     ((string-empty-p (string-trim new-title)) nil)
     ((string= new-title current) nil)
     (t (my/tasks--update-title file new-title) t))))

(defun my/tasks--wizard-defer-date (char)
  "Return a YYYY-MM-DD date string from defer keystroke CHAR.
?1 = +1 day, ?7 = +1 week, ?m = +30 days, ?d = read date via org calendar."
  (pcase char
    (?1 (format-time-string "%Y-%m-%d"
                            (time-add (current-time) (days-to-time 1))))
    (?7 (format-time-string "%Y-%m-%d"
                            (time-add (current-time) (days-to-time 7))))
    (?m (format-time-string "%Y-%m-%d"
                            (time-add (current-time) (days-to-time 30))))
    (?d (my/tasks--read-date nil "Defer to: "))))

(defun my/tasks--apply-wizard-action (file action &optional payload)
  "Apply ACTION (symbol) to FILE. PAYLOAD is action-specific.

Actions:
  today / next / waiting / someday — set status only
  done                              — archive (status=done, move to archive dir)
  trash                             — delete the file
  defer                             — status=next + scheduled=PAYLOAD (date string)"
  (pcase action
    ('today    (my/tasks--update-property file "status" "today"))
    ('next     (my/tasks--update-property file "status" "next"))
    ('waiting  (my/tasks--update-property file "status" "waiting"))
    ('someday  (my/tasks--update-property file "status" "someday"))
    ('done     (my/tasks--archive-file file))
    ('trash
     (let ((buf (get-file-buffer file)))
       (when buf
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf)))
     (delete-file file))
    ('defer
     (my/tasks--update-property file "status" "next")
     (my/tasks--update-property file "scheduled" payload))
    (_ (error "Unknown wizard action: %s" action))))

(defconst my/tasks--wizard-action-chars
  '(?t ?n ?w ?s ?x ?T ?+ ?i ?q)
  "Valid keystrokes for the inbox-wizard status prompt.")

(defconst my/tasks--wizard-defer-chars
  '(?1 ?7 ?m ?d)
  "Valid keystrokes for the inbox-wizard defer sub-prompt.")

;; --- Prompt wrappers (mockable for tests) ---

(defun my/tasks--wizard-read-title (prompt default)
  "Wrapper around `read-from-minibuffer' so tests can stub it cleanly."
  (read-from-minibuffer prompt default))

(defun my/tasks--wizard-read-char (prompt chars)
  "Wrapper around `read-char-choice'."
  (read-char-choice prompt chars))

(defun my/tasks--wizard-confirm (prompt)
  "Wrapper around `y-or-n-p'."
  (y-or-n-p prompt))

(defun my/tasks--wizard-process-one (file i total)
  "Run the wizard for FILE (item I of TOTAL).
Returns t if a destination action was applied, nil if skipped.
Throws `tasks-wizard-quit' if the user chooses to quit."
  (let* ((task (my/tasks--parse-frontmatter file))
         (orig-title (plist-get task :title))
         (typed (my/tasks--wizard-read-title
                 (format "[%d/%d] Title (Enter to keep): " i total)
                 orig-title)))
    (my/tasks--wizard-rename file typed))
  (let* ((title (plist-get (my/tasks--parse-frontmatter file) :title))
         (prompt (format
                  "[%d/%d] %s\n  [t]oday [n]ext [w]aiting [s]omeday [x]done [T]rash [+]defer [i]skip [q]uit: "
                  i total title))
         (choice (my/tasks--wizard-read-char
                  prompt my/tasks--wizard-action-chars)))
    (pcase choice
      (?q (throw 'tasks-wizard-quit nil))
      (?i nil)
      (?+ (let* ((sub (my/tasks--wizard-read-char
                       "Defer: [1] +1d  [7] +1w  [m] +1mo  [d] pick date: "
                       my/tasks--wizard-defer-chars))
                 (date (my/tasks--wizard-defer-date sub)))
            (my/tasks--apply-wizard-action file 'defer date)
            t))
      (?T (when (my/tasks--wizard-confirm (format "Wirklich löschen %s? " title))
            (my/tasks--apply-wizard-action file 'trash)
            t))
      (?t (my/tasks--apply-wizard-action file 'today) t)
      (?n (my/tasks--apply-wizard-action file 'next) t)
      (?w (my/tasks--apply-wizard-action file 'waiting) t)
      (?s (my/tasks--apply-wizard-action file 'someday) t)
      (?x (my/tasks--apply-wizard-action file 'done) t))))

(defun my/tasks-process-inbox ()
  "Walk through every Inbox task one by one to formulate a next action.

For each item: prompt to rename the title to a concrete next action
\(Enter keeps the existing title), then pick a status with a single key:
  t today  n next  w waiting  s someday  x done(<2min)  T trash  + defer
  i skip   q quit
Defer asks for a span (+1d/+1w/+1mo/pick) and sets status to next."
  (interactive)
  (let* ((items (my/tasks-by-status "inbox"))
         (total (length items)))
    (if (zerop total)
        (message "Inbox is empty.")
      (let ((processed 0)
            (i 0))
        (catch 'tasks-wizard-quit
          (dolist (task items)
            (cl-incf i)
            (let ((file (plist-get task :file)))
              (when (and file (file-exists-p file))
                (when (my/tasks--wizard-process-one file i total)
                  (cl-incf processed))))))
        (message "Inbox wizard: processed %d of %d." processed total)))))

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

(defun my/tasks-capture-from-mu4e (&optional title)
  "Capture a task linked to the current mu4e message.
TITLE defaults to a `read-string' prompt when called interactively.
Saves the Message-ID in frontmatter so `my/tasks-open-mail' can jump back."
  (interactive (list nil))
  (let ((msg (and (fboundp 'mu4e-message-at-point)
                  (mu4e-message-at-point))))
    (unless msg (user-error "Not in a mu4e buffer"))
    (let* ((msgid (mu4e-message-field msg :message-id))
           (subject (or (mu4e-message-field msg :subject) ""))
           (from-email (my/tasks--mu4e-from-email
                        (mu4e-message-field msg :from)))
           (title (or title (read-string "Task title: ")))
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
(global-set-key (kbd "C-c t k") #'my/tasks-set-contexts)
(global-set-key (kbd "C-c t @") #'my/tasks-show-context)
(global-set-key (kbd "C-c t /") #'my/tasks-search)
(global-set-key (kbd "C-c t I") #'my/tasks-process-inbox)

(provide 'tasks)
;;; tasks.el ends here
