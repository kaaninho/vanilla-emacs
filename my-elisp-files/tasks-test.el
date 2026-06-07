;;; tasks-test.el --- Tests for tasks.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'tasks)

(defmacro tasks-test--with-temp-dirs (&rest body)
  "Run BODY with `my/tasks-directory' and `my/tasks-archive-directory' as temp paths.
The macro exposes `temp-dir' as the active-tasks directory."
  (declare (indent 0) (debug t))
  `(let* ((temp-dir (make-temp-file "tasks-test-" t))
          (my/tasks-directory temp-dir)
          (my/tasks-archive-directory (expand-file-name "archive" temp-dir)))
     (unwind-protect
         (progn ,@body)
       (dolist (buf (buffer-list))
         (let ((bfn (buffer-file-name buf)))
           (when (and bfn
                      (string-prefix-p
                       (file-name-as-directory (expand-file-name temp-dir))
                       (expand-file-name bfn)))
             (with-current-buffer buf (set-buffer-modified-p nil))
             (kill-buffer buf))))
       (dolist (name '("*Inbox*" "*Today*" "*Next*" "*Waiting*" "*Someday*" "*Archive*"))
         (when (get-buffer name) (kill-buffer name)))
       (when (file-directory-p temp-dir)
         (delete-directory temp-dir t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slugify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest tasks-test--slugify-basic ()
  (should (equal (my/tasks--slugify "Hello World") "hello-world"))
  (should (equal (my/tasks--slugify "FOO  bar") "foo-bar"))
  (should (equal (my/tasks--slugify "") "task")))

(ert-deftest tasks-test--slugify-umlauts ()
  (should (equal (my/tasks--slugify "Wäsche waschen") "waesche-waschen"))
  (should (equal (my/tasks--slugify "Größe ändern für Üben")
                 "groesse-aendern-fuer-ueben")))

(ert-deftest tasks-test--slugify-strips-brackets ()
  (should (equal (my/tasks--slugify "[[E-Mail schreiben]]")
                 "e-mail-schreiben")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frontmatter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest tasks-test--parse-frontmatter ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\ndue: 2026-06-08\n---\n\n# Title\n"))
      (let ((task (my/tasks--parse-frontmatter file)))
        (should (equal (plist-get task :status) "inbox"))
        (should (equal (plist-get task :due) "2026-06-08"))
        (should (equal (plist-get task :title) "Title"))))))

(ert-deftest tasks-test--parse-frontmatter-quoted-value ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\nproject: \"[[Projekt X]]\"\n---\n\n# T\n"))
      (let ((task (my/tasks--parse-frontmatter file)))
        (should (equal (plist-get task :project) "[[Projekt X]]"))))))

(ert-deftest tasks-test--update-property-replace ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file (insert "---\nstatus: inbox\n---\n\n# T\n"))
      (my/tasks--update-property file "status" "today")
      (should (equal (plist-get (my/tasks--parse-frontmatter file) :status)
                     "today")))))

(ert-deftest tasks-test--update-property-add ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file (insert "---\nstatus: inbox\n---\n\n# T\n"))
      (my/tasks--update-property file "due" "2026-06-08")
      (let ((task (my/tasks--parse-frontmatter file)))
        (should (equal (plist-get task :due) "2026-06-08"))
        (should (equal (plist-get task :status) "inbox"))))))

(ert-deftest tasks-test--update-property-remove ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\ndue: 2026-06-08\n---\n\n# T\n"))
      (my/tasks--update-property file "due" nil)
      (let ((task (my/tasks--parse-frontmatter file)))
        (should-not (plist-get task :due))
        (should (equal (plist-get task :status) "inbox"))))))

(ert-deftest tasks-test--update-property-quotes-bracketed-value ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file (insert "---\nstatus: inbox\n---\n\n# T\n"))
      (my/tasks--update-property file "project" "[[Projekt X]]")
      (let ((task (my/tasks--parse-frontmatter file)))
        (should (equal (plist-get task :project) "[[Projekt X]]"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest tasks-test--capture-creates-file ()
  (tasks-test--with-temp-dirs
    (my/tasks-capture "Test Task")
    (let* ((path (expand-file-name "test-task.md" temp-dir))
           (task (my/tasks--parse-frontmatter path)))
      (should (file-exists-p path))
      (should (equal (plist-get task :status) "inbox"))
      (should (equal (plist-get task :title) "Test Task"))
      (should (plist-get task :created)))))

(ert-deftest tasks-test--capture-collision ()
  (tasks-test--with-temp-dirs
    (my/tasks-capture "Same Title")
    (my/tasks-capture "Same Title")
    (should (file-exists-p (expand-file-name "same-title.md" temp-dir)))
    (should (file-exists-p (expand-file-name "same-title-2.md" temp-dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading & filtering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest tasks-test--read-tasks-ignores-non-tasks ()
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: inbox\n---\n\n# A\n"))
    (with-temp-file (expand-file-name "b.md" temp-dir)
      (insert "Random notes.\n"))
    (with-temp-file (expand-file-name "c.md" temp-dir)
      (insert "---\ntitle: c\n---\n# C\n"))
    (let ((tasks (my/read-tasks)))
      (should (= (length tasks) 1))
      (should (equal (plist-get (car tasks) :title) "A")))))

(ert-deftest tasks-test--read-tasks-excludes-archive-subdir ()
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: inbox\n---\n\n# A\n"))
    (make-directory my/tasks-archive-directory)
    (with-temp-file (expand-file-name "x.md" my/tasks-archive-directory)
      (insert "---\nstatus: done\n---\n\n# X\n"))
    (let ((tasks (my/read-tasks)))
      (should (= (length tasks) 1))
      (should (equal (plist-get (car tasks) :title) "A")))))

(ert-deftest tasks-test--by-status ()
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: inbox\n---\n\n# A\n"))
    (with-temp-file (expand-file-name "b.md" temp-dir)
      (insert "---\nstatus: next\n---\n\n# B\n"))
    (with-temp-file (expand-file-name "c.md" temp-dir)
      (insert "---\nstatus: today\n---\n\n# C\n"))
    (should (= (length (my/tasks-by-status "inbox")) 1))
    (should (= (length (my/tasks-by-status "next")) 1))
    (should (= (length (my/tasks-today)) 1))))

(ert-deftest tasks-test--due-today ()
  (tasks-test--with-temp-dirs
    (let ((today (format-time-string "%Y-%m-%d")))
      (with-temp-file (expand-file-name "a.md" temp-dir)
        (insert (format "---\nstatus: next\ndue: %s\n---\n\n# A\n" today)))
      (with-temp-file (expand-file-name "b.md" temp-dir)
        (insert "---\nstatus: next\ndue: 2099-12-31\n---\n\n# B\n"))
      (should (= (length (my/tasks-due-today)) 1))
      (should (equal (plist-get (car (my/tasks-due-today)) :title) "A")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest tasks-test--toggle-today ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "a.md" temp-dir)))
      (with-temp-file file (insert "---\nstatus: next\n---\n\n# A\n"))
      (find-file file)
      (my/tasks-toggle-today)
      (should (equal (plist-get (my/tasks--parse-frontmatter file) :status)
                     "today"))
      (my/tasks-toggle-today)
      (should (equal (plist-get (my/tasks--parse-frontmatter file) :status)
                     "next")))))

(ert-deftest tasks-test--mark-done-archives ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "a.md" temp-dir)))
      (with-temp-file file (insert "---\nstatus: inbox\n---\n\n# A\n"))
      (find-file file)
      (my/tasks-mark-done)
      (should-not (file-exists-p file))
      (let* ((archive-files (directory-files
                             my/tasks-archive-directory t "\\.md\\'"))
             (archived-path (car archive-files)))
        (should (= 1 (length archive-files)))
        (let ((task (my/tasks--parse-frontmatter archived-path)))
          (should (equal (plist-get task :status) "done"))
          (should (plist-get task :archived-at)))))))

(ert-deftest tasks-test--archived-file-has-date-prefix ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "wash-clothes.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\n---\n\n# Wash clothes\n"))
      (find-file file)
      (my/tasks-mark-done)
      (let ((archive-files (directory-files
                            my/tasks-archive-directory nil "\\.md\\'")))
        (should (= 1 (length archive-files)))
        (should (string-match-p
                 (concat "^" (format-time-string "%Y-%m-%d")
                         "-wash-clothes\\.md$")
                 (car archive-files)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest tasks-test--view-cursor-lands-on-first-task ()
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: inbox\n---\n\n# A\n"))
    (my/tasks-show-inbox)
    (with-current-buffer "*Inbox*"
      (should (get-text-property (point) 'my/task-file)))))

(ert-deftest tasks-test--view-mark-done-queues-then-commits-on-refresh ()
  "In a view `x' queues the change; the actual archive happens on refresh."
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "a.md" temp-dir)))
      (with-temp-file file (insert "---\nstatus: inbox\n---\n\n# A\n"))
      (my/tasks-show-inbox)
      (with-current-buffer "*Inbox*"
        (my/tasks-mark-done)
        ;; Queued: file still in active, annotation visible.
        (should (file-exists-p file))
        (should (assoc file my/tasks-pending-changes))
        (should (string-match-p "→ done" (buffer-string)))
        ;; Refresh commits.
        (my/tasks-view-refresh))
      (should-not (file-exists-p file))
      (should (= 1 (length (directory-files my/tasks-archive-directory
                                            t "\\.md\\'")))))))

(ert-deftest tasks-test--view-commits-on-reopen ()
  "Re-invoking the view from outside the buffer commits pending changes."
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "a.md" temp-dir)))
      (with-temp-file file (insert "---\nstatus: inbox\n---\n\n# A\n"))
      (my/tasks-show-inbox)
      (with-current-buffer "*Inbox*" (my/tasks-set-status "next"))
      ;; File should still be in inbox status (pending only).
      (should (equal (plist-get (my/tasks--parse-frontmatter file) :status)
                     "inbox"))
      ;; Reopening the view triggers commit.
      (my/tasks-show-inbox)
      (should (equal (plist-get (my/tasks--parse-frontmatter file) :status)
                     "next")))))

(ert-deftest tasks-test--view-toggle-today-queues ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "a.md" temp-dir)))
      (with-temp-file file (insert "---\nstatus: next\n---\n\n# A\n"))
      (my/tasks-show-next)
      (with-current-buffer "*Next*"
        (my/tasks-toggle-today)
        (should (equal (cdr (assoc file my/tasks-pending-changes)) "today"))
        ;; Toggle again: today → next.
        (my/tasks-toggle-today)
        (should (equal (cdr (assoc file my/tasks-pending-changes)) "next")))
      ;; Still next on disk.
      (should (equal (plist-get (my/tasks--parse-frontmatter file) :status)
                     "next")))))

(ert-deftest tasks-test--archive-view-lists-archived ()
  (tasks-test--with-temp-dirs
    (make-directory my/tasks-archive-directory t)
    (with-temp-file (expand-file-name "2026-05-26-old.md"
                                      my/tasks-archive-directory)
      (insert "---\nstatus: done\narchived-at: 2026-05-26\n---\n\n# Old\n"))
    (my/tasks-show-archive)
    (with-current-buffer "*Archive*"
      (should (string-match-p "Old" (buffer-string))))))

(ert-deftest tasks-test--view-commit-from-archive-back-to-active ()
  "Setting status on an archived task in the view should move it back on commit."
  (tasks-test--with-temp-dirs
    (make-directory my/tasks-archive-directory t)
    (let ((archived (expand-file-name "2026-05-26-old.md"
                                      my/tasks-archive-directory)))
      (with-temp-file archived
        (insert "---\nstatus: done\narchived-at: 2026-05-26\n---\n\n# Old\n"))
      (my/tasks-show-archive)
      (with-current-buffer "*Archive*"
        (my/tasks-set-status "inbox")
        (my/tasks-view-refresh))
      (should-not (file-exists-p archived))
      (let ((restored (expand-file-name "old.md" temp-dir)))
        (should (file-exists-p restored))
        (let ((task (my/tasks--parse-frontmatter restored)))
          (should (equal (plist-get task :status) "inbox"))
          (should-not (plist-get task :archived-at)))))))

(ert-deftest tasks-test--current-task-file-excludes-archive ()
  "Inside an archived task buffer, status/date actions should refuse to operate."
  (tasks-test--with-temp-dirs
    (make-directory my/tasks-archive-directory t)
    (let ((archived (expand-file-name "2026-05-26-old.md"
                                      my/tasks-archive-directory)))
      (with-temp-file archived
        (insert "---\nstatus: done\narchived-at: 2026-05-26\n---\n\n# Old\n"))
      (find-file archived)
      (should-not (my/tasks--current-task-file))
      (should-error (my/tasks-toggle-today) :type 'user-error))))

(ert-deftest tasks-test--view-date-setter-does-not-commit-pending ()
  "Setting a date in a view must NOT commit pending status changes."
  (tasks-test--with-temp-dirs
    (let ((a (expand-file-name "a.md" temp-dir))
          (b (expand-file-name "b.md" temp-dir)))
      (with-temp-file a (insert "---\nstatus: inbox\n---\n\n# A\n"))
      (with-temp-file b (insert "---\nstatus: inbox\n---\n\n# B\n"))
      (my/tasks-show-inbox)
      (with-current-buffer "*Inbox*"
        ;; Queue a status change on A.
        (goto-char (or (next-single-property-change (point-min) 'my/task-file)
                       (point-min)))
        (my/tasks-mark-done)
        ;; Now set due on B (next task line).
        (let ((next (next-single-property-change (point) 'my/task-file)))
          (when next (goto-char (1+ next))))
        (my/tasks-set-due "2026-12-31"))
      ;; A's status change should still be pending (file not yet archived).
      (should (file-exists-p a))
      ;; B's due date is on disk.
      (should (equal (plist-get (my/tasks--parse-frontmatter b) :due)
                     "2026-12-31"))
      ;; Pending changes for A still in buffer.
      (with-current-buffer "*Inbox*"
        (should (assoc a my/tasks-pending-changes))))))

(ert-deftest tasks-test--multiple-pending-commit-together ()
  (tasks-test--with-temp-dirs
    (let ((a (expand-file-name "a.md" temp-dir))
          (b (expand-file-name "b.md" temp-dir))
          (c (expand-file-name "c.md" temp-dir)))
      (with-temp-file a (insert "---\nstatus: inbox\n---\n\n# A\n"))
      (with-temp-file b (insert "---\nstatus: inbox\n---\n\n# B\n"))
      (with-temp-file c (insert "---\nstatus: inbox\n---\n\n# C\n"))
      (my/tasks-show-inbox)
      (with-current-buffer "*Inbox*"
        ;; Programmatically queue three changes.
        (my/tasks--queue-status-change a "next")
        (my/tasks--queue-status-change b "today")
        (my/tasks--queue-status-change c "done")
        (my/tasks-view-refresh))
      (should (equal (plist-get (my/tasks--parse-frontmatter a) :status) "next"))
      (should (equal (plist-get (my/tasks--parse-frontmatter b) :status) "today"))
      (should-not (file-exists-p c))
      (should (= 1 (length (directory-files my/tasks-archive-directory
                                            t "\\.md\\'")))))))

(ert-deftest tasks-test--queue-overrides-previous ()
  "Queueing the same file twice should keep only the latest target status."
  (tasks-test--with-temp-dirs
    (let ((a (expand-file-name "a.md" temp-dir)))
      (with-temp-file a (insert "---\nstatus: inbox\n---\n\n# A\n"))
      (my/tasks-show-inbox)
      (with-current-buffer "*Inbox*"
        (my/tasks--queue-status-change a "done")
        (my/tasks--queue-status-change a "next")
        (should (equal (cdr (assoc a my/tasks-pending-changes)) "next"))
        (should (= 1 (length my/tasks-pending-changes)))
        (my/tasks-view-refresh))
      ;; Final status is "next"; file still in active dir (not archived).
      (should (file-exists-p a))
      (should (equal (plist-get (my/tasks--parse-frontmatter a) :status) "next")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mu4e integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest tasks-test--mu4e-from-name-extracts-name ()
  (should (equal (my/tasks--mu4e-from-name "Just a string") "Just a string"))
  (should (equal (my/tasks--mu4e-from-name
                  '((:name "Alice" :email "a@b")))
                 "Alice"))
  (should (equal (my/tasks--mu4e-from-name
                  '(("Bob" . "bob@example.com")))
                 "Bob"))
  (should (equal (my/tasks--mu4e-from-name
                  '((:email "no-name@host")))
                 "no-name@host"))
  (should (equal (my/tasks--mu4e-from-name nil) "")))

(ert-deftest tasks-test--capture-from-mu4e-stores-msgid ()
  (tasks-test--with-temp-dirs
    (cl-letf (((symbol-function 'mu4e-message-at-point)
               (lambda () (list :from '((:name "Alice" :email "a@b"))
                                :subject "Important request"
                                :message-id "abc123@host")))
              ((symbol-function 'mu4e-message-field)
               (lambda (msg field) (plist-get msg field)))
              ((symbol-function 'read-string)
               (lambda (&rest _) "Important request")))
      (my/tasks-capture-from-mu4e))
    (let* ((path (expand-file-name "important-request.md" temp-dir))
           (task (my/tasks--parse-frontmatter path)))
      (should (file-exists-p path))
      (should (equal (plist-get task :status) "inbox"))
      (should (equal (plist-get task :mu4e-msgid) "abc123@host"))
      (with-temp-buffer
        (insert-file-contents path)
        (should (string-match-p "Von: Alice" (buffer-string)))
        (should (string-match-p "Betreff: Important request" (buffer-string)))))))

(ert-deftest tasks-test--capture-from-mu4e-errors-outside-mu4e ()
  (tasks-test--with-temp-dirs
    (cl-letf (((symbol-function 'mu4e-message-at-point) (lambda () nil)))
      (should-error (my/tasks-capture-from-mu4e) :type 'user-error))))

(ert-deftest tasks-test--open-mail-uses-frontmatter-msgid ()
  "Uses `mu4e-view-message-with-message-id' (current mu4e API)."
  (tasks-test--with-temp-dirs
    (let* ((path (expand-file-name "a.md" temp-dir))
           (called-with nil))
      (with-temp-file path
        (insert "---\nstatus: inbox\nmu4e-msgid: my-id@host\n---\n\n# A\n"))
      (find-file path)
      (cl-letf (((symbol-function 'mu4e-view-message-with-message-id)
                 (lambda (id) (setq called-with id)))
                ((symbol-function 'require) (lambda (&rest _) t)))
        (my/tasks-open-mail))
      (should (equal called-with "my-id@host")))))

(ert-deftest tasks-test--open-mail-falls-back-to-old-mu4e ()
  "Falls back to `mu4e-view-message-with-msgid' when only the old name exists."
  (tasks-test--with-temp-dirs
    (let* ((path (expand-file-name "a.md" temp-dir))
           (called-with nil))
      (with-temp-file path
        (insert "---\nstatus: inbox\nmu4e-msgid: my-id@host\n---\n\n# A\n"))
      (find-file path)
      (cl-letf (((symbol-function 'mu4e-view-message-with-message-id) nil)
                ((symbol-function 'mu4e-view-message-with-msgid)
                 (lambda (id) (setq called-with id)))
                ((symbol-function 'fboundp)
                 (lambda (sym) (eq sym 'mu4e-view-message-with-msgid)))
                ((symbol-function 'require) (lambda (&rest _) t)))
        (my/tasks-open-mail))
      (should (equal called-with "my-id@host")))))

(ert-deftest tasks-test--open-mail-errors-without-msgid ()
  (tasks-test--with-temp-dirs
    (let ((path (expand-file-name "a.md" temp-dir)))
      (with-temp-file path
        (insert "---\nstatus: inbox\n---\n\n# A\n"))
      (find-file path)
      (should-error (my/tasks-open-mail) :type 'user-error))))

(ert-deftest tasks-test--open-mail-from-archived-file ()
  "open-mail should also work from inside an archived task buffer."
  (tasks-test--with-temp-dirs
    (make-directory my/tasks-archive-directory t)
    (let ((archived (expand-file-name "2026-05-26-old.md"
                                      my/tasks-archive-directory))
          (called-with nil))
      (with-temp-file archived
        (insert "---\nstatus: done\narchived-at: 2026-05-26\n"
                "mu4e-msgid: archived-id@host\n---\n\n# Old\n"))
      (find-file archived)
      (cl-letf (((symbol-function 'mu4e-view-message-with-message-id)
                 (lambda (id) (setq called-with id)))
                ((symbol-function 'require) (lambda (&rest _) t)))
        (my/tasks-open-mail))
      (should (equal called-with "archived-id@host")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Date face — edge cases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest tasks-test--date-face-handles-time-component ()
  "Date-face must strip the time before comparing against today."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (today-with-time (concat today " 09:00")))
    (should (eq (my/tasks--date-face today-with-time) 'my/tasks-due-today-face))
    (should (eq (my/tasks--date-face "2099-12-31 23:59") 'my/tasks-date-face))
    (should (eq (my/tasks--date-face "1999-01-01 00:01") 'my/tasks-overdue-face))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Un-archive
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest tasks-test--unarchive-strips-date-prefix ()
  (tasks-test--with-temp-dirs
    (make-directory my/tasks-archive-directory t)
    (let ((archived (expand-file-name "2026-05-26-old.md"
                                      my/tasks-archive-directory)))
      (with-temp-file archived
        (insert "---\nstatus: done\narchived-at: 2026-05-26\n---\n\n# Old\n"))
      (find-file archived)
      (my/tasks-unarchive "next")
      (should-not (file-exists-p archived))
      (let ((new-file (expand-file-name "old.md" temp-dir)))
        (should (file-exists-p new-file))
        (let ((task (my/tasks--parse-frontmatter new-file)))
          (should (equal (plist-get task :status) "next"))
          (should-not (plist-get task :archived-at)))))))

(ert-deftest tasks-test--unarchive-default-status ()
  (tasks-test--with-temp-dirs
    (make-directory my/tasks-archive-directory t)
    (let ((archived (expand-file-name "2026-05-26-old.md"
                                      my/tasks-archive-directory)))
      (with-temp-file archived
        (insert "---\nstatus: done\narchived-at: 2026-05-26\n---\n\n# Old\n"))
      (find-file archived)
      (my/tasks-unarchive "inbox")
      (let* ((new-file (expand-file-name "old.md" temp-dir))
             (task (my/tasks--parse-frontmatter new-file)))
        (should (equal (plist-get task :status) "inbox"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Date face helper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest tasks-test--date-face ()
  (let ((today (format-time-string "%Y-%m-%d")))
    (should (eq (my/tasks--date-face today) 'my/tasks-due-today-face))
    (should (eq (my/tasks--date-face "1999-01-01") 'my/tasks-overdue-face))
    (should (eq (my/tasks--date-face "2999-01-01") 'my/tasks-date-face))
    (should-not (my/tasks--date-face nil))))

(provide 'tasks-test)
;;; tasks-test.el ends here
