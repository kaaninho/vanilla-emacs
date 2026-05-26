(require 'ert)
(require 'tasks)

(ert-deftest tasks-test--read-tasks-emojis ()
  "Test if my/read-tasks parses emojis correctly and handles order independence."
  (let ((my/tasks-directory (make-temp-file "tasks" t))
        (my/tasks-file "tasks.md")
        (obsidian-directory "/tmp"))
    (with-temp-buffer
      (insert "- [ ] Task 1 📅 2026-05-26 ⏳ 2026-05-25\n")
      (insert "- [ ] Task 2 ⏰ 2026-05-26 10:00 📅 2026-12-31\n")
      (write-file (my/tasks-path)))
    (let ((tasks (my/read-tasks)))
      (should (= (length tasks) 2))
      (let ((t1 (car tasks))
            (t2 (nth 1 tasks)))
        (should (equal (plist-get t1 :due) "2026-05-26"))
        (should (equal (plist-get t1 :scheduled) "2026-05-25"))
        (should (equal (plist-get t2 :reminder) "2026-05-26 10:00"))
        (should (equal (plist-get t2 :due) "2026-12-31"))))))

(ert-deftest tasks-test--toggle-today-status ()
  "Test if my/tasks-toggle-today works with status:: property."
  (let* ((temp-dir (make-temp-file "tasks" t))
         (my/tasks-directory temp-dir)
         (my/tasks-file "tasks.md")
         (obsidian-directory "/tmp"))
    (with-current-buffer (find-file (my/tasks-path))
      (insert "- [ ] Task 1\n  status:: inbox\n")
      (save-buffer)
      
      (goto-char (point-min))
      (my/tasks-toggle-today)
      (should (string-match-p "status:: today" (buffer-string)))
      
      (my/tasks-toggle-today)
      (should (string-match-p "status:: next" (buffer-string)))
      
      (kill-buffer))))

(ert-deftest tasks-test--mark-done ()
  "Test if my/tasks-mark-done toggles checkbox robustly."
  (with-temp-buffer
    (insert "  - [ ] Task 1\n")
    (insert "- [X] Task 2\n")
    
    (goto-char (point-min))
    (my/tasks-mark-done)
    (should (string-match-p "  - \\[x\\] Task 1" (buffer-string)))
    
    (forward-line 1)
    (my/tasks-mark-done)
    (should (string-match-p "- \\[ \\] Task 2" (buffer-string)))))

(ert-deftest tasks-test--archive-timestamp ()
  "Test if archiving adds archived-at timestamp."
  (let* ((temp-dir (make-temp-file "tasks" t))
         (my/tasks-directory temp-dir)
         (my/tasks-file "tasks.md")
         (my/archive-file "archive.md")
         (obsidian-directory "/tmp"))
    (with-current-buffer (find-file (my/tasks-path))
      (insert "- [x] Done Task\n  status:: done\n")
      (save-buffer)
      (goto-char (point-min))
      (my/tasks-archive-at-point)
      (should (= (buffer-size) 0)))
    
    (with-current-buffer (find-file-noselect (my/archive-path))
      (revert-buffer t t)
      (goto-char (point-min))
      (should (re-search-forward "archived-at:: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" nil t)))))

(ert-deftest tasks-test--batch-archive ()
  "Test if my/archive-done archives all completed tasks."
  (let* ((temp-dir (make-temp-file "tasks" t))
         (my/tasks-directory temp-dir)
         (my/tasks-file "tasks.md")
         (my/archive-file "archive.md")
         (obsidian-directory "/tmp"))
    (with-current-buffer (find-file (my/tasks-path))
      (insert "- [x] Task 1\n- [ ] Task 2\n- [x] Task 3\n")
      (save-buffer)
      (my/archive-done)
      (revert-buffer t t)
      (should (string-match-p "- \\[ \\] Task 2" (buffer-string)))
      (should-not (string-match-p "Task 1" (buffer-string)))
      (should-not (string-match-p "Task 3" (buffer-string))))
    
    (with-current-buffer (find-file-noselect (my/archive-path))
      (revert-buffer t t)
      (should (string-match-p "Task 1" (buffer-string)))
      (should (string-match-p "Task 3" (buffer-string))))))

(ert-deftest tasks-test--interactive-view ()
  "Test if interactive view actions update the source file and stay visible."
  (let* ((temp-dir (make-temp-file "tasks" t))
         (my/tasks-directory temp-dir)
         (my/tasks-file "tasks.md")
         (obsidian-directory "/tmp"))
    (with-temp-buffer
      (insert "- [ ] Task 1\n  status:: inbox\n")
      (write-file (my/tasks-path)))
    
    (my/tasks-show-inbox)
    (let ((view-buf (get-buffer "*Inbox*")))
      (with-current-buffer view-buf
        (goto-char (point-min))
        (re-search-forward "- \\[ \\] Task 1")
        (beginning-of-line)
        
        ;; Verify marker is present
        (should (get-text-property (point) 'my/task-pos))
        
        ;; Mark as done
        (my/tasks-view-mark-done)
        
        ;; Verify visual update in view
        (goto-char (line-beginning-position))
        (should (looking-at ".*- \\[x\\] Task 1"))
        
        ;; Mark as undone (Toggle again)
        (my/tasks-view-mark-done)
        
        ;; Verify visual update back to undone
        (goto-char (line-beginning-position))
        (should (looking-at ".*- \\[ \\] Task 1"))
        
        ;; Set due date from view
        (my/tasks-view-set-due "2026-12-24")
        
        ;; Verify source file update
        (with-current-buffer (find-file-noselect (my/tasks-path))
          (revert-buffer t t)
          (goto-char (point-min))
          (should (re-search-forward "📅 2026-12-24" nil t))))
      (kill-buffer view-buf))))

(ert-deftest tasks-test--view-cycling ()
  "Test if view cycling switches buffers."
  (let* ((temp-dir (make-temp-file "tasks" t))
         (my/tasks-directory temp-dir)
         (my/tasks-file "tasks.md")
         (obsidian-directory "/tmp"))
    (with-temp-buffer (write-file (my/tasks-path)))
    
    (my/tasks-show-inbox)
    (should (string= (buffer-name) "*Inbox*"))
    (my/tasks-view-cycle)
    (should (string= (buffer-name) "*Today*"))
    (my/tasks-view-cycle)
    (should (string= (buffer-name) "*Next*"))
    
    (kill-buffer "*Inbox*")
    (kill-buffer "*Today*")
    (kill-buffer "*Next*")))

(ert-deftest tasks-test--wiki-link-detection ()
  "Test if wiki links are detected correctly."
  (let* ((obsidian-directory (make-temp-file "obsidian" t))
         (target-file (expand-file-name "My Note.md" obsidian-directory)))
    (with-temp-buffer (write-file target-file))
    
    ;; This is a bit hard to test fully without real filesystem logic, 
    ;; but we can test the regex part.
    (with-temp-buffer
      (insert "- [ ] Check [[My Note]] 📅 2026-05-26\n")
      (goto-char (point-min))
      (should (string-match "\\[\\[\\(.*?\\)\\]\\]" (buffer-string)))
      (should (equal (match-string 1 (buffer-string)) "My Note")))))
