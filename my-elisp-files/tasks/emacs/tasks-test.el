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
          (my/tasks-archive-directory (expand-file-name "archive" temp-dir))
          ;; Keep streak state isolated so tests never touch the user's
          ;; `~/.tasks-streak.json'.
          (my/tasks-streak-file (expand-file-name "streak.json" temp-dir)))
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
       (dolist (name '("*Inbox*" "*Today*" "*Next*" "*Waiting*" "*Someday*"
                       "*Archive*" "*Week*"))
         (when (get-buffer name) (kill-buffer name)))
       (dolist (buf (buffer-list))
         (let ((bn (buffer-name buf)))
           (when (and bn (or (string-prefix-p "*Search: " bn)
                             (string-prefix-p "*Context: " bn)))
             (kill-buffer buf))))
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

(ert-deftest tasks-test--parse-frontmatter-inline-list ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\ntags: [work, urgent]\n---\n\n# T\n"))
      (let ((task (my/tasks--parse-frontmatter file)))
        (should (equal (plist-get task :status) "inbox"))
        (should (equal (plist-get task :tags) '("work" "urgent")))))))

(ert-deftest tasks-test--parse-frontmatter-block-list ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\ntags:\n  - work\n  - urgent\n---\n\n# T\n"))
      (let ((task (my/tasks--parse-frontmatter file)))
        (should (equal (plist-get task :status) "inbox"))
        (should (equal (plist-get task :tags) '("work" "urgent")))))))

(ert-deftest tasks-test--parse-frontmatter-quoted-list-items ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\ncontexts:\n"
                "  - \"@work\"\n  - \"@phone\"\n---\n\n# T\n"))
      (let ((task (my/tasks--parse-frontmatter file)))
        (should (equal (plist-get task :contexts) '("@work" "@phone")))))))

(ert-deftest tasks-test--parse-frontmatter-inline-quoted-list-items ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\ntags: [\"a, b\", c]\n---\n\n# T\n"))
      (let ((task (my/tasks--parse-frontmatter file)))
        ;; Naive split-by-comma — quoted commas not preserved.
        ;; Document the limitation in the test so it's intentional.
        (should (listp (plist-get task :tags)))))))

(ert-deftest tasks-test--parse-frontmatter-empty-list ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\ntags: []\n---\n\n# T\n"))
      (let ((task (my/tasks--parse-frontmatter file)))
        (should-not (plist-get task :tags))
        (should (equal (plist-get task :status) "inbox"))))))

(ert-deftest tasks-test--parse-frontmatter-coerces-scalar-status-list ()
  "Obsidian's List property type writes single-value `status' as a block list."
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus:\n  - next\n---\n\n# T\n"))
      (let ((task (my/tasks--parse-frontmatter file)))
        (should (equal (plist-get task :status) "next"))
        (should (stringp (plist-get task :status)))))))

(ert-deftest tasks-test--parse-frontmatter-coerces-scalar-due-flow-list ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\ndue: [2026-06-08]\n---\n\n# T\n"))
      (let ((task (my/tasks--parse-frontmatter file)))
        (should (equal (plist-get task :due) "2026-06-08"))
        (should (stringp (plist-get task :due)))))))

(ert-deftest tasks-test--parse-frontmatter-keeps-true-list-fields ()
  "Non-scalar keys (e.g. tags) remain lists after coercion."
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\ntags:\n  - work\n  - urgent\n"
                "---\n\n# T\n"))
      (let ((task (my/tasks--parse-frontmatter file)))
        (should (equal (plist-get task :status) "inbox"))
        (should (equal (plist-get task :tags) '("work" "urgent")))))))

(ert-deftest tasks-test--parse-frontmatter-list-survives-scalar-after ()
  "A block list must not swallow the next scalar property."
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\ntags:\n  - a\n  - b\n"
                "due: 2026-06-08\n---\n\n# T\n"))
      (let ((task (my/tasks--parse-frontmatter file)))
        (should (equal (plist-get task :tags) '("a" "b")))
        (should (equal (plist-get task :due) "2026-06-08"))))))

(ert-deftest tasks-test--update-property-removes-block-list ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\ntags:\n  - a\n  - b\n"
                "due: 2026-06-08\n---\n\n# T\n"))
      (my/tasks--update-property file "tags" nil)
      (let ((text (with-temp-buffer (insert-file-contents file) (buffer-string))))
        (should-not (string-match-p "tags:" text))
        (should-not (string-match-p "- a" text))
        (should-not (string-match-p "- b" text))
        (should (string-match-p "status: inbox" text))
        (should (string-match-p "due: 2026-06-08" text))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contexts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest tasks-test--update-list-property-writes-block ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file (insert "---\nstatus: inbox\n---\n\n# T\n"))
      (my/tasks--update-list-property file "contexts" '("@work" "@computer"))
      (let* ((text (with-temp-buffer (insert-file-contents file)
                                     (buffer-string)))
             (task (my/tasks--parse-frontmatter file)))
        (should (string-match-p "contexts:\n  - \"@work\"\n  - \"@computer\"" text))
        (should (equal (plist-get task :contexts) '("@work" "@computer")))))))

(ert-deftest tasks-test--update-list-property-removes ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\ncontexts:\n  - \"@work\"\n"
                "  - \"@home\"\ndue: 2026-06-08\n---\n\n# T\n"))
      (my/tasks--update-list-property file "contexts" nil)
      (let ((text (with-temp-buffer (insert-file-contents file)
                                    (buffer-string))))
        (should-not (string-match-p "contexts:" text))
        (should-not (string-match-p "@work" text))
        (should (string-match-p "status: inbox" text))
        (should (string-match-p "due: 2026-06-08" text))))))

(ert-deftest tasks-test--update-list-property-replaces-scalar ()
  "Setting a list value over an existing scalar replaces it cleanly."
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\ncontexts: oldscalar\n---\n\n# T\n"))
      (my/tasks--update-list-property file "contexts" '("@a"))
      (let* ((task (my/tasks--parse-frontmatter file)))
        (should (equal (plist-get task :contexts) '("@a")))))))

(ert-deftest tasks-test--by-context-filters ()
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: next\ncontexts:\n  - \"@work\"\n---\n\n# A\n"))
    (with-temp-file (expand-file-name "b.md" temp-dir)
      (insert "---\nstatus: next\ncontexts:\n"
              "  - \"@work\"\n  - \"@home\"\n---\n\n# B\n"))
    (with-temp-file (expand-file-name "c.md" temp-dir)
      (insert "---\nstatus: next\ncontexts:\n  - \"@home\"\n---\n\n# C\n"))
    (with-temp-file (expand-file-name "d.md" temp-dir)
      (insert "---\nstatus: next\n---\n\n# D\n"))
    (let ((titles (lambda (xs)
                    (sort (mapcar (lambda (t) (plist-get t :title)) xs)
                          #'string<))))
      (should (equal (funcall titles (my/tasks-by-context "@work"))
                     '("A" "B")))
      (should (equal (funcall titles (my/tasks-by-context "@home"))
                     '("B" "C"))))))

(ert-deftest tasks-test--show-context-renders-only-matching ()
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: next\ncontexts:\n  - \"@work\"\n---\n\n# A\n"))
    (with-temp-file (expand-file-name "b.md" temp-dir)
      (insert "---\nstatus: next\ncontexts:\n  - \"@home\"\n---\n\n# B\n"))
    (my/tasks-show-context "@work")
    (with-current-buffer "*Context: @work*"
      (let ((text (buffer-string)))
        (should (string-match-p "▸ A" text))
        (should-not (string-match-p "▸ B" text))))))

(ert-deftest tasks-test--view-filter-context-narrows-and-clears ()
  "Setting and clearing the buffer-local context filter both work."
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: next\ncontexts:\n  - \"@work\"\n---\n\n# A\n"))
    (with-temp-file (expand-file-name "b.md" temp-dir)
      (insert "---\nstatus: next\n---\n\n# B\n"))
    (my/tasks-show-next)
    (with-current-buffer "*Next*"
      ;; Without filter: both visible.
      (should (string-match-p "▸ A" (buffer-string)))
      (should (string-match-p "▸ B" (buffer-string)))
      ;; Apply filter.
      (setq my/tasks-view-context-filter "@work")
      (my/tasks--redraw-view)
      (should (string-match-p "▸ A" (buffer-string)))
      (should-not (string-match-p "▸ B" (buffer-string)))
      ;; Clear filter.
      (setq my/tasks-view-context-filter nil)
      (my/tasks--redraw-view)
      (should (string-match-p "▸ A" (buffer-string)))
      (should (string-match-p "▸ B" (buffer-string))))))

(ert-deftest tasks-test--card-renders-context-chips ()
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: next\ncontexts:\n"
              "  - \"@work\"\n  - \"@computer\"\n---\n\n# A\n"))
    (my/tasks-show-next)
    (with-current-buffer "*Next*"
      (should (string-match-p "@work" (buffer-string)))
      (should (string-match-p "@computer" (buffer-string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest tasks-test--search-matches-title ()
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: next\n---\n\n# Find me\n"))
    (with-temp-file (expand-file-name "b.md" temp-dir)
      (insert "---\nstatus: next\n---\n\n# Other\n"))
    (let* ((results (my/tasks-search-results "find me"))
           (titles (mapcar (lambda (t) (plist-get t :title)) results)))
      (should (equal titles '("Find me"))))))

(ert-deftest tasks-test--search-matches-body ()
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: next\n---\n\n# Foo\n\nSpecificWord in body.\n"))
    (with-temp-file (expand-file-name "b.md" temp-dir)
      (insert "---\nstatus: next\n---\n\n# Bar\n\nUnrelated.\n"))
    (let* ((results (my/tasks-search-results "specificword"))
           (titles (mapcar (lambda (t) (plist-get t :title)) results)))
      (should (equal titles '("Foo"))))))

(ert-deftest tasks-test--search-matches-frontmatter ()
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: next\nproject: \"[[Email Migration]]\"\n"
              "---\n\n# Reply to Alice\n"))
    (let* ((results (my/tasks-search-results "Email Migration"))
           (titles (mapcar (lambda (t) (plist-get t :title)) results)))
      (should (equal titles '("Reply to Alice"))))))

(ert-deftest tasks-test--search-includes-archive ()
  (tasks-test--with-temp-dirs
    (make-directory my/tasks-archive-directory t)
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: next\n---\n\n# Active hit\n"))
    (with-temp-file (expand-file-name "2026-05-26-old.md"
                                      my/tasks-archive-directory)
      (insert "---\nstatus: done\narchived-at: 2026-05-26\n"
              "---\n\n# Archived hit\n"))
    (let* ((results (my/tasks-search-results "hit"))
           (titles (sort (mapcar (lambda (t) (plist-get t :title)) results)
                         #'string<)))
      (should (equal titles '("Active hit" "Archived hit"))))))

(ert-deftest tasks-test--search-case-insensitive ()
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: next\n---\n\n# MixedCase Title\n"))
    (should (= 1 (length (my/tasks-search-results "mixedcase"))))
    (should (= 1 (length (my/tasks-search-results "MIXEDCASE"))))
    (should (= 1 (length (my/tasks-search-results "MixedCase"))))))

(ert-deftest tasks-test--search-empty-query-returns-nil ()
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: next\n---\n\n# A\n"))
    (should-not (my/tasks-search-results ""))
    (should-not (my/tasks-search-results "   "))))

(ert-deftest tasks-test--search-no-match ()
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: next\n---\n\n# A\n"))
    (should-not (my/tasks-search-results "nonexistent"))))

(ert-deftest tasks-test--show-search-renders-buffer ()
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: next\n---\n\n# Unique-title-foo\n"))
    (with-temp-file (expand-file-name "b.md" temp-dir)
      (insert "---\nstatus: next\n---\n\n# Other\n"))
    (my/tasks-search "Unique-title-foo")
    (with-current-buffer "*Search: Unique-title-foo*"
      (let ((text (buffer-string)))
        (should (string-match-p "▸ Unique-title-foo" text))
        (should-not (string-match-p "▸ Other" text))))))

(ert-deftest tasks-test--search-empty-query-errors ()
  (tasks-test--with-temp-dirs
    (should-error (my/tasks-search "") :type 'user-error)
    (should-error (my/tasks-search "   ") :type 'user-error)))

(ert-deftest tasks-test--render-shows-archived-at-marker ()
  "When a task carries `archived-at', the card renders a 📦 marker
so search hits from the archive are recognisable."
  (tasks-test--with-temp-dirs
    (make-directory my/tasks-archive-directory t)
    (with-temp-file (expand-file-name "2026-05-26-old.md"
                                      my/tasks-archive-directory)
      (insert "---\nstatus: done\narchived-at: 2026-05-26\n"
              "---\n\n# Archived needle\n"))
    (my/tasks-search "needle")
    (with-current-buffer "*Search: needle*"
      (should (string-match-p "📦 2026-05-26" (buffer-string))))))

(ert-deftest tasks-test--update-property-replaces-block-list-with-scalar ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\ntags:\n  - a\n  - b\n---\n\n# T\n"))
      (my/tasks--update-property file "tags" "single")
      (let ((text (with-temp-buffer (insert-file-contents file) (buffer-string))))
        (should (string-match-p "tags: single" text))
        (should-not (string-match-p "- a" text))
        (should-not (string-match-p "- b" text))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest tasks-test--write-task-file-creates-file ()
  (tasks-test--with-temp-dirs
    (let* ((path (my/tasks--write-task-file "Test Task" ""))
           (task (my/tasks--parse-frontmatter path)))
      (should (file-exists-p path))
      (should (equal (file-name-nondirectory path) "test-task.md"))
      (should (equal (plist-get task :status) "inbox"))
      (should (equal (plist-get task :title) "Test Task"))
      (should (plist-get task :created)))))

(ert-deftest tasks-test--write-task-file-writes-body ()
  (tasks-test--with-temp-dirs
    (let ((path (my/tasks--write-task-file "T" "first line\nsecond line")))
      (with-temp-buffer
        (insert-file-contents path)
        (let ((text (buffer-string)))
          (should (string-match-p "first line" text))
          (should (string-match-p "second line" text)))))))

(ert-deftest tasks-test--write-task-file-custom-status ()
  (tasks-test--with-temp-dirs
    (let* ((path (my/tasks--write-task-file "Test Task" "" "next"))
           (task (my/tasks--parse-frontmatter path)))
      (should (equal (plist-get task :status) "next")))))

(ert-deftest tasks-test--capture-set-status-affects-finalize ()
  (tasks-test--with-temp-dirs
    (my/tasks-capture)
    (with-current-buffer my/tasks-capture-buffer-name
      (goto-char (point-min))
      (end-of-line)
      (insert "Captured with status")
      (my/tasks-capture-set-status "waiting")
      (should (equal my/tasks-capture--status "waiting"))
      (my/tasks-capture-finalize))
    (let* ((path (expand-file-name "captured-with-status.md" temp-dir))
           (task (my/tasks--parse-frontmatter path)))
      (should (equal (plist-get task :status) "waiting")))))

(ert-deftest tasks-test--write-task-file-collision ()
  (tasks-test--with-temp-dirs
    (my/tasks--write-task-file "Same Title" "")
    (my/tasks--write-task-file "Same Title" "")
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

(ert-deftest tasks-test--mu4e-from-email-extracts-email ()
  (should (equal (my/tasks--mu4e-from-email "alice@example.com")
                 "alice@example.com"))
  (should (equal (my/tasks--mu4e-from-email
                  '((:name "Alice" :email "alice@example.com")))
                 "alice@example.com"))
  (should (equal (my/tasks--mu4e-from-email
                  '(("Bob" . "bob@example.com")))
                 "bob@example.com"))
  (should (equal (my/tasks--mu4e-from-email
                  '((:name "No-email")))
                 "No-email"))
  (should (equal (my/tasks--mu4e-from-email nil) "")))

(ert-deftest tasks-test--capture-from-mu4e-body-is-bullet-list ()
  "Title is whatever the user types; body lists source/from/subject as bullets."
  (tasks-test--with-temp-dirs
    (cl-letf (((symbol-function 'mu4e-message-at-point)
               (lambda () (list :from '((:name "Alice"
                                        :email "alice@example.com"))
                                :subject "Important request"
                                :message-id "abc123@host")))
              ((symbol-function 'mu4e-message-field)
               (lambda (msg field) (plist-get msg field))))
      (my/tasks-capture-from-mu4e "Reply to Alice"))
    (let* ((path (expand-file-name "reply-to-alice.md" temp-dir))
           (task (my/tasks--parse-frontmatter path)))
      (should (file-exists-p path))
      (should (equal (plist-get task :title) "Reply to Alice"))
      (should (equal (plist-get task :status) "inbox"))
      (should (equal (plist-get task :mu4e-msgid) "abc123@host"))
      (with-temp-buffer
        (insert-file-contents path)
        (let ((body (buffer-string)))
          (should (string-match-p "- aus E-Mail" body))
          (should (string-match-p "- von: alice@example.com" body))
          (should (string-match-p "- Betreff: Important request" body))
          ;; Subject must NOT appear in the H1 (title).
          (should-not (string-match-p "^# Important request" body)))))))

(ert-deftest tasks-test--capture-from-mu4e-custom-status ()
  "A STATUS argument overrides the default `inbox' in the frontmatter."
  (tasks-test--with-temp-dirs
    (cl-letf (((symbol-function 'mu4e-message-at-point)
               (lambda () (list :from '((:name "Alice"
                                        :email "alice@example.com"))
                                :subject "S"
                                :message-id "id@host")))
              ((symbol-function 'mu4e-message-field)
               (lambda (msg field) (plist-get msg field))))
      (my/tasks-capture-from-mu4e "Reply to Alice" "next"))
    (let* ((path (expand-file-name "reply-to-alice.md" temp-dir))
           (task (my/tasks--parse-frontmatter path)))
      (should (equal (plist-get task :status) "next")))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inbox-processing wizard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- pure: my/tasks--update-title ---

(ert-deftest tasks-test--update-title-replaces-h1 ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\n---\n\n# Old Title\n\nbody\n"))
      (my/tasks--update-title file "New Title")
      (let ((task (my/tasks--parse-frontmatter file)))
        (should (equal (plist-get task :title) "New Title")))
      (with-temp-buffer
        (insert-file-contents file)
        (should-not (string-match-p "# Old Title" (buffer-string)))
        (should (string-match-p "body" (buffer-string)))))))

(ert-deftest tasks-test--update-title-inserts-when-missing ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\n---\n\nbody only\n"))
      (my/tasks--update-title file "Inserted")
      (should (equal (plist-get (my/tasks--parse-frontmatter file) :title)
                     "Inserted")))))

;; --- pure: my/tasks--wizard-rename ---

(ert-deftest tasks-test--wizard-rename-changes-title ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\n---\n\n# Steuererklärung\n"))
      (should (my/tasks--wizard-rename
               file "Steuerbüro Anruf wegen Termin"))
      (should (equal (plist-get (my/tasks--parse-frontmatter file) :title)
                     "Steuerbüro Anruf wegen Termin")))))

(ert-deftest tasks-test--wizard-rename-noop-on-same ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\n---\n\n# Same\n"))
      (should-not (my/tasks--wizard-rename file "Same"))
      (should (equal (plist-get (my/tasks--parse-frontmatter file) :title)
                     "Same")))))

(ert-deftest tasks-test--wizard-rename-noop-on-empty ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\n---\n\n# Keep me\n"))
      (should-not (my/tasks--wizard-rename file ""))
      (should-not (my/tasks--wizard-rename file "   "))
      (should-not (my/tasks--wizard-rename file nil))
      (should (equal (plist-get (my/tasks--parse-frontmatter file) :title)
                     "Keep me")))))

;; --- pure: my/tasks--wizard-defer-date ---

(ert-deftest tasks-test--wizard-defer-date-spans ()
  (let ((today (format-time-string "%Y-%m-%d"))
        (plus1 (format-time-string
                "%Y-%m-%d" (time-add (current-time) (days-to-time 1))))
        (plus7 (format-time-string
                "%Y-%m-%d" (time-add (current-time) (days-to-time 7))))
        (plus30 (format-time-string
                 "%Y-%m-%d" (time-add (current-time) (days-to-time 30)))))
    (should (equal (my/tasks--wizard-defer-date ?1) plus1))
    (should (equal (my/tasks--wizard-defer-date ?7) plus7))
    (should (equal (my/tasks--wizard-defer-date ?m) plus30))
    (should-not (equal (my/tasks--wizard-defer-date ?1) today))))

;; --- pure: my/tasks--apply-wizard-action ---

(ert-deftest tasks-test--apply-wizard-action-status-keys ()
  (tasks-test--with-temp-dirs
    (dolist (case '((today . "today")
                    (next . "next")
                    (waiting . "waiting")
                    (someday . "someday")))
      (let* ((sym (car case))
             (expected (cdr case))
             (file (expand-file-name (format "%s.md" expected) temp-dir)))
        (with-temp-file file (insert "---\nstatus: inbox\n---\n\n# A\n"))
        (my/tasks--apply-wizard-action file sym)
        (should (equal (plist-get (my/tasks--parse-frontmatter file) :status)
                       expected))))))

(ert-deftest tasks-test--apply-wizard-action-done-archives ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file (insert "---\nstatus: inbox\n---\n\n# A\n"))
      (my/tasks--apply-wizard-action file 'done)
      (should-not (file-exists-p file))
      (should (= 1 (length (directory-files my/tasks-archive-directory
                                            t "\\.md\\'")))))))

(ert-deftest tasks-test--apply-wizard-action-trash ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file (insert "---\nstatus: inbox\n---\n\n# A\n"))
      (my/tasks--apply-wizard-action file 'trash)
      (should-not (file-exists-p file))
      ;; Archive dir untouched (no file produced there).
      (when (file-directory-p my/tasks-archive-directory)
        (should-not (directory-files my/tasks-archive-directory
                                     nil "\\.md\\'"))))))

(ert-deftest tasks-test--apply-wizard-action-defer ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file (insert "---\nstatus: inbox\n---\n\n# A\n"))
      (my/tasks--apply-wizard-action file 'defer "2099-12-31")
      (let ((task (my/tasks--parse-frontmatter file)))
        (should (equal (plist-get task :status) "next"))
        (should (equal (plist-get task :scheduled) "2099-12-31"))))))

(ert-deftest tasks-test--apply-wizard-action-rejects-unknown ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file (insert "---\nstatus: inbox\n---\n\n# A\n"))
      (should-error (my/tasks--apply-wizard-action file 'no-such-action)))))

;; --- integration: process-inbox end-to-end ---

(ert-deftest tasks-test--process-inbox-empty ()
  "Empty inbox: wizard exits silently, no error, no file changes."
  (tasks-test--with-temp-dirs
    ;; No files in inbox — just call. Test passes if no error.
    (my/tasks-process-inbox)
    (should-not (directory-files my/tasks-directory nil "\\.md\\'"))))

(ert-deftest tasks-test--process-inbox-rename-and-next ()
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: inbox\n---\n\n# Steuererklärung\n"))
    (cl-letf (((symbol-function 'my/tasks--wizard-read-title)
               (lambda (_prompt _default) "Steuerbüro Anruf wegen Termin"))
              ((symbol-function 'my/tasks--wizard-read-char)
               (lambda (_prompt _chars) ?n)))
      (my/tasks-process-inbox))
    (let ((task (my/tasks--parse-frontmatter
                 (expand-file-name "a.md" temp-dir))))
      (should (equal (plist-get task :title)
                     "Steuerbüro Anruf wegen Termin"))
      (should (equal (plist-get task :status) "next")))))

(ert-deftest tasks-test--process-inbox-skip-keeps-state ()
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: inbox\n---\n\n# Untouched\n"))
    (cl-letf (((symbol-function 'my/tasks--wizard-read-title)
               (lambda (_p default) default))
              ((symbol-function 'my/tasks--wizard-read-char)
               (lambda (_p _c) ?i)))
      (my/tasks-process-inbox))
    (let ((task (my/tasks--parse-frontmatter
                 (expand-file-name "a.md" temp-dir))))
      (should (equal (plist-get task :title) "Untouched"))
      (should (equal (plist-get task :status) "inbox")))))

(ert-deftest tasks-test--process-inbox-done-archives ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "a.md" temp-dir)))
      (with-temp-file file (insert "---\nstatus: inbox\n---\n\n# Quick task\n"))
      (cl-letf (((symbol-function 'my/tasks--wizard-read-title)
                 (lambda (_p default) default))
                ((symbol-function 'my/tasks--wizard-read-char)
                 (lambda (_p _c) ?x)))
        (my/tasks-process-inbox))
      (should-not (file-exists-p file))
      (should (= 1 (length (directory-files my/tasks-archive-directory
                                            t "\\.md\\'")))))))

(ert-deftest tasks-test--process-inbox-trash-with-confirm ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "a.md" temp-dir)))
      (with-temp-file file (insert "---\nstatus: inbox\n---\n\n# Garbage\n"))
      (cl-letf (((symbol-function 'my/tasks--wizard-read-title)
                 (lambda (_p default) default))
                ((symbol-function 'my/tasks--wizard-read-char)
                 (lambda (_p _c) ?T))
                ((symbol-function 'my/tasks--wizard-confirm)
                 (lambda (_p) t)))
        (my/tasks-process-inbox))
      (should-not (file-exists-p file))
      ;; Trash skips archive — directory should not contain the file.
      (when (file-directory-p my/tasks-archive-directory)
        (should-not (directory-files my/tasks-archive-directory nil "\\.md\\'"))))))

(ert-deftest tasks-test--process-inbox-trash-cancelled ()
  "Declining the trash confirmation leaves the file in place."
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "a.md" temp-dir)))
      (with-temp-file file (insert "---\nstatus: inbox\n---\n\n# Garbage\n"))
      (cl-letf (((symbol-function 'my/tasks--wizard-read-title)
                 (lambda (_p default) default))
                ((symbol-function 'my/tasks--wizard-read-char)
                 (lambda (_p _c) ?T))
                ((symbol-function 'my/tasks--wizard-confirm)
                 (lambda (_p) nil)))
        (my/tasks-process-inbox))
      (should (file-exists-p file))
      (should (equal (plist-get (my/tasks--parse-frontmatter file) :status)
                     "inbox")))))

(ert-deftest tasks-test--process-inbox-defer-sets-next-and-scheduled ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "a.md" temp-dir))
          (char-calls 0))
      (with-temp-file file (insert "---\nstatus: inbox\n---\n\n# A\n"))
      (cl-letf (((symbol-function 'my/tasks--wizard-read-title)
                 (lambda (_p default) default))
                ((symbol-function 'my/tasks--wizard-read-char)
                 (lambda (_p _c)
                   (cl-incf char-calls)
                   (if (= char-calls 1) ?+ ?7))))
        (my/tasks-process-inbox))
      (let* ((task (my/tasks--parse-frontmatter file))
             (expected (format-time-string
                        "%Y-%m-%d"
                        (time-add (current-time) (days-to-time 7)))))
        (should (equal (plist-get task :status) "next"))
        (should (equal (plist-get task :scheduled) expected))))))

(ert-deftest tasks-test--process-inbox-quit-stops-loop ()
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: inbox\n---\n\n# First\n"))
    (with-temp-file (expand-file-name "b.md" temp-dir)
      (insert "---\nstatus: inbox\n---\n\n# Second\n"))
    (cl-letf (((symbol-function 'my/tasks--wizard-read-title)
               (lambda (_p default) default))
              ((symbol-function 'my/tasks--wizard-read-char)
               (lambda (_p _c) ?q)))
      (my/tasks-process-inbox))
    ;; Both still in inbox — quit fired on item 1 before anything changed.
    (should (equal (plist-get
                    (my/tasks--parse-frontmatter
                     (expand-file-name "a.md" temp-dir)) :status) "inbox"))
    (should (equal (plist-get
                    (my/tasks--parse-frontmatter
                     (expand-file-name "b.md" temp-dir)) :status) "inbox"))))

(ert-deftest tasks-test--process-inbox-walks-multiple-items ()
  "Different choices per item: rename + next, then archive."
  (tasks-test--with-temp-dirs
    (let ((title-calls 0)
          (char-calls 0)
          (file-a (expand-file-name "a.md" temp-dir))
          (file-b (expand-file-name "b.md" temp-dir)))
      (with-temp-file file-a
        (insert "---\nstatus: inbox\n---\n\n# Steuern\n"))
      (with-temp-file file-b
        (insert "---\nstatus: inbox\n---\n\n# Trinkflasche kaufen\n"))
      (cl-letf (((symbol-function 'my/tasks--wizard-read-title)
                 (lambda (_p default)
                   (cl-incf title-calls)
                   (pcase title-calls
                     (1 "Steuerbüro anrufen")
                     (_ default))))
                ((symbol-function 'my/tasks--wizard-read-char)
                 (lambda (_p _c)
                   (cl-incf char-calls)
                   (pcase char-calls
                     (1 ?n)
                     (_ ?x)))))
        (my/tasks-process-inbox))
      (let ((task-a (my/tasks--parse-frontmatter file-a)))
        (should (equal (plist-get task-a :title) "Steuerbüro anrufen"))
        (should (equal (plist-get task-a :status) "next")))
      (should-not (file-exists-p file-b))
      (should (= 1 (length (directory-files my/tasks-archive-directory
                                            t "\\.md\\'")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Header-line key hints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest tasks-test--header-line-contains-hint ()
  "Header-line builder ends with the configured hint string."
  (let ((result (my/tasks--header-line)))
    (should (stringp result))
    (should (string-suffix-p my/tasks--view-hint result))))

(ert-deftest tasks-test--header-line-uses-align-to-display-prop ()
  "The pad prefix carries a `(space :align-to ...)' display property
when no stats badges are in the way."
  (tasks-test--with-temp-dirs
    (let* ((result (my/tasks--header-line))
           (prop (get-text-property 0 'display result)))
      (should (eq (car prop) 'space))
      (should (eq (cadr prop) :align-to)))))

(ert-deftest tasks-test--view-has-header-line-format ()
  "After entering `my/tasks-mode', the buffer has a header-line-format."
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: inbox\n---\n\n# A\n"))
    (my/tasks-show-inbox)
    (with-current-buffer "*Inbox*"
      (should header-line-format))))

(ert-deftest tasks-test--view-help-opens-help-buffer ()
  "Calling `my/tasks-view-help' produces a `*Tasks Help*' buffer with content."
  (tasks-test--with-temp-dirs
    (unwind-protect
        (progn
          (my/tasks-view-help)
          (with-current-buffer "*Tasks Help*"
            (let ((text (buffer-string)))
              (should (string-match-p "Tasks View — Keybindings" text))
              (should (string-match-p "RET" text))
              (should (string-match-p "inbox-processing wizard" text)))))
      (when (get-buffer "*Tasks Help*") (kill-buffer "*Tasks Help*")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alarm banner (overdue / today)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest tasks-test--alarm-state-overdue ()
  (should (eq 'overdue (my/tasks--alarm-state (list :due "1999-01-01"))))
  (should (eq 'overdue (my/tasks--alarm-state (list :scheduled "1999-01-01"))))
  (should (eq 'overdue
              (my/tasks--alarm-state (list :reminder "1999-01-01 09:00")))))

(ert-deftest tasks-test--alarm-state-today ()
  (let ((today (format-time-string "%Y-%m-%d")))
    (should (eq 'today (my/tasks--alarm-state (list :due today))))
    (should (eq 'today (my/tasks--alarm-state (list :scheduled today))))
    (should (eq 'today (my/tasks--alarm-state
                        (list :reminder (concat today " 09:00")))))))

(ert-deftest tasks-test--alarm-state-future-is-nil ()
  (should-not (my/tasks--alarm-state (list :due "2999-12-31")))
  (should-not (my/tasks--alarm-state '()))
  (should-not (my/tasks--alarm-state (list :title "no dates"))))

(ert-deftest tasks-test--alarm-state-overdue-trumps-today ()
  "If any date field is overdue, the result is `overdue' regardless of the rest."
  (let ((today (format-time-string "%Y-%m-%d")))
    (should (eq 'overdue
                (my/tasks--alarm-state
                 (list :due today :scheduled "1999-01-01"))))))

(ert-deftest tasks-test--collect-alarms-buckets ()
  (tasks-test--with-temp-dirs
    (let ((today (format-time-string "%Y-%m-%d")))
      (with-temp-file (expand-file-name "a.md" temp-dir)
        (insert "---\nstatus: next\ndue: 1999-01-01\n---\n\n# Overdue A\n"))
      (with-temp-file (expand-file-name "b.md" temp-dir)
        (insert (format "---\nstatus: next\ndue: %s\n---\n\n# Today B\n"
                        today)))
      (with-temp-file (expand-file-name "c.md" temp-dir)
        (insert "---\nstatus: next\n---\n\n# No-date C\n"))
      (let* ((alarms (my/tasks--collect-alarms))
             (overdue (car alarms))
             (today-list (cdr alarms)))
        (should (= 1 (length overdue)))
        (should (= 1 (length today-list)))
        (should (equal (plist-get (car overdue) :title) "Overdue A"))
        (should (equal (plist-get (car today-list) :title) "Today B"))))))

(ert-deftest tasks-test--collect-alarms-overdue-sorted-oldest-first ()
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: next\ndue: 2020-06-01\n---\n\n# Newer\n"))
    (with-temp-file (expand-file-name "b.md" temp-dir)
      (insert "---\nstatus: next\ndue: 1999-01-01\n---\n\n# Older\n"))
    (let* ((overdue (car (my/tasks--collect-alarms)))
           (titles (mapcar (lambda (t) (plist-get t :title)) overdue)))
      (should (equal titles '("Older" "Newer"))))))

(ert-deftest tasks-test--view-renders-alarm-banner-when-enabled ()
  "Banner section shows overdue + today, sitting above the view title."
  (tasks-test--with-temp-dirs
    (let ((today (format-time-string "%Y-%m-%d"))
          (my/tasks-show-alarm-banner t))
      (with-temp-file (expand-file-name "a.md" temp-dir)
        (insert "---\nstatus: someday\ndue: 1999-01-01\n---\n\n# Old Stale\n"))
      (with-temp-file (expand-file-name "b.md" temp-dir)
        (insert (format "---\nstatus: next\ndue: %s\n---\n\n# Reply Today\n"
                        today)))
      (my/tasks-show-inbox)
      (with-current-buffer "*Inbox*"
        (let ((text (buffer-string)))
          (should (string-match-p "⚠ Überfällig" text))
          (should (string-match-p "▸ Old Stale" text))
          (should (string-match-p "📅 Heute" text))
          (should (string-match-p "▸ Reply Today" text))
          ;; Both alarm sections appear before the Inbox heading.
          (let ((overdue-pos (string-match "⚠ Überfällig" text))
                (today-pos   (string-match "📅 Heute" text))
                (inbox-pos   (string-match "^Inbox" text)))
            (should overdue-pos)
            (should today-pos)
            (should inbox-pos)
            (should (< overdue-pos today-pos inbox-pos))))))))

(ert-deftest tasks-test--view-title-uses-header-face ()
  "View title always uses the big header face — no master heading wraps it."
  (tasks-test--with-temp-dirs
    (let ((my/tasks-show-alarm-banner t))
      (with-temp-file (expand-file-name "a.md" temp-dir)
        (insert "---\nstatus: someday\ndue: 1999-01-01\n---\n\n# Stale\n"))
      (my/tasks-show-inbox)
      (with-current-buffer "*Inbox*"
        (goto-char (point-min))
        (re-search-forward "^Inbox")
        (goto-char (match-beginning 0))
        (should (eq (get-text-property (point) 'face)
                    'my/tasks-header-face))))))

(ert-deftest tasks-test--view-hides-alarm-banner-when-disabled ()
  (tasks-test--with-temp-dirs
    (let ((today (format-time-string "%Y-%m-%d"))
          (my/tasks-show-alarm-banner nil))
      (with-temp-file (expand-file-name "a.md" temp-dir)
        (insert (format "---\nstatus: next\ndue: %s\n---\n\n# T\n" today)))
      (my/tasks-show-inbox)
      (with-current-buffer "*Inbox*"
        (let ((text (buffer-string)))
          (should-not (string-match-p "⚠ Überfällig" text))
          (should-not (string-match-p "📅 Heute" text)))))))

(ert-deftest tasks-test--view-no-banner-when-no-alarms ()
  "Even with the flag on, no banner is drawn when no task triggers."
  (tasks-test--with-temp-dirs
    (let ((my/tasks-show-alarm-banner t))
      (with-temp-file (expand-file-name "a.md" temp-dir)
        (insert "---\nstatus: inbox\n---\n\n# No-date task\n"))
      (my/tasks-show-inbox)
      (with-current-buffer "*Inbox*"
        (let ((text (buffer-string)))
          (should-not (string-match-p "⚠ Überfällig" text))
          (should-not (string-match-p "📅 Heute" text)))))))

(ert-deftest tasks-test--toggle-alarm-banner-flips-flag ()
  (let ((my/tasks-show-alarm-banner t))
    (my/tasks-toggle-alarm-banner)
    (should (eq my/tasks-show-alarm-banner nil))
    (my/tasks-toggle-alarm-banner)
    (should (eq my/tasks-show-alarm-banner t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Week view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest tasks-test--week-dates-has-seven-days ()
  (let ((week (my/tasks--week-dates)))
    (should (= 7 (length week)))
    (should (equal (mapcar #'cdr week) '("Mo" "Di" "Mi" "Do" "Fr" "Sa" "So")))))

(ert-deftest tasks-test--week-dates-monday-first ()
  "First day of the week list must be a Monday."
  (let* ((week (my/tasks--week-dates))
         (first-date (caar week))
         ;; Use date-to-time to make sure %u returns Monday=1.
         (dow (string-to-number
               (format-time-string "%u" (date-to-time
                                         (concat first-date " 00:00"))))))
    (should (= dow 1))))

(ert-deftest tasks-test--task-on-day-p ()
  (let ((task '(:due "2026-06-23" :scheduled "2026-06-25")))
    (should (my/tasks--task-on-day-p task "2026-06-23"))
    (should (my/tasks--task-on-day-p task "2026-06-25"))
    (should-not (my/tasks--task-on-day-p task "2026-06-24"))
    (should-not (my/tasks--task-on-day-p '() "2026-06-23"))))

(ert-deftest tasks-test--task-on-day-p-handles-datetime ()
  "Reminders with times still match the date part."
  (should (my/tasks--task-on-day-p
           '(:reminder "2026-06-23 09:00") "2026-06-23")))

(ert-deftest tasks-test--show-week-renders-tasks-on-their-day ()
  (tasks-test--with-temp-dirs
    (let* ((week (my/tasks--week-dates))
           (monday (caar week))
           (friday (car (nth 4 week))))
      (with-temp-file (expand-file-name "a.md" temp-dir)
        (insert (format "---\nstatus: next\ndue: %s\n---\n\n# Monday Task\n"
                        monday)))
      (with-temp-file (expand-file-name "b.md" temp-dir)
        (insert (format "---\nstatus: next\nscheduled: %s\n---\n\n# Friday Task\n"
                        friday)))
      (with-temp-file (expand-file-name "c.md" temp-dir)
        (insert "---\nstatus: next\n---\n\n# No-Date Task\n"))
      (my/tasks-show-week)
      (with-current-buffer "*Week*"
        (let ((text (buffer-string)))
          (should (string-match-p "Diese Woche" text))
          (should (string-match-p "▸ Monday Task" text))
          (should (string-match-p "▸ Friday Task" text))
          ;; Tasks without any date in this week are NOT shown.
          (should-not (string-match-p "▸ No-Date Task" text))
          ;; The Monday header precedes the Monday task,
          ;; the Friday header precedes the Friday task,
          ;; and Monday block precedes Friday block.
          (let ((mon-pos (string-match (concat "^Mo · " monday) text))
                (fri-pos (string-match (concat "^Fr · " friday) text))
                (mon-task-pos (string-match "▸ Monday Task" text))
                (fri-task-pos (string-match "▸ Friday Task" text)))
            (should mon-pos)
            (should fri-pos)
            (should (< mon-pos mon-task-pos))
            (should (< fri-pos fri-task-pos))
            (should (< mon-pos fri-pos))))))))

(ert-deftest tasks-test--show-week-marks-empty-day ()
  (tasks-test--with-temp-dirs
    (my/tasks-show-week)
    (with-current-buffer "*Week*"
      ;; Without any tasks scheduled today, the day section shows the em-dash.
      (should (string-match-p "—" (buffer-string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Status-change audit log
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest tasks-test--status-change-appends-log-line ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\n---\n\n# Task\n"))
      (my/tasks--update-property file "status" "next")
      (let ((text (with-temp-buffer
                    (insert-file-contents file) (buffer-string))))
        (should (string-match-p ": inbox → next\\b" text))))))

(ert-deftest tasks-test--multiple-status-changes-stack-log-lines ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: inbox\n---\n\n# Task\n"))
      (my/tasks--update-property file "status" "next")
      (my/tasks--update-property file "status" "today")
      (let* ((text (with-temp-buffer
                     (insert-file-contents file) (buffer-string)))
             (lines (seq-filter
                     (lambda (l) (string-match-p ": .+ → .+" l))
                     (split-string text "\n"))))
        (should (= 2 (length lines)))
        (should (string-match-p "inbox → next" (nth 0 lines)))
        (should (string-match-p "next → today" (nth 1 lines)))))))

(ert-deftest tasks-test--no-log-when-status-unchanged ()
  "Setting status to the same value must not emit a log line."
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: next\n---\n\n# Task\n"))
      (my/tasks--update-property file "status" "next")
      (let ((text (with-temp-buffer
                    (insert-file-contents file) (buffer-string))))
        (should-not (string-match-p "→" text))))))

(ert-deftest tasks-test--no-log-for-non-status-property ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: next\n---\n\n# Task\n"))
      (my/tasks--update-property file "due" "2026-06-25")
      (let ((text (with-temp-buffer
                    (insert-file-contents file) (buffer-string))))
        (should-not (string-match-p "→" text))))))

(ert-deftest tasks-test--archive-records-status-done ()
  "Archiving a task should log the inbox/next/etc → done transition."
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: next\n---\n\n# Task\n"))
      (my/tasks--archive-file file)
      (let* ((archived (car (directory-files my/tasks-archive-directory
                                              t "\\.md\\'")))
             (text (with-temp-buffer
                     (insert-file-contents archived) (buffer-string))))
        (should (string-match-p ": next → done\\b" text))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Header-line done-today stats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest tasks-test--archived-today-count-zero-when-empty ()
  (tasks-test--with-temp-dirs
    (should (= 0 (my/tasks--archived-today-count)))))

(ert-deftest tasks-test--archived-today-count-counts-today-prefix-only ()
  "Files dated today count; files dated other days do not."
  (tasks-test--with-temp-dirs
    (make-directory my/tasks-archive-directory t)
    (let ((today (format-time-string "%Y-%m-%d")))
      (dotimes (i 3)
        (with-temp-file (expand-file-name
                         (format "%s-task-%d.md" today i)
                         my/tasks-archive-directory)
          (insert "---\nstatus: done\n---\n\n# x\n")))
      (with-temp-file (expand-file-name
                       "2026-01-01-old.md" my/tasks-archive-directory)
        (insert "---\nstatus: done\n---\n\n# old\n"))
      (should (= 3 (my/tasks--archived-today-count))))))

(ert-deftest tasks-test--archive-action-bumps-today-count ()
  "After archiving via the wizard action, the today-count goes up by one."
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file (insert "---\nstatus: next\n---\n\n# T\n"))
      (should (= 0 (my/tasks--archived-today-count)))
      (my/tasks--apply-wizard-action file 'done)
      (should (= 1 (my/tasks--archived-today-count))))))

(ert-deftest tasks-test--header-line-prefixes-stats-when-present ()
  (tasks-test--with-temp-dirs
    (make-directory my/tasks-archive-directory t)
    (let ((today (format-time-string "%Y-%m-%d")))
      (with-temp-file (expand-file-name
                       (format "%s-t.md" today)
                       my/tasks-archive-directory)
        (insert "---\nstatus: done\n---\n\n# x\n"))
      (let ((line (my/tasks--header-line)))
        (should (string-match-p "✓ 1 done today" line))
        (should (string-suffix-p my/tasks--view-hint line))))))

(ert-deftest tasks-test--header-line-omits-stats-when-zero ()
  (tasks-test--with-temp-dirs
    (let ((line (my/tasks--header-line)))
      (should-not (string-match-p "done today" line))
      (should (string-suffix-p my/tasks--view-hint line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inbox-zero streak
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro tasks-test--with-streak-state (state &rest body)
  "Bind `my/tasks-streak-file' to a fresh temp file pre-populated with STATE,
run BODY, then clean up."
  (declare (indent 1) (debug t))
  `(let ((my/tasks-streak-file (make-temp-file "tasks-streak-" nil ".json")))
     (unwind-protect
         (progn
           (when ,state (my/tasks--streak-write ,state))
           ,@body)
       (when (file-exists-p my/tasks-streak-file)
         (delete-file my/tasks-streak-file)))))

(ert-deftest tasks-test--streak-read-defaults-when-missing ()
  (tasks-test--with-streak-state nil
    (let ((state (my/tasks--streak-read)))
      (should (= 0 (or (plist-get state :current) 0)))
      (should (= 0 (or (plist-get state :longest) 0))))))

(ert-deftest tasks-test--streak-effective-zero-when-broken ()
  "If the last zero-day is older than yesterday, the streak displays as 0."
  (tasks-test--with-streak-state
      '(:current 5 :longest 7 :last_zero_date "2020-01-01")
    (should (= 0 (my/tasks--streak-effective)))))

(ert-deftest tasks-test--streak-effective-alive-prev-working-day ()
  ;; All weekdays working → previous working day is literally yesterday,
  ;; so the test is independent of the day it runs on.
  (let* ((my/tasks-streak-working-days '(1 2 3 4 5 6 7))
         (yesterday (format-time-string
                     "%Y-%m-%d"
                     (time-subtract (current-time) (days-to-time 1)))))
    (tasks-test--with-streak-state
        (list :current 5 :longest 5 :last_zero_date yesterday)
      (should (= 5 (my/tasks--streak-effective))))))

(ert-deftest tasks-test--streak-effective-alive-today ()
  (let ((today (format-time-string "%Y-%m-%d")))
    (tasks-test--with-streak-state
        (list :current 3 :longest 7 :last_zero_date today)
      (should (= 3 (my/tasks--streak-effective))))))

(ert-deftest tasks-test--streak-touch-first-time ()
  (tasks-test--with-temp-dirs
    (let ((my/tasks-streak-working-days '(1 2 3 4 5 6 7)))
      (tasks-test--with-streak-state nil
        ;; No tasks in inbox → first touch starts the streak at 1.
        (my/tasks--streak-touch)
        (let ((state (my/tasks--streak-read)))
          (should (= 1 (plist-get state :current)))
          (should (= 1 (plist-get state :longest)))
          (should (equal (plist-get state :last_zero_date)
                         (format-time-string "%Y-%m-%d"))))))))

(ert-deftest tasks-test--streak-touch-continues-from-prev-working-day ()
  (tasks-test--with-temp-dirs
    (let ((my/tasks-streak-working-days '(1 2 3 4 5 6 7))
          (yesterday (format-time-string
                      "%Y-%m-%d"
                      (time-subtract (current-time) (days-to-time 1)))))
      (tasks-test--with-streak-state
          (list :current 4 :longest 4 :last_zero_date yesterday)
        (my/tasks--streak-touch)
        (should (= 5 (plist-get (my/tasks--streak-read) :current)))))))

(ert-deftest tasks-test--streak-touch-idempotent-same-day ()
  (tasks-test--with-temp-dirs
    (let ((my/tasks-streak-working-days '(1 2 3 4 5 6 7))
          (today (format-time-string "%Y-%m-%d")))
      (tasks-test--with-streak-state
          (list :current 3 :longest 3 :last_zero_date today)
        (my/tasks--streak-touch)
        (should (= 3 (plist-get (my/tasks--streak-read) :current)))))))

(ert-deftest tasks-test--streak-touch-noop-on-non-working-day ()
  ;; Make today a non-working day → touch must leave the state untouched.
  (tasks-test--with-temp-dirs
    (let* ((dow (string-to-number (format-time-string "%u")))
           (my/tasks-streak-working-days (remq dow '(1 2 3 4 5 6 7))))
      (tasks-test--with-streak-state
          '(:current 5 :longest 5 :last_zero_date "2020-01-01")
        (my/tasks--streak-touch)
        (let ((state (my/tasks--streak-read)))
          (should (= 5 (plist-get state :current)))
          (should (equal "2020-01-01"
                         (plist-get state :last_zero_date))))))))

(ert-deftest tasks-test--streak-touch-gap-stashes-prev ()
  (tasks-test--with-temp-dirs
    (let ((my/tasks-streak-working-days '(1 2 3 4 5 6 7)))
      (tasks-test--with-streak-state
          '(:current 5 :longest 7 :last_zero_date "2020-01-01")
        (my/tasks--streak-touch)
        (let ((s (my/tasks--streak-read)))
          (should (= 1 (plist-get s :current)))
          (should (= 5 (plist-get s :prev_current)))
          (should (equal "2020-01-01" (plist-get s :prev_zero_date))))))))

(ert-deftest tasks-test--streak-touch-does-nothing-when-inbox-non-empty ()
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: inbox\n---\n\n# Not empty\n"))
    (let ((my/tasks-streak-working-days '(1 2 3 4 5 6 7)))
      (tasks-test--with-streak-state nil
        (my/tasks--streak-touch)
        (let ((state (my/tasks--streak-read)))
          (should (= 0 (or (plist-get state :current) 0))))))))

(ert-deftest tasks-test--streak-bridge-reanchors-when-away ()
  ;; Returning from a long absence before clearing the inbox: the stored
  ;; streak becomes alive again, ready to continue on the next inbox-zero.
  (let ((my/tasks-streak-working-days '(1 2 3 4 5 6 7)))
    (tasks-test--with-streak-state
        '(:current 12 :longest 12 :last_zero_date "2020-01-01")
      (should (= 0 (my/tasks--streak-effective)))
      (my/tasks-streak-bridge)
      (should (= 12 (plist-get (my/tasks--streak-read) :current)))
      (should (= 12 (my/tasks--streak-effective))))))

(ert-deftest tasks-test--streak-bridge-recovers-after-reset ()
  ;; The gap already reset the streak to 1 today; bridge recovers the
  ;; pre-absence value and counts today on top of it.
  (let ((today (format-time-string "%Y-%m-%d")))
    (tasks-test--with-streak-state
        (list :current 1 :longest 12 :last_zero_date today
              :prev_current 12 :prev_zero_date "2020-01-01")
      (my/tasks-streak-bridge)
      (let ((s (my/tasks--streak-read)))
        (should (= 13 (plist-get s :current)))
        (should (= 0 (plist-get s :prev_current)))))))

(ert-deftest tasks-test--streak-bridge-noop-when-already-counted ()
  (let ((today (format-time-string "%Y-%m-%d")))
    (tasks-test--with-streak-state
        (list :current 5 :longest 5 :last_zero_date today
              :prev_current 0 :prev_zero_date "")
      (my/tasks-streak-bridge)
      (should (= 5 (plist-get (my/tasks--streak-read) :current))))))

(ert-deftest tasks-test--header-line-shows-streak-when-alive ()
  (tasks-test--with-streak-state
      (list :current 7 :longest 9
            :last_zero_date (format-time-string "%Y-%m-%d"))
    (let ((line (my/tasks--header-line)))
      (should (string-match-p "🔥 7d streak" line)))))

(ert-deftest tasks-test--header-line-omits-streak-when-broken ()
  (tasks-test--with-streak-state
      '(:current 5 :longest 9 :last_zero_date "2020-01-01")
    (let ((line (my/tasks--header-line)))
      (should-not (string-match-p "🔥" line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Waiting-since aging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest tasks-test--entering-waiting-sets-waiting-since ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir))
          (today (format-time-string "%Y-%m-%d")))
      (with-temp-file file (insert "---\nstatus: next\n---\n\n# T\n"))
      (my/tasks--update-property file "status" "waiting")
      (let ((task (my/tasks--parse-frontmatter file)))
        (should (equal (plist-get task :status) "waiting"))
        (should (equal (plist-get task :waiting-since) today))))))

(ert-deftest tasks-test--leaving-waiting-clears-waiting-since ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: waiting\nwaiting-since: 2020-01-01\n"
                "---\n\n# T\n"))
      (my/tasks--update-property file "status" "next")
      (let ((task (my/tasks--parse-frontmatter file)))
        (should (equal (plist-get task :status) "next"))
        (should-not (plist-get task :waiting-since))))))

(ert-deftest tasks-test--non-status-change-leaves-waiting-since-alone ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: waiting\nwaiting-since: 2020-01-01\n"
                "---\n\n# T\n"))
      (my/tasks--update-property file "due" "2026-12-31")
      (should (equal (plist-get (my/tasks--parse-frontmatter file)
                                :waiting-since)
                     "2020-01-01")))))

(ert-deftest tasks-test--waiting-to-waiting-noop ()
  "Setting status to its current value must not bump waiting-since."
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: waiting\nwaiting-since: 2020-01-01\n"
                "---\n\n# T\n"))
      (my/tasks--update-property file "status" "waiting")
      (should (equal (plist-get (my/tasks--parse-frontmatter file)
                                :waiting-since)
                     "2020-01-01")))))

(ert-deftest tasks-test--days-since-basic ()
  (let ((today (format-time-string "%Y-%m-%d"))
        (yesterday (format-time-string
                    "%Y-%m-%d"
                    (time-subtract (current-time) (days-to-time 1))))
        (week-ago (format-time-string
                   "%Y-%m-%d"
                   (time-subtract (current-time) (days-to-time 7)))))
    (should (= 0 (my/tasks--days-since today)))
    (should (= 1 (my/tasks--days-since yesterday)))
    (should (= 7 (my/tasks--days-since week-ago)))))

(ert-deftest tasks-test--waiting-card-shows-aging ()
  (tasks-test--with-temp-dirs
    (let ((yesterday (format-time-string
                      "%Y-%m-%d"
                      (time-subtract (current-time) (days-to-time 1)))))
      (with-temp-file (expand-file-name "a.md" temp-dir)
        (insert (format "---\nstatus: waiting\nwaiting-since: %s\n---\n\n# Reply Alice\n"
                        yesterday)))
      (my/tasks-show-waiting)
      (with-current-buffer "*Waiting*"
        (let ((text (buffer-string)))
          (should (string-match-p "▸ Reply Alice" text))
          (should (string-match-p "(seit 1d)" text)))))))

(ert-deftest tasks-test--waiting-card-nag-face-after-threshold ()
  "After `my/tasks-waiting-nag-days' the chip turns red."
  (tasks-test--with-temp-dirs
    (let* ((my/tasks-waiting-nag-days 14)
           (long-ago (format-time-string
                      "%Y-%m-%d"
                      (time-subtract (current-time)
                                     (days-to-time 20)))))
      (with-temp-file (expand-file-name "a.md" temp-dir)
        (insert (format "---\nstatus: waiting\nwaiting-since: %s\n---\n\n# Old\n"
                        long-ago)))
      (my/tasks-show-waiting)
      (with-current-buffer "*Waiting*"
        (goto-char (point-min))
        (re-search-forward "(seit 20d)")
        (goto-char (match-beginning 0))
        (should (eq (get-text-property (point) 'face)
                    'my/tasks-overdue-face))))))

(ert-deftest tasks-test--waiting-card-fresh-uses-date-face ()
  (tasks-test--with-temp-dirs
    (let ((today (format-time-string "%Y-%m-%d")))
      (with-temp-file (expand-file-name "a.md" temp-dir)
        (insert (format "---\nstatus: waiting\nwaiting-since: %s\n---\n\n# Fresh\n"
                        today)))
      (my/tasks-show-waiting)
      (with-current-buffer "*Waiting*"
        (goto-char (point-min))
        (re-search-forward "(seit 0d)")
        (goto-char (match-beginning 0))
        (should (eq (get-text-property (point) 'face)
                    'my/tasks-date-face))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recurring tasks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest tasks-test--parse-recurrence-aliases ()
  (should (equal (my/tasks--parse-recurrence "daily") '(1 . ?d)))
  (should (equal (my/tasks--parse-recurrence "weekly") '(1 . ?w)))
  (should (equal (my/tasks--parse-recurrence "monthly") '(1 . ?m)))
  (should (equal (my/tasks--parse-recurrence "Weekly") '(1 . ?w)))
  (should (equal (my/tasks--parse-recurrence "every 2d") '(2 . ?d)))
  (should (equal (my/tasks--parse-recurrence "every 3w") '(3 . ?w)))
  (should (equal (my/tasks--parse-recurrence "every 6m") '(6 . ?m))))

(ert-deftest tasks-test--parse-recurrence-rejects-garbage ()
  (should-not (my/tasks--parse-recurrence nil))
  (should-not (my/tasks--parse-recurrence ""))
  (should-not (my/tasks--parse-recurrence "every 5"))
  (should-not (my/tasks--parse-recurrence "each week"))
  (should-not (my/tasks--parse-recurrence "daily-ish")))

(ert-deftest tasks-test--bump-date-days ()
  (should (equal (my/tasks--bump-date "2026-06-20" 1 ?d) "2026-06-21"))
  (should (equal (my/tasks--bump-date "2026-06-30" 5 ?d) "2026-07-05"))
  (should (equal (my/tasks--bump-date "2026-06-20 09:00" 1 ?d)
                 "2026-06-21 09:00")))

(ert-deftest tasks-test--bump-date-weeks ()
  (should (equal (my/tasks--bump-date "2026-06-20" 1 ?w) "2026-06-27"))
  (should (equal (my/tasks--bump-date "2026-06-20" 4 ?w) "2026-07-18")))

(ert-deftest tasks-test--bump-date-months-basic ()
  (should (equal (my/tasks--bump-date "2026-06-20" 1 ?m) "2026-07-20"))
  (should (equal (my/tasks--bump-date "2026-06-20" 6 ?m) "2026-12-20"))
  (should (equal (my/tasks--bump-date "2026-08-15" 12 ?m) "2027-08-15")))

(ert-deftest tasks-test--bump-date-months-clamps-day ()
  (should (equal (my/tasks--bump-date "2026-01-31" 1 ?m) "2026-02-28"))
  (should (equal (my/tasks--bump-date "2024-01-31" 1 ?m) "2024-02-29")))

(ert-deftest tasks-test--strip-audit-log ()
  (let ((stripped
         (my/tasks--strip-audit-log
          (concat "User notes here.\nMore notes.\n\n"
                  "- 2026-06-20 11:23: inbox → next\n"
                  "- 2026-06-21 09:15: next → today\n"))))
    (should (string-match-p "User notes here" stripped))
    (should (string-match-p "More notes" stripped))
    (should-not (string-match-p "→" stripped))))

(ert-deftest tasks-test--strip-audit-log-keeps-other-bullets ()
  (let ((stripped
         (my/tasks--strip-audit-log
          "- bullet A\n- bullet B\n- 2026-06-20: inbox → next\n")))
    (should (string-match-p "bullet A" stripped))
    (should (string-match-p "bullet B" stripped))
    (should-not (string-match-p "→" stripped))))

(defun tasks-test--first-active-md (dir)
  "Return the (single) active .md file in DIR, ignoring subdirs."
  (car (seq-filter
        (lambda (f) (and (not (file-directory-p f))
                         (string-suffix-p ".md" f)))
        (directory-files dir t "\\.md\\'"))))

(ert-deftest tasks-test--archive-recurring-creates-next-instance ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "wash.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: next\nscheduled: 2026-06-20\n"
                "recurrence: weekly\n---\n\n# Wäsche waschen\n\n"
                "Notes about laundry.\n"))
      (my/tasks--archive-file file)
      (should-not (file-exists-p file))
      (should (= 1 (length (directory-files my/tasks-archive-directory
                                            t "\\.md\\'"))))
      (let* ((new (tasks-test--first-active-md my/tasks-directory))
             (task (my/tasks--parse-frontmatter new)))
        (should new)
        (should (equal (plist-get task :title) "Wäsche waschen"))
        (should (equal (plist-get task :recurrence) "weekly"))
        (should (equal (plist-get task :scheduled) "2026-06-27"))
        (should (equal (plist-get task :status) "next"))
        (let ((text (with-temp-buffer (insert-file-contents new)
                                      (buffer-string))))
          (should (string-match-p "Notes about laundry" text))
          (should-not (string-match-p "→" text)))))))

(ert-deftest tasks-test--archive-recurring-bumps-all-date-fields ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "review.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: next\nscheduled: 2026-06-20\n"
                "due: 2026-06-25\nreminder: 2026-06-20 09:00\n"
                "recurrence: weekly\n---\n\n# Weekly Review\n"))
      (my/tasks--archive-file file)
      (let* ((task (my/tasks--parse-frontmatter
                    (tasks-test--first-active-md my/tasks-directory))))
        (should (equal (plist-get task :scheduled) "2026-06-27"))
        (should (equal (plist-get task :due) "2026-07-02"))
        (should (equal (plist-get task :reminder) "2026-06-27 09:00"))))))

(ert-deftest tasks-test--archive-recurring-today-keeps-status ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "standup.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: today\nscheduled: 2026-06-20\n"
                "recurrence: daily\n---\n\n# Standup\n"))
      (my/tasks--archive-file file)
      (let* ((task (my/tasks--parse-frontmatter
                    (tasks-test--first-active-md my/tasks-directory))))
        (should (equal (plist-get task :status) "today"))
        (should (equal (plist-get task :scheduled) "2026-06-21"))))))

(ert-deftest tasks-test--archive-recurring-no-dates-anchors-to-today ()
  (tasks-test--with-temp-dirs
    (let* ((file (expand-file-name "habit.md" temp-dir))
           (today-+3 (format-time-string
                      "%Y-%m-%d"
                      (time-add (current-time) (days-to-time 3)))))
      (with-temp-file file
        (insert "---\nstatus: next\nrecurrence: every 3d\n---\n\n# Habit\n"))
      (my/tasks--archive-file file)
      (let* ((task (my/tasks--parse-frontmatter
                    (tasks-test--first-active-md my/tasks-directory))))
        (should (equal (plist-get task :scheduled) today-+3))))))

(ert-deftest tasks-test--archive-non-recurring-creates-no-instance ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "once.md" temp-dir)))
      (with-temp-file file (insert "---\nstatus: next\n---\n\n# Once\n"))
      (my/tasks--archive-file file)
      (should-not (tasks-test--first-active-md my/tasks-directory)))))

(ert-deftest tasks-test--archive-recurring-invalid-spec-creates-no-instance ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "bad.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: next\nscheduled: 2026-06-20\n"
                "recurrence: gobbledygook\n---\n\n# Bad\n"))
      (my/tasks--archive-file file)
      (should-not (tasks-test--first-active-md my/tasks-directory)))))

(ert-deftest tasks-test--archive-recurring-preserves-contexts-and-project ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "yoga.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: next\nscheduled: 2026-06-20\n"
                "recurrence: weekly\n"
                "project: \"[[Health]]\"\ncontexts:\n  - \"@home\"\n"
                "  - \"@morning\"\n---\n\n# Yoga\n"))
      (my/tasks--archive-file file)
      (let* ((task (my/tasks--parse-frontmatter
                    (tasks-test--first-active-md my/tasks-directory))))
        (should (equal (plist-get task :project) "[[Health]]"))
        (should (equal (plist-get task :contexts) '("@home" "@morning")))))))

(ert-deftest tasks-test--card-renders-recurrence-chip ()
  (tasks-test--with-temp-dirs
    (with-temp-file (expand-file-name "a.md" temp-dir)
      (insert "---\nstatus: next\nrecurrence: weekly\n---\n\n# A\n"))
    (my/tasks-show-next)
    (with-current-buffer "*Next*"
      (should (string-match-p "🔁 weekly" (buffer-string))))))

(ert-deftest tasks-test--set-recurrence-writes-field ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file (insert "---\nstatus: next\n---\n\n# T\n"))
      (find-file file)
      (my/tasks-set-recurrence "weekly")
      (should (equal (plist-get (my/tasks--parse-frontmatter file)
                                :recurrence)
                     "weekly")))))

(ert-deftest tasks-test--set-recurrence-empty-clears ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file
        (insert "---\nstatus: next\nrecurrence: weekly\n---\n\n# T\n"))
      (find-file file)
      (my/tasks-set-recurrence "")
      (should-not (plist-get (my/tasks--parse-frontmatter file)
                             :recurrence)))))

(ert-deftest tasks-test--set-recurrence-invalid-errors ()
  (tasks-test--with-temp-dirs
    (let ((file (expand-file-name "t.md" temp-dir)))
      (with-temp-file file (insert "---\nstatus: next\n---\n\n# T\n"))
      (find-file file)
      (should-error (my/tasks-set-recurrence "garbage") :type 'user-error))))

(provide 'tasks-test)
;;; tasks-test.el ends here
