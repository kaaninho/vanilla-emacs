;; Jumping between Clojure implementations and tests
(require 'clojure-mode)

(defun clojure-in-tests-p ()
  (or (string-match-p "test\." (clojure-find-ns))
      (string-match-p "/test" (buffer-file-name))))

(defun clojure-underscores-for-hyphens (namespace)
  (replace-regexp-in-string "-" "_" namespace))

(defun clojure-test-implementation-for (namespace)
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string namespace "\\."))
         (namespace-end (split-string (car (last segments)) "_"))
         (namespace-end (mapconcat 'identity (butlast namespace-end 1) "_"))
         (impl-segments (append (butlast segments 1) (list namespace-end))))
    (mapconcat 'identity impl-segments "/")))

(defun clojure-test-for (namespace)
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string namespace "\\.")))
    (mapconcat 'identity segments "/")))

(defun clojure-test-jump-to-implementation ()
  "Jump from test file to implementation."
  (interactive)
  (find-file (format "%s/src/%s.clj"
                     (locate-dominating-file buffer-file-name "src/")
                     (clojure-test-implementation-for (clojure-find-ns)))))

(defun clojure-jump-to-test ()
  "Jump from implementation file to test."
  (interactive)
  (find-file (format "%stest/%s_test.clj"
                     (file-name-as-directory
                      (locate-dominating-file buffer-file-name "src/"))
                     (clojure-test-for (clojure-find-ns)))))

(defun clojure-jump-between-tests-and-code ()
  (interactive)
  (if (clojure-in-tests-p)
      (clojure-test-jump-to-implementation)
    (clojure-jump-to-test)))

(define-key clojure-mode-map (kbd "C-c t") 'clojure-jump-between-tests-and-code)
