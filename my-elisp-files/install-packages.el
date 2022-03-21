(defun install-packages
    ()
  (when (not (package-installed-p 'dash))
    (package-refresh-contents)
    (package-install 'dash))
  (load "dash.el")
  (let* ((all-packages
	  '(auctex-latexmk
	    auctex
	    cider
	    clojure-mode-extra-font-locking
	    clojure-mode
	    company csv-mode
	    git-timemachine
	    helm-ag
	    helm-projectile
	    helm helm-core
	    magit
	    git-commit
	    magit-popup
	    monokai-theme
	    org
	    org-bullets
	    paredit
	    popup
	    projectile
	    pkg-info
	    epl
	    queue
	    rainbow-delimiters
	    rainbow-mode
	    seq
	    smex
	    spinner
	    with-editor
	    dash
	    async))
	 (packages-to-install
	  (-remove 'package-installed-p all-packages)))
    (when packages-to-install
      (package-refresh-contents)
      (mapc 'package-install packages-to-install))))

(install-packages)
