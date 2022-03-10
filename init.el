;;; I use "use-package" for package management
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;; Timeclock
(use-package dash) ; wird von kaan-timeclock.el gebraucht
(load-file "~/.elisp-files/kaan-timeclock.el")

;; overwrite bc of other path
(setq timeclock-file "~/.emacs.dspacemacs/timelog")

;;; Timeclock Erweiterung
(bind-key "C-x t i" #'timeclock-in)
(bind-key "C-x t o" #'timeclock-out)
(bind-key "C-x t c" #'timeclock-change)

;; Für mehr Timeclock-Funktionalität
(load-file "~/.elisp-files/kaan-timeclock.el")
(bind-key "C-x t s" #'timeclock-sum-all-hours)
(bind-key "C-x t t" #'timeclock-hours-worked-today)
(bind-key "C-x t e" #'timeclock-hours-to-days-end)
(bind-key "C-x t u" #'timeclock-overtime)
(bind-key "C-x t w" #'timeclock-last-x-days-overtime)
(bind-key "C-x t f" #'timeclock-open-timelog-file)

;; Um an Timeclock einen Kommentar zu hängen
(load-file "~/.elisp-files/mikes-timeclock.el")
(bind-key "C-x t d" #'timeclock-provide-description)

;;; Open Init File
(bind-key "C-x i" (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;;; magit
(use-package magit
  :ensure t
  :defer t
  :config
  ;; show more detailed diff
  (setq magit-diff-refine-hunk t)

  ;; so that hitting "q" the magit buffers get killed, not only burried
  (eval-after-load 'magit
    '(bind-key "q" (lambda() (interactive) (magit-mode-bury-buffer t)) magit-mode-map))

  :bind ("C-x g" . magit))

(use-package git-timemachine
  :ensure t
  :defer t)

;; noch konfigurieren / testen
(use-package helpful
  :ensure t)

(use-package csv-mode
  :ensure t
  :defer t)

;;; Mail MU4E --------------
(require 'mu4e)
(use-package mu4e
  ;; Muss manuell installieren, klappt nicht via use-package, ebenso: mu4e-alert
  ;; Abhilfe: https://github.com/raxod502/straight.el/issues/491
  :ensure nil
  :init
  ;; Delete key binding "C-x m" for more usage below
  (unbind-key "C-x m")
  (setq mu4e-sent-folder "/activemail/Sent"
      ;; mu4e-sent-messages-behavior 'delete ;; Unsure how this should be configured
      mu4e-trash-folder "/activemail/Trash"
      mu4e-drafts-folder "/activemail/drafts"
      user-mail-address "kaan.sahin@active-group.de"
      smtpmail-default-smtp-server "smtp.active-group.de"
      smtpmail-smtp-server "smtp.active-group.de"
      smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
      smtpmail-smtp-service 587
      mu4e-compose-signature
      "\nKaan Sahin\nActive Group GmbH\nkaan.sahin@active-group.de\n+49 7071 70896 80\n\nHechinger Straße 12/1, 72072 Tübingen\nRegistergericht: Amtsgericht Stuttgart, HRB 224404\nGeschäftsführer: Dr. Michael Sperber\n"
      smtpmail-smtp-user "sahin"
      smtpmail-local-domain "active-group.de"
      ;; um gesendete buffer zu killen
      message-kill-buffer-on-exit t

      message-send-mail-function 'smtpmail-send-it

      ;; Flags nicht als Symbole
      mu4e-use-fancy-chars 'nil
      ;; "-o" wichtig, da sonst das Programm nicht beendet wird und von mu4e
      ;; nicht -wieder aufgerufen werden kann
      mu4e-get-mail-command "offlineimap -o -q"
      mu4e-update-interval 3600
      mu4e-view-show-images t
      mu4e-view-show-addresses t

      ;; Bookmarks
      mu4e-bookmarks
      `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
        ("maildir:/activemail/INBOX" "Active Group INBOX" ?a)
        ("maildir:/activemail/eBike-Manager" "eBike-Manager" ?e)
        ("maildir:/activemail/Sent" "Sent" ?s))

      ;; Bei Reply oder Zitat die Zeile anpassen, dass auch Datum/Uhrzeit angezeigt wird
      message-citation-line-function #'message-insert-formatted-citation-line
      message-citation-line-format "On %Y-%m-%d at %R %Z, %f wrote:\n"
      ;; Setze User-Mail-Adresse, um beim Antworten auf Mails die eigene Adresse
      ;; nicht im CC zu haben
      mu4e-user-mail-address-list '("kaan.sahin@active-group.de")

      ;;; Format flowed für E-Mails
      ;; format=flowed gesendete Nachrichten brechen optional nach X Zeichen um
      ;; Das ist insbesondere für mobile Geräte, wo die standardmäßigen 72 Zeichen
      ;; pro Zeile zu viel sind, sinnvoll, da sonst doppelt umgebrochen wird.

      ;; format=flowed unterstützen nicht alle Programme. Deshalb benutze ich einfach
      ;; harte Breaks. Ist auf mobilen Geräten nicht so schön, aber who cares. Dafuq
      use-hard-newlines nil
      mu4e-compose-format-flowed nil
      fill-flowed-encode-column 72

      ;; Falls eml Dateien drin sind die nicht gut lesbar sind (nicht öffnen kann)
      mu4e-view-use-gnus t
      )
  ;; Mit `q' kann man mu4e ganz verlassen (erhält dann aber auch keine Mails mehr).
  ;; Deshalb überschreiben wir es mit `previous-buffer'.
  (eval-after-load 'mu4e
    '(bind-key "q" #'previous-buffer mu4e-main-mode-map))

  ;; start mu4e
  (add-to-list 'load-path "/home/kaan/.nix-profile/share/emacs/site-lisp/mu/mu4e/")
  (require 'mu4e)
  (mu4e t)

  ;; Notifications
  (mu4e-alert-set-default-style 'libnotify)
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display)

  ;; Damit kann man Kalendereinladungen per Mail mu4e annehmen
  (require 'mu4e-icalendar)
  (mu4e-icalendar-setup)

  :bind (("C-x m m" . mu4e)
         ;; Global Key Binding für Update mails
         ("C-x m u" . mu4e-update-mail-and-index)))

;;; Ivy
(use-package ivy
  :ensure t
  :defer t
  :diminish ""
  :init
  (ivy-mode))

;;; Counsel
(use-package counsel
  :ensure t
  :defer t
  :diminish ""
  :bind ("C-x C-r" . counsel-buffer-or-recentf)
  :init
  ;; I want to have recentf enabled and more saved items
  (recentf-mode 1)
  (counsel-mode)
  (setq recentf-max-menu-items 200)
  (setq recentf-max-saved-items 500))

(use-package wgrep
  :ensure t
  :defer t)

;;; Smex fork
;; Persists and shows M-x history
(use-package amx
  :ensure t
  :init
  (amx-mode))

(use-package which-key
  :ensure t
  :defer t
  :diminish ""
  :init (which-key-mode)

  ;; falls which-key mal langsam sein sollte, einfach
  ;; (setq which-key-replacement-alist nil)
  ;; ausführen.
  )

;;; Swiper
(use-package swiper
  :ensure t
  :defer t
  :bind ("C-s" . swiper))

;;; Marginalia
;; Shows hints in mini buffer
(use-package marginalia
  :ensure t
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package org
  :ensure t
  :defer t
  :init
  ;; This is somehow needed for storing a link from a message for capturing
  (require 'org-mu4e)

  ;; Org-capture-templates
  (setq org-capture-templates
        `(("t" "Todo mit Datei-Link" entry (file+headline "todos.org" "Inbox")
           "* TODO %?\n %i %a" :prepend t)
          ("z" "Todo" entry (file+headline "todos.org" "Inbox")
           "* TODO %?\n %i" :prepend t)))
  ;; org-agenda, org-refile
  (setq org-agenda-files '("~/org")
        org-default-notes-file "todos.org"
        org-refile-targets '((org-agenda-files :maxlevel . 1))
        org-agenda-window-setup 'current-window
        org-agenda-restore-windows-after-quit t)

  ;; Dadurch werden nach einem
  ;; - refile von capture
  ;; - finish von capture
  ;; - TODOstatus zu DONEstatus in org-agenda
  ;; - reschedule in agenda view
  ;; die org-Dateien direkt gespeichert
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (advice-add 'org-capture-finalize :after 'org-save-all-org-buffers)
  (advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)
  (advice-add 'org-agenda-schedule :after (lambda (&rest args) (org-save-all-org-buffers)))


  ;; geht irgendwie nicht zu installieren?
  ;; vielleicht org-notifications benutzen?
  ;; org-notify um an Termine zu erinnern
  ;; (require 'org-notify)
  ;; (org-notify-add 'daily
  ;;                 '(:time "5m" :period "2m" :duration 100
  ;;       	          :actions -notify)
  ;;                 '(:time "5m" :period "2m" :duration 100
  ;;       	          :actions (-message -ding)))
  ;; (org-notify-start)

  ;; I do not want to show the daily TODOs in agenda view
  ;; see: https://orgmode.org/manual/Special-Agenda-Views.html
  (defun my-skip-fn ()
    "Skip daily"
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (and (re-search-forward "notify" subtree-end t)
               (re-search-forward "daily" subtree-end t))
          subtree-end
        nil)))
  (setq org-agenda-skip-function-global 'my-skip-fn)

  ;; Todo Faces
  (setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("To-Watch" . "yellow")
        ("Watched" . (:foreground "#FD971F" :weight bold))))

  ;; org-reveal
  (setq org-reveal-root "file:///home/kaan/tmp/reveal.js")

  ;; org-babel language support
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((clojure . t)
       (emacs-lisp . t)
       (haskell . t)
       (python . t)
       (latex . t)
       (plantuml . t))))

  ;; ---- Ende :init ----

  :config
  (setq org-indent-mode nil
        org-edit-src-content-indentation 0
        ;; org-capture funktioniert nicht weil Variable nicht bekannt,
        ;; deshalb setzen
        org-indent-indentation-per-level 2
        org-enable-reveal-js-support t
        org-hide-emphasis-markers t
        org-emphasis-alist '(("*" bold)
		             ("/" italic)
		             ("_" underline)
		             ("=" (:foreground "#EFCA08" :background "#555555"))
		             ("~" org-verbatim verbatim)
		             ("+" (:strike-through t)))

        ;; For images in org-mode
        org-image-actual-width nil
        )

  ;;; org-reveal
  (use-package org-re-reveal
    :ensure t)
  (require 'org-re-reveal)

  ;;; ORG EASY STRUCTURE TEMPLATE
  ;; To use org easy structure templates (also `<s' für Code-Beispiel)
  (require 'org-tempo)
  ;; org-present
  (use-package org-present
  :hook ((org-present-mode . (lambda ()
                               (org-present-big)
                               (org-display-inline-images)
                               (org-present-hide-cursor)
                               (org-present-read-only)))
         (org-present-mode-quit . (lambda ()
                                    (org-present-small)
                                    (org-remove-inline-images)
                                    (org-present-show-cursor)
                                    (org-present-read-write)))

         ;; org-mode disable indentation after return
         ;; (add-hook 'org-mode-hook (lambda () (electric-indent-mode -1)))

         ))

  ;; When `org-hide-emphasis-markers' is `t' it's convenient to see
  ;; the markers when editing the text. `org-appear' does that.
  (use-package org-appear
    :ensure t
    :defer t)

  (use-package org-superstar
    :ensure t
    :config
    (setq org-superstar-remove-leading-stars nil
          org-superstar-headline-bullets-list '(" ․")
          ;; This is for not slowing down
          inhibit-compacting-font-caches t
          ))

  ;; ---- Ende :config ----

  :hook (;; Set margin to left side
         (org-mode . (lambda ()
                       (setq-local left-margin-width 5)
                       (setq-local right-margin-width 5)
                       (set-window-buffer nil (current-buffer))))
         (org-mode . (lambda () (setq fill-column 80)))
         (org-mode . (lambda () (org-superstar-mode)))
         (org-mode . (lambda () (display-line-numbers-mode -1)))
         (org-mode . (lambda () (org-appear-mode)))
         ;; Use other fonts for headings
         (org-mode . (lambda ()
                       (let* ((variable-tuple
                               (cond ((x-list-fonts "NotoSerif")   '(:font "NotoSerif"))
                                     ((x-family-fonts "Serif")    '(:family "Serif"))
                                     (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
                              (base-font-color     (face-foreground 'default nil 'default))
                              (headline           `(:inherit default :weight bold :foreground ,base-font-color))
                              (title-info-font '(:font "DejaVu Sans Mono")))

                         (custom-theme-set-faces
                          'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.15))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5))))
                          `(org-document-title ((t (,@headline ,@title-info-font :height 1.25 :underline nil))))
                          `(org-document-info ((t (,@headline ,@title-info-font :height 1.0 :underline nil))))
                          `(org-warning ((t (:inherit default :weight bold :foreground "#FFAAAA" ,@title-info-font :height 1.05 :underline nil)))))))))

  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)))

;;; Idle Highlight Mode
(use-package auto-highlight-symbol
  :ensure t
  :demand t
  :diminish ""
  :init
  (global-auto-highlight-symbol-mode t)
  :config
  (ahs-set-idle-interval 2))

;;; Auto-Completion
(use-package company
  :ensure t
  :defer t
  :diminish ""
  :init
  (global-company-mode)
  (setq company-idle-delay 0.5)
  :bind ("C-<return>" . company-complete))

;;; Projectile
(use-package projectile
  :ensure t
  :diminish ""
  :init
  (projectile-mode +1)

  (setq projectile-project-search-path '(("~/projekte/" . 2) ("~/active-group/" . 5)))
  ;; The auto-discover consumes too much time. Trigger manually with
  ;; (projectile-discover-projects-in-search-path)
  (setq projectile-auto-discover nil)
  (setq projectile-sort-order 'recently-active)

  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

;; Um Mode-Namen in der Mode Line zu verändern oder ganz zu entfernen
;; benutzbar in `use-package' via `:diminish' Keyword
(use-package diminish
  :ensure t)

;; Schönere Themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-monokai-machine t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
(package-refresh-contents)

;; Hiermit werden Tastenanschläge und -Kombinationen in der Mode-Line
;; angezeigt, gut für Screencasting
;; Zudem wird bei Funktionen der Funktionsname nebendran angezeigt
(use-package keycast
    :ensure t
    :init
    (add-to-list 'global-mode-string '("" mode-line-keycast))
    ;; (keycast-mode)
    )

;;;; ---- Programming Languages ----

;;; Clojure
(use-package clojure-mode
  :ensure t
  :defer t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))

  :config
  ;; clj-refactor don't warn when using `cljr-find-usages'
  (setq cljr-warn-on-eval nil)
  (use-package clj-refactor
    :ensure t
    :defer t)

  :hook ((clojure-mode . (lambda () (bind-key (kbd "M-t") 'transpose-sexps 'clojure-mode-map 'clojure-mode?)))
         ;; Clojure add define-record-type Tipperleichterung
         ;; Record-Tipperleichterung
         (clojure-mode . (lambda ()
                           (load-file "~/.elisp-files/insert-define-record-type.el")
                           (bind-key (kbd "C-c C-r C-r") 'insert-define-record-type)))))

;;; CIDER
(use-package cider
  :ensure t
  :defer t
  :init (add-hook 'cider-mode-hook #'clj-refactor-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))


;;; LSP
(setq elixir-path "~/.elixir-lsp/release")

(use-package lsp-mode
  :ensure t
  :defer t
  :hook ((elixir-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (python-mode . lsp))

  :init
  (add-to-list 'company-backends 'company-capf)
  (add-to-list 'exec-path elixir-path)
  ;; :config
  ;; (setq lsp-ui-doc-enable nil)
  :bind (("C-c d d" . lsp-ui-doc-show)
         ("C-c d h" . lsp-ui-doc-hide))
  :commands lsp)

;; Needed somehow for python + lsp
(use-package all-the-icons
  :ensure t
  :defer t)

;; optionally
(use-package lsp-ui
  :ensure t
  :defer t
  :commands lsp-ui-mode)

;; if you are ivy user
(use-package lsp-ivy
  :ensure t
  :defer t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :defer t
  :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language


;;; Scala
(use-package scala-mode
  :ensure t
  :defer t
  :interpreter
  ("scala" . scala-mode)
  ;; :hook
  ;; (scala-mode . (lambda ()
  ;;                 (add-hook 'before-save-hook #'delete-trailing-whitespace)))
  ;; (scala-mode . (lambda ()
  ;;                       (add-hook 'before-save-hook #'lsp-format-buffer)))
)

(use-package sbt-mode
  :commands sbt-start sbt-command
  :ensure t
  :defer t
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))


;; Add metals backend for lsp-mode
(use-package lsp-metals
  :defer t
  :ensure t
  :hook (scala-mode . lsp))

;;; TODO
;;; Um Häufigkeit von Keybinding-Usage anzuzeigen
;; (use-package keyfreq
;;   :init
;;   (require 'keyfreq)
;;   (keyfreq-mode 1)
;;   (keyfreq-autosave-mode 1)

;;   :config
;;   (setq keyfreq-excluded-commands
;;         '(self-insert-command
;;           forward-char
;;           backward-char
;;           previous-line
;;           next-line
;;           ivy-next-line
;;           org-self-insert-command
;;           lsp-ui-doc--handle-mouse-movement
;;           ignore
;;           mwheel-scroll
;;           magit-next-line
;;           ivy-backward-delete-char
;;           delete-backward-char
;;           org-delete-backward-char
;;           delete-char
;;           mouse-drag-region
;;           new-line-and-indent
;;           ivy-done
;;           magit-previous-line
;;           ivy-previous-line)))

;;; Elixir
;; Language server von hier geklont:
;; https://github.com/elixir-lsp/elixir-ls
;; Muss manchmal aktualisiert werden, gehe ins Repo
;; > cd "~/.elixir-lsp/release/language_server.sh"
;; und mache > `git pull'

(use-package elixir-mode
  :ensure t
  :defer t

  :config
  (setq  elixir-backend 'lsp
         elixir-ls-path elixir-path
         lsp-elixir-fetch-deps nil
         ;; needed?
         ;;lsp-elixir-local-server-command "~/.elixir-lsp/release/language_server.sh"
         )
  (add-to-list 'exec-path "~/.elixir-lsp/release/language_server.sh")
  ;; TODO
  ;; (spacemacs/declare-prefix-for-mode 'elixir-mode
  ;;                                    "mt" "tests" "testing related functionality")
  ;; (spacemacs/set-leader-keys-for-major-mode 'elixir-mode
  ;;                                           "ta" 'exunit-verify-all
  ;;                                           "tb" 'exunit-verify
  ;;                                           "tr" 'exunit-rerun
  ;;                                           "tt" 'exunit-verify-single)
  ;; Needed?
  ;; Elixir settings
  ;; Wenn lsp mal nicht geht (und auch z. B. linter), dann das hier ausführen:
  ;; rm -r deps _build .elixir_ls && mix deps.get
  ;; ggf. auch mix deps.compile

  ;; das hier setzen, damit emacs die deps nicht automatisch fetcht (gab bei mir
  ;; probleme bzgl mix.lock)
  ;; lsp-elixir-fetch-deps nil

  ;; (add-hook 'elixir-mode-hook
  ;;     (lambda ()
  ;;       (add-hook 'before-save-hook #'elixir-format t t)))

  ;; documentation popup timeout / delay
  (setq lsp-ui-doc-delay 2)

  )

(use-package python
  :ensure t
  :defer t
  :init
  (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
  (custom-set-variables
  '(elpy-rpc-virtualenv-path (quote current))
  ))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred



;;;; ---- Global Key Bindings ----

;;; Protokoll-Template
(load-file "~/.elisp-files/protokoll-template.el")
(bind-key "C-x p" #'protocol-template)

(bind-key "C-<tab>" #'other-window)
(bind-key "C-S-<tab>" (lambda () (interactive) (other-window -1)))
(bind-key "C-x C-y" #'yas-expand)
(bind-key "C-z" #'undo)
(bind-key "C-#" "@")
(bind-key "C-+" "~")
(bind-key "ö" "[")
(bind-key "M-ö" "{")
(bind-key "ä" "]")
(bind-key "M-ä" "}")
(bind-key "C-x C-b" #'ibuffer)
(defun insert-backslash ()
  "insert backslash"
  (interactive)
  (insert "\\"))
(bind-key "M-+" #'insert-backslash)
(bind-key "M-*" "|")
(bind-key "C-ö" #'join-line)
(bind-key "C-q" #'previous-buffer)

;;; Umlaute
(defun insert-oe ()
  "insert ö"
  (interactive)
  (insert "ö"))
(defun insert-ae ()
  "insert ä"
  (interactive)
  (insert "ä"))
(bind-key "C-M-ö" #'insert-oe)
(bind-key "C-M-ä" #'insert-ae)

;;; Page Up and Down
(bind-key "M-p" #'scroll-down-command)
(bind-key "M-n" #'scroll-up-command)

;; Timeclock Erweiterung
(bind-key "C-x t i" #'timeclock-in)
(bind-key "C-x t o" #'timeclock-out)
(bind-key "C-x t c" #'timeclock-change)

;; Für mehr Timeclock-Funktionalität
(load-file "~/.elisp-files/kaan-timeclock.el")
(bind-key "C-x t s" #'timeclock-sum-all-hours)
(bind-key "C-x t t" #'timeclock-hours-worked-today)
(bind-key "C-x t e" #'timeclock-hours-to-days-end)
(bind-key "C-x t u" #'timeclock-overtime)
(bind-key "C-x t w" #'timeclock-last-x-days-overtime)
(bind-key "C-x t f" #'timeclock-open-timelog-file)

;; Um an Timeclock einen Kommentar zu hängen
(load-file "~/.elisp-files/mikes-timeclock.el")
(bind-key "C-x t d" #'timeclock-provide-description)

;; Ergänzung zu `other-window'
(bind-key "C-c <left>"  #'windmove-left)
(bind-key "C-c <right>" #'windmove-right)
(bind-key "C-c <up>"    #'windmove-up)
(bind-key "C-c <down>"  #'windmove-down)

;; other-window wird so oft verwendet von mir, mache Abkürzung:
(bind-key "C-o" #'other-window)

;; Damit kill-buffer ohne Confirmation direkt den Buffer killt:
(bind-key "C-x k" #'kill-current-buffer)

(bind-key "C-ö" #'join-line)
;; Damit kill-buffer ohne Confirmation direkt den Buffer killt:
(bind-key "C-x k" #'kill-current-buffer)
(bind-key "C-q" #'previous-buffer)

;;;; ---- Hooks ----

;;; Paredit
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojurescript-mode-hook 'paredit-mode)

;;; before save delete trailing white space
;; (add-hook 'before-save-hook #'delete-trailing-whitespace)

;;;; ---- Other stuff ----

;; Customize user interface.
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-startup-screen nil)
(column-number-mode)

;; Scrolling
(setq scroll-step            1
      scroll-conservatively  10000)

(set-face-attribute 'default nil :height 120)

;; Use spaces, not tabs, for indentation.
(setq-default indent-tabs-mode nil)

;; Highlight matching pairs of parentheses.
(setq show-paren-delay 0)
(show-paren-mode)

;; Write auto-saves and backups to separate directory.
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))

(setq custom-file (concat user-emacs-directory "custom.el"))

(load custom-file t)

;; Do not move the current file while creating backup.
(setq backup-by-copying t)

;; Disable lockfiles.
(setq create-lockfiles nil)

;;; Benutze olivetti-mode für schönes Schreiben
(when (require 'so-long nil :noerror)
  (global-so-long-mode 1))

;;; Um markierte Region zu loeschen bei Tastatureingabe
(delete-selection-mode 1)

;;; So geht das Scrolling bei langen Zeilen hoffentlich schneller
(setq-default bidi-display-reordering nil)
(setq fast-but-imprecise-scrolling t)

;;; Backup Files:
(load-file "~/.elisp-files/backup.el")

;;; PlantUML
(setq plantuml-jar-path (expand-file-name "~/bin/plantuml.jar")
      org-plantuml-jar-path plantuml-jar-path
      plantuml-default-exec-mode 'jar
      plantuml-output-type "svg")

;;; TODO
;;; direnv envrc
;;(envrc-global-mode)

;;; Cursor goes to Help buffer
(setq help-window-select t)

;;; Nested Project
;; with bottom-up first, we always get dir with .hg / .git
;; even if true project directory is sitting underneath
(setq projectile-project-root-files-functions
      '(projectile-root-local
        projectile-root-top-down
        projectile-root-top-down-recurring
        projectile-root-bottom-up))

;;; Tramp
;; damit man Dateien über SSH öffnen kann
;; (Liegt an custom shell prompt (liquidprompt))
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

;;; Set Custom face
;; set region highlighting more visible (orange)
(set-face-attribute 'region nil :background "#FD971F" :inherit 'highlight)
(set-face-attribute 'ivy-current-match nil :background "#FD971F" :inherit 'highlight)

;;; line numbers
(global-display-line-numbers-mode)

;;; Highlight current line
(global-hl-line-mode)

;;; Sentences are ended with only one space
(setq sentence-end-double-space nil)

;;; Save position u were in
(save-place-mode 1)

;;; If a file changes on disk, revert buffer automatically
(global-auto-revert-mode)


;;; Start server.
(require 'server)
(unless (server-running-p)
  (server-start))

;;;; ---- Tips, Tricks and Workarounds ----

;;; Command History
;; Das hier setzen, falls command-history Duplikate zeigt
;; (setq  history-delete-duplicates t)
;; Falls man die History begrenzen möchte in der Größe
;; (setq history-length 150)
;; (setq extended-command-history nil)  ; hiermit manuell die ganze Liste löschen


;;; Bug, still there?
;; This is a bug. The variable on the left side is old but still used, but
;; cannot be found anymore.
;; TODO: Delete at next update!
;; (setq org-priority-highest org-highest-priority)
;; (setq org-priority-lowest org-lowest-priority)

;;; org-refile funktioniert nicht wegen
;; "Invalid function: org-preserve-local-variables"
;; Dann: lösche kompilierte Dateien und rekompiliere:
;; `cd ~/.emacs.d && find . -name *.elc -print0 | xargs -0 rm`
;; Danach `M-x spacemacs/recompile-elpa`

;;; Alte Netzlive Elixir Einstellungen
;; (with-eval-after-load 'lsp-mode
;;   (setq lsp-restart 'auto-restart)
;;   (setq lsp-file-watch-threshold 25000)
;;   (push "[/\\]docker/temp$" lsp-file-watch-ignored)
;;   (push "temp$" lsp-file-watch-ignored))


;;;; TODO

;; TODO: MULTI CURSORS

;; TODO: Schriftart: Hack statt Deja Vu

;; TODO: Selectrum? https://github.com/raxod502/selectrum


;;;; Aussortiert

;;; project.el
;; eingebaut. Hatte nicht alles, was ich wollte, was projectile hat. Z. B.:
;; - toggle namespace / tests
;; - search in project files for a string
;;
;; (use-package project
;;   :ensure t
;;   :config
;;   (fset 'project-prefix-map project-prefix-map)
;;   :bind ("C-c p" . project-prefix-map))
