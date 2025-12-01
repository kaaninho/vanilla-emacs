;;;; Emacs Config File
;;;; Author: Kaan Sahin

;;; Package management

(require 'package)
(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

;;; I use "use-package" for package management
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;; Open Init File
(bind-key "C-x i" (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;;; um auf MacOS die Option und  Meta Tasten zu tauschen
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;;; magit
(use-package magit
  :defer t
  :config
  ;; show more detailed diff
  (setq magit-diff-refine-hunk t)

  ;; so that hitting "q" the magit buffers get killed, not only burried
  (eval-after-load 'magit
    '(bind-key "q" (lambda() (interactive) (magit-mode-bury-buffer t)) magit-mode-map))

  :bind ("C-x g" . magit))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package git-timemachine
  :defer t)

;; noch konfigurieren / testen
(use-package helpful
  )
(use-package csv-mode
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package nix-mode
  :defer t
  :mode "\\.nix\\'")

;;; Mail MU4E --------------
(use-package mu4e
  ;; Muss manuell installieren, klappt nicht via use-package, ebenso: mu4e-alert
  ;; Abhilfe: https://github.com/raxod502/straight.el/issues/491
  :ensure nil
  :init
  ;; Delete key binding "C-x m" for more usage below
  (unbind-key "C-x m")
  (setq
   ;; Mail Receive
   mu4e-get-mail-command "mbsync PHL"
   mu4e-mu-binary "/opt/homebrew/bin/mu"

   mu4e-sent-folder "/ph/Gesendete Elemente"
   ;; mu4e-sent-messages-behavior 'delete ;; Unsure how this should be configured
   mu4e-trash-folder "/ph/Gelöschte Elemente"
   mu4e-drafts-folder "/ph/Entwürfe"
   user-mail-address "kaan.sahin@ph-ludwigsburg.de"
   user-full-name "Sahin, Kaan"

   ;; Mail Send
   message-send-mail-function 'message-send-mail-with-sendmail
   sendmail-program "msmtp"
   
   mu4e-compose-signature
   "Kaan Sahin\nInstitut für Informatik\nPädagogische Hochschule Ludwigsburg\nReuteallee 46\nD-71634 Ludwigsburg\n\nTel: +49 (0) 7141/140 - 2894\nRaum: 5.209\nE-Mail: kaan.sahin@ph-ludwigsburg.de"
   smtpmail-local-domain "mail.ph-gw.de"
   ;; um gesendete buffer zu killen
   message-kill-buffer-on-exit t

   ;; Flags als Symbole
   mu4e-use-fancy-chars 't
   mu4e-update-hourly-p t
   mu4e-update-interval 3600
   mu4e-view-show-images t
   mu4e-view-show-addresses t
   mu4e-confirm-quit nil

   ;; Bookmarks
   mu4e-bookmarks
   `(("flag:unread AND NOT flag:trashed" "Ungelesene Nachrichten" ?u)
     ("maildir:/ph/Inbox" "INBOX" ?a)
     ("maildir:/ph/Gesendete Elemente" "Gesendete Elemente" ?s))

   ;; Bei Reply oder Zitat die Zeile anpassen, dass auch Datum/Uhrzeit angezeigt wird
   message-citation-line-function #'message-insert-formatted-citation-line
   message-citation-line-format "On %Y-%m-%d at %R %Z, %f wrote:\n"
   ;; Setze User-Mail-Adresse, um beim Antworten auf Mails die eigene Adresse
   ;; nicht im CC zu haben
   mu4e-user-mail-address-list '("kaan.sahin@ph-ludwigsburg.de")
   mu4e-compose-dont-reply-to-self t

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
   mu4e-attachment-dir "/Users/kaan/Downloads"

   mu4e-headers-fields  '((:human-date . 12)
                          (:flags . 6)
                          (:from-or-to . 25)
                          (:subject . nil)))
  
  ;; Mit `q' kann man mu4e ganz verlassen (erhält dann aber auch keine Mails mehr).
  ;; Deshalb überschreiben wir es mit `previous-buffer'.
  (eval-after-load 'mu4e
    '(bind-key "q" (lambda ()
                     (interactive)
                     (winner-undo))
               mu4e-main-mode-map))
  ;; leichter updaten
  (eval-after-load 'mu4e
    '(bind-key "u" #'mu4e-update-mail-and-index mu4e-main-mode-map))

  ;; start mu4e
  ;; Hab für den Mac mu installiert mit brew
  (add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e")
  (require 'mu4e)
  (mu4e t)

  (defun mu4e-toggle-update-frequency ()
    (interactive)
    (if mu4e-update-hourly-p
        (progn
          (setq mu4e-update-interval 10)
          (setq mu4e-update-hourly-p nil)
          (mu4e-quit)
          (sit-for 1)
          (message "[mu4e] Update every 10 SECONDS")
          (sit-for 1)
          (mu4e t))
      (progn
        (setq mu4e-update-interval 3600)
        (setq mu4e-update-hourly-p t)
        (mu4e-quit)
        (sit-for 1)
        (message "[mu4e] Update every HOUR")
        (sit-for 1)
        (mu4e t))))
  
  ;; Notifications
  ;; (mu4e-alert-set-default-style 'libnotify)
  ;; (mu4e-alert-enable-notifications)
  ;; (mu4e-alert-enable-mode-line-display)

  ;; Damit kann man Kalendereinladungen per Mail mu4e annehmen
  (require 'mu4e-icalendar)
  (mu4e-icalendar-setup)

  :bind (("C-x m m" . mu4e)
         ;; Global Key Binding für Update mails
         ("C-x m u" . mu4e-update-mail-and-index)
         ("C-x m t" . mu4e-toggle-update-frequency)))

;; Damit kann man Mails als HTML versenden einfach im
;; compse-mail-buffer dann `org-mime-htmlize' aufrufen.
;; Um in org-mode die Mail zu verfassen, kann man
;; `org-mime-edit-mail-in-org-mode'
(use-package org-mime
  :defer t)

;;; Ivy
(use-package ivy
  :defer t
  :diminish ""
  :init
  (ivy-mode))

;;; This is for inlining the Mx- and Search buffers
;;  a centered frame appears
;; (use-package ivy-posframe
;;   :diminish ""
;;   :after ivy
;;   :config
;;   (setq ivy-posframe-border-width 1)
;;   (set-face-attribute 'ivy-posframe-border nil :background "#FD971F")
;;   (ivy-posframe-mode))

;;; Counsel
(use-package counsel
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
  :defer t)

;;; Smex fork
;; Persists and shows M-x history
(use-package amx
  :init
  (amx-mode))

(use-package which-key
  :defer t
  :diminish ""
  :init (which-key-mode)

  ;; falls which-key mal langsam sein sollte, einfach
  ;; (setq which-key-replacement-alist nil)
  ;; ausführen.
  )

;;; Swiper
(use-package swiper
  :defer t
  :bind ("C-s" . swiper))

;;; Marginalia
;; Shows hints in mini buffer
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package org
  :defer t
  :init
  ;; Org-capture-templates
  (setq org-capture-templates
        `(("t" "Todo mit Datei-Link" entry (file+headline "todos.org" "Inbox")
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n %i %a" :prepend t)
          ("z" "Todo" entry (file+headline "todos.org" "Inbox")
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n %i" :prepend t)))
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
      '(("TODO" . org-warning)
        ("To-Watch" . "yellow")
        ("Watched" . (:foreground "#FD971F" :weight bold))))

  ;; org-reveal
  (setq org-reveal-root "file:///home/kaan/tmp/reveal.js")

  (add-to-list 'load-path "/home/kaan/.emacs.d/my-elisp-files/")

  ;; org-babel language support
  (setq org-confirm-babel-evaluate nil)
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((clojure . t)
       (emacs-lisp . t)
       (haskell . t)
       (python . t)
       (latex . t)
       (plantuml . t)
       (scheme . t))))

  ;; ---- Ende :init ----

  :config
  (setq org-indent-mode nil
        org-edit-src-content-indentation 0
        ;; org-capture funktioniert nicht weil Variable nicht bekannt,
        ;; deshalb setzen
        org-indent-indentation-per-level 0
        ;; dies wegen "nach enter nicht einrücken. ggf. nicht nötig,
        ;; wahrscheinlich eher `(electric-indent-mode -1)`
        org-adapt-indentation nil
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
    ) (require 'org-re-reveal)

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
         ))

  ;; When `org-hide-emphasis-markers' is `t' it's convenient to see
  ;; the markers when editing the text. `org-appear' does that.
  (use-package org-appear
    :defer t)

  (use-package org-superstar
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
                              (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

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
                          `(org-document-title ((t (,@headline :height 1.25 :underline nil))))
                          `(org-document-info ((t (,@headline :height 1.0 :underline nil))))
                          `(org-warning ((t (:inherit default :weight bold :foreground "#FFAAAA" :height 1.05 :underline nil)))))))))

  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "/home/kaan/org/org-roam"))
  (org-roam-dailies-directory "/home/kaan/org/org-roam/daily")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n d" . org-roam-dailies-goto-today)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (setq org-roam-graph-viewer "/usr/bin/google-chrome")
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;; Little Major Mode for an obsidian markdown buffer
(define-derived-mode obsidian-capture-mode markdown-mode "Obsidian Capture"
  "Major mode for capturing a note in Obsidian Daily file."
  ;; Set keybinding for saving and going to previous buffer
  (bind-key "C-c C-c" (lambda () (save-buffer) (kill-buffer))
            'obsidian-markdown-mode-map))

(defun obsidian-daily-note-with-time ()
  (interactive)
  (obsidian-daily-note)
  (obsidian-markdown-mode)
  (insert "\n\n# " (format-time-string "%H:%M:%S\n\n"))
  (insert "- [ ] ")
  (save-buffer))

;;; Obsidian-Integration
(use-package obsidian
  :after (markdown-mode)
  :config
  (setq obsidian-directory "/Users/kaan/Library/Mobile Documents/iCloud~md~obsidian/Documents/ObsidianOnIcloud"
        obsidian-daily-notes-directory "Daily"
        obsidian-inbox-directory "INBOX")

  :bind (("C-c c" . obsidian-capture)
         ("C-c d" . obsidian-daily-note-with-time)
         ("C-c n f" . obsidian-follow-link-at-point)))

(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package markdown-mode
  :mode
  (("\\.md\\'" . markdown-mode))

  :custom
  (markdown-header-scaling t)
  (markdown-hide-urls t)
  (markdown-fontify-code-blocks-natively t))

;;; Idle Highlight Mode
(use-package auto-highlight-symbol
  :demand t
  :diminish ""
  :init
  (global-auto-highlight-symbol-mode t)
  :config
  (ahs-set-idle-interval 2))

;;; Auto-Completion
(use-package company
  :defer t
  :diminish ""

  :init
  (global-company-mode)
  (setq company-idle-delay 0.2)

  :bind
  ("C-<return>" . company-complete))

;;; Projectile
(use-package projectile
  :diminish ""
  :init
  (projectile-mode +1)

  (setq projectile-project-search-path '(("~/projekte/" . 2) ("~/active-group/" . 5)))
  ;; The auto-discover consumes too much time. Trigger manually with
  ;; (projectile-discover-projects-in-search-path)
  ;; Hab es trotzdem mal auf `t' gesetzt, mal gucken (davor wars, wegen Kommentar oben, auf `nil')
  (setq projectile-auto-discover t)
  (setq projectile-sort-order 'recently-active)

;;; Nested Project
  ;; with bottom-up first, we always get dir with .hg / .git
  ;; even if true project directory is sitting underneath
  (setq projectile-project-root-files-functions
        '(projectile-root-local
          projectile-root-top-down
          projectile-root-bottom-up
          projectile-root-top-down-recurring))

  (setq projectile-enable-caching t)

  :config
  (use-package projectile-ripgrep)
  (use-package counsel-projectile)
  :bind ((:map projectile-mode-map
               ("C-c p" . projectile-command-map)
               ("C-c p b" . counsel-projectile-switch-to-buffer))
         ;; To get buffer of results, use ivy-occur (C-c C-o)
         (:map projectile-mode-map
               ("C-c p s" . counsel-projectile-rg))))

;; Um Mode-Namen in der Mode Line zu verändern oder ganz zu entfernen
;; benutzbar in `use-package' via `:diminish' Keyword
(use-package diminish)
;; Schönere Themes
(use-package doom-themes
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

;; GPTEL

(defun ollama-backend () (gptel-make-ollama "Ollama" :host "localhost:11434" :stream t :models '(llama3)))
(defun openai-backend () (gptel-make-openai "OpenAI"
                            :key ""
                            :models '(gpt-4.1-nano gpt-3.5-turbo gpt-4o) :stream t))
(use-package gptel
  :ensure t
  :commands (gptel gptel-send)
  :bind (("C-c b" . my/gptel-switch-backend)
         ("C-c g" . gptel)
         ("C-c s" . gptel-send))
  :config
  ;; Standardmodell
  (setq gptel-model 'llama3
        gptel-backend
        (if (my/ollama-running-p)
            ;; Lokales Modell, wenn Ollama läuft
            (progn
              (message "Ollama erkannt – nutze lokales Modell.")
              (ollama-backend))
          ;; Fallback auf OpenAI, wenn Ollama nicht läuft
          (progn
            (message "Ollama nicht gefunden – nutze OpenAI-API.")
            (openai-backend))))
  
  (defun my/ollama-running-p ()
    "Prüft, ob Ollama-Server auf localhost:11434 erreichbar ist."
    (interactive)
    (condition-case nil
        (with-temp-buffer
          (url-insert-file-contents "http://localhost:11434/api/tags")
          (message "Ollama läuft")
          t)
      (message "ERROR Ollama läuft nicht")
      (error nil)))

  (defun my/gptel-switch-backend ()
    "Wechselt zwischen lokalem Ollama und OpenAI im aktuellen gptel-Chat."
    (interactive)
    (require 'gptel)
    (if (equal (gptel-backend-name gptel-backend) "Ollama")
        ;; Wechsel zu OpenAI
        (progn
          (setq gptel-backend
                (openai-backend))
          (setq gptel-model 'gpt-4.1-nano)
          (message "☁️ Backend gewechselt zu OpenAI (%s)" gptel-model))
      ;; Wechsel zu Ollama
      (if (my/ollama-running-p)
          (progn
            (setq gptel-backend (ollama-backend)
                  gptel-model 'llama3)
            (message "🦙 Backend gewechselt zu Ollama (%s)" gptel-model))
        (message "🛑 Ollama läuft gerade nicht! Fallback auf OpenAI.")))))

(use-package copilot
  :config
  (setq copilot-idle-delay 3
        copilot-enable-display-predicates
        '(copilot-display-in-comments
          copilot-display-in-code
          copilot-display-in-strings))
  :ensure t
  ;; I don't want to have copilot everywhere
  ;; :hook
  ;; (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-TAB" . copilot-next-completion)
              ("C-<tab>" . copilot-next-completion)))

;; Ellama-Konfiguration mit use-package
(use-package ellama
  :ensure t
  :bind
  ;; Ellama-Menü starten
  ("C-c e" . ellama)
  :init
  ;; Automatisches Scrollen im Chat-Puffer
  (setq ellama-auto-scroll t)
  ;; Sprache Deutsch (optional)
  (setq ellama-language "German")
  ;; Provider mit llama3:latest konfigurieren
  (require 'llm-ollama)
  (setq ellama-provider
        (make-llm-ollama
         :chat-model "llama3:latest"
         :embedding-model "llama3:latest"
         :default-chat-non-standard-params
         '(("num_ctx" . 8192))))
  ;; Optional: Zusammenfassung, Code, Übersetzung alles mit demselben Modell
  (setq ellama-summarization-provider
        (make-llm-ollama
         :chat-model "llama3:latest"
         :embedding-model "llama3:latest"
         :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setq ellama-coding-provider
        (make-llm-ollama
         :chat-model "llama3:latest"
         :embedding-model "llama3:latest"
         :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setq ellama-translation-provider
        (make-llm-ollama
         :chat-model "llama3:latest"
         :embedding-model "llama3:latest"
         :default-chat-non-standard-params '(("num_ctx" . 8192))))
  ;; Session-Namen automatisch generieren
  (setq ellama-naming-provider
        (make-llm-ollama
         :chat-model "llama3:latest"
         :embedding-model "llama3:latest"))
  (setq ellama-naming-scheme 'ellama-generate-name-by-llm)
  :config
  ;; Kontext- und Session-Anzeige aktivieren
  (ellama-context-header-line-global-mode +1)
  (ellama-session-header-line-global-mode +1)
  ;; Chat-Buffer Darstellung
  (setq ellama-chat-display-action-function #'display-buffer-full-frame)
  (setq ellama-instant-display-action-function #'display-buffer-at-bottom)
  ;; Scroll-Verhalten für Streaming optimieren
  (advice-add 'pixel-scroll-precision :before #'ellama-disable-scroll)
  (advice-add 'end-of-buffer :after #'ellama-enable-scroll))

(defun my/ollama-status ()
  "Prüft, ob gptel aktuell mit Ollama verbunden ist und gibt eine Testantwort aus."
  (interactive)
  (require 'gptel)
  (gptel-request
   "Sag einfach 'Ollama läuft' wenn du mich hörst."
   :callback (lambda (response info)
               (message "Backend: %s → Antwort: %s"
                        (gptel-backend-name (plist-get info :backend))
                        (string-trim (or response "Keine Antwort."))))))

;; Hiermit werden Tastenanschläge und -Kombinationen in der Mode-Line
;; angezeigt, gut für Screencasting
;; Zudem wird bei Funktionen der Funktionsname nebendran angezeigt
(use-package keycast
  :init
  (add-to-list 'global-mode-string '("" mode-line-keycast))
  ;; (keycast-mode)
  )

;; Um alles (also auch z. B. den Mini-Buffer) zu skalieren
(use-package default-text-scale
  ;; Muss es manuell über M-x installieren
  :ensure nil
  :defer t
  :bind (("C-M-+" . default-text-scale-increase)
         ("C-M-#" . default-text-scale-decrease)))

(use-package json-mode
  :defer t)

(use-package multiple-cursors
  :defer t
  :bind ("C-M-m" . 'mc/edit-lines))

(use-package diredfl
  :defer t
  :hook (dired-mode . diredfl-mode))

(use-package paredit
  :defer t
  :hook ((emacs-lisp-mode . paredit-mode)
         (clojure-mode . paredit-mode)
         (clojurescript . paredit-mode)
         (scheme-mode . paredit-mode)
         (racket-mode . paredit-mode))

  :config
  (unbind-key "C-j" paredit-mode-map)
  
  :bind ("C-M-g" . paredit-forward-down))

(use-package smartparens
  :defer t
  :config
  (require 'smartparens-config))

;;; Plantuml
;; Lade beim ersten Installieren das Jar-File herunter mit
;; `plantuml-download-jar'
(use-package plantuml-mode
  :config
  (setq plantuml-jar-path "bin/plantuml.jar")
  (setq org-plantuml-jar-path "/home/kaan/bin/plantuml.jar")
  (setq plantuml-output-type "svg"))
;;;; ---- Programming Languages ----

;;; Clojure
(use-package clojure-mode
  :defer t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))

  :config
  ;; clj-refactor don't warn when using `cljr-find-usages'
  (setq cljr-warn-on-eval nil)
  (use-package clj-refactor
    :defer t)

  :hook ((clojure-mode . (lambda () (bind-key (kbd "M-t") 'transpose-sexps 'clojure-mode-map 'clojure-mode?)))
         ;; Clojure add define-record-type Tipperleichterung
         ;; Record-Tipperleichterung
         (clojure-mode . (lambda ()
                           (load-file "~/.emacs.d/my-elisp-files/insert-define-record-type.el")
                           (bind-key (kbd "C-c C-r C-r") 'insert-define-record-type)))

         ;; Do this with dir-locals
         ;; (clojure-mode . (lambda () (add-hook 'before-save-hook 'cider-format-buffer nil t)))
         ))

;;; CIDER
(use-package cider
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

(use-package elixir-mode
  :defer t

  :hook ((elixir-mode . lsp)
         (elixir-mode . (lambda ()
                           (add-hook 'before-save-hook #'elixir-format nil t)))
         (elixir-mode . yas-minor-mode)
         (elixir-mode . smartparens-mode))

  :bind
  ("C-k" . paredit-kill)
  :config
  (setq  elixir-backend 'lsp)
  ;; Default keine Lenses, nerven nur
  (setq lsp-lens-enable nil)
  ;; Needed?
  ;; (add-to-list 'exec-path elixir-path)

  ;; Elixir settings / troubleshoot
  ;; Wenn lsp mal nicht geht (und auch z. B. linter), dann das hier ausführen:
  ;; rm -r deps _build .elixir_ls && mix deps.get
  ;; ggf. auch mix deps.compile

  ;; das hier setzen, damit emacs die deps nicht automatisch fetcht (gab bei mir
  ;; probleme bzgl mix.lock)
  ;; lsp-elixir-fetch-deps nil

  ;; overwrite fn from elixir-format https://github.com/elixir-editors/emacs-elixir/blob/master/elixir-format.el#L97C1-L105C1
  ;; because the elixir code reload constantly picks them up and complains about duplicate modules.
  ;; add ".#" in from of filename to prevent that/resp. make it easier to prevent it.
  (defun elixir-format--temp-file-path ()
    "Make a temp file in the current directory, because mix format
     applies rules based on path patterns and looks for .formatter.exs
     files in subdirectories."
    (let ((target-file-name (elixir-format--target-file-name)))
      (concat
       (file-name-directory target-file-name)
       ".#"
       (file-name-base target-file-name)
       "-emacs-elixir-format."
       (file-name-extension target-file-name))))

  )

;; Für Elixir Phoenix, inline HTML
(use-package polymode
  :mode ("\.ex$" . poly-elixir-web-mode)
  :ensure t
  :config
  (define-hostmode poly-elixir-hostmode :mode 'elixir-mode)
  (define-innermode poly-liveview-expr-elixir-innermode
    :mode 'web-mode
    :head-matcher (rx line-start (* space) "~H" (= 3 (char "\"'")) line-end)
    :tail-matcher (rx line-start (* space) (= 3 (char "\"'")) line-end)
    :head-mode 'host
    :tail-mode 'host
    :allow-nested nil
    :keep-in-mode 'host
    :fallback-mode 'host)
  (define-polymode poly-elixir-web-mode
    :hostmode 'poly-elixir-hostmode
    :innermodes '(poly-liveview-expr-elixir-innermode))
  (setq web-mode-engines-alist '(("elixir" . "\\.ex\\'"))))

(use-package lsp-mode
  :defer t
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (python-mode . lsp))

  :init
  ;; Better performance, see https://emacs-lsp.github.io/lsp-mode/page/performance/
  ;; Interesting https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
  ;; Um zu Monitoren, wie oft GC durchgeführt wird: (setq garbage-collection-messages t)
  ;; Um immer beim "Unfocus" von Emacs GC zu triggern: (add-hook 'focus-out-hook 'garbage-collect)
  (setq gc-cons-threshold 100000000) ;; 100 mb, default was 800 kb
  (setq read-process-output-max (* 1024 1024)) ;; 1 mb , default was 4096 b

  (lsp-lens-mode )
  :commands lsp)

(use-package lsp-ui
  :defer t
  :config
  (setq lsp-ui-doc-enable 1)
  (setq lsp-ui-doc-delay 2)
  ;; Dass es beim drüberhovern (ohne Maus) zeigt
  (setq lsp-ui-doc-show-with-cursor t)
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :defer t
  :commands lsp-treemacs-errors-list)

;; Needed somehow for python + lsp
(use-package all-the-icons
  :defer t)

;; if you are ivy user
(use-package lsp-ivy
  :defer t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))
  :commands lsp-ivy-workspace-symbol)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package flycheck
  :init (global-flycheck-mode))

;;; Racket
;; ATTENTION: Don't visit a file and immediately hit `C-c C-c` for a
;; repl, this somehow fails. Wait a few seconds, then it starts. I
;; think `racket-mode` starts a racket backend in the background, this
;; has to be up first.
(use-package racket-mode
  :defer t
  :hook ((racket-mode . racket-xp-mode))
  :init
  (setq racket-program "/home/kaan/.nix-profile/bin/racket"))

;;; Scala
(use-package scala-mode
  :defer t
  :interpreter
  ("scala" . scala-mode)
  :hook
  (scala-mode . lsp)

  ;; Wenn dann: (scala-mode . lsp-lens-mode) ?!?!
  ;; (lsp-mode . lsp-lens-mode)
  ;; :hook
  ;; (scala-mode . (lambda ()
  ;;                 (add-hook 'before-save-hook #'delete-trailing-whitespace)))
  ;; (scala-mode . (lambda ()
  ;;                       (add-hook 'before-save-hook #'lsp-format-buffer nil t)))
  )

(use-package sbt-mode
  :commands sbt-start sbt-command
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

(use-package lsp-metals
  :defer t)


;;; Die zwei folgenden waren bei der offiziellen metals scala emacs
;;; doku empfohlen, ich lass es mal drin
;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

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


(use-package haskell-mode
  :defer t)

;; Highlight "TODO:" In commented code
(use-package hl-todo
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode)))

(use-package envrc
  :init
  (envrc-global-mode))

(use-package default-text-scale
  :defer t
  :init
  (default-text-scale-mode)
  (let ((scale 10)) ;; 0 = Standard, 1 = etwas größer, 2 = noch größer, usw.
    (dotimes (_ scale)
      (default-text-scale-increase))))

(use-package emacs
  :init

  (setq initial-frame-alist
        '((top . 0)             ;; Oben am Bildschirm
          (left . 0)            ;; Links am Bildschirm
          (width . 100)         ;; Breite in Zeichen
          (height . 50)))       ;; Höhe in Zeilen

  ;; abbrev
  (setq-default abbrev-mode t)
  (setq save-abbrevs 'silently)

  ;; only ask for "y" and "n"
  (fset 'yes-or-no-p 'y-or-n-p)

  (winner-mode)

  ;; I have to do this, else Elixir mode is selected with Latex files
  ;;; Latex
  (add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))

  :bind (:map global-map
              :prefix-map my-prefix-map
              :prefix "C-j"
              ("C-k" . comment-or-uncomment-region)
              ("g" . counsel-rg)))

;;;; ---- Global Key Bindings ----

;;; Protokoll-Template
(load-file "~/.emacs.d/my-elisp-files/protokoll-template.el")
(bind-key "C-x p" #'protocol-template)

(bind-key "C-<tab>" #'other-window)
(bind-key "C-S-<tab>" (lambda () (interactive) (other-window -1)))
(bind-key "C-x C-y" #'yas-expand)
(bind-key "C-z" #'undo)
(bind-key "C-#" "@")
(bind-key "C-+" "~")
(bind-key "ö" "[")
(bind-key "C-c C--" "–")
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

;;; Timeclock
(bind-key "C-x t i" #'timeclock-in)
(bind-key "C-x t o" #'timeclock-out)
(bind-key "C-x t c" #'timeclock-change)

;; overwrite bc of other path
(setq timeclock-file "~/.emacs.d/timelog")

;; Für mehr Timeclock-Funktionalität
(use-package dash) ; wird von kaan-timeclock.el gebraucht
(load-file "~/.emacs.d/my-elisp-files/kaan-timeclock.el")
(bind-key "C-x t s" #'timeclock-sum-all-hours)
(bind-key "C-x t t" #'timeclock-hours-worked-today)
(bind-key "C-x t e" #'timeclock-hours-to-days-end)
(bind-key "C-x t u" #'timeclock-overtime)
(bind-key "C-x t w" #'timeclock-last-x-days-overtime)
(bind-key "C-x t f" #'timeclock-open-timelog-file)

;; Um an Timeclock einen Kommentar zu hängen
(load-file "~/.emacs.d/my-elisp-files/mikes-timeclock.el")
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

;; Damit kill-buffer ohne Confirmation direkt den Buffer killt:
(bind-key "C-x k" #'kill-current-buffer)
(bind-key "C-q" #'previous-buffer)

;;;; ---- Hooks ----

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
(load-file "~/.emacs.d/my-elisp-files/backup.el")

;;; Cursor goes to Help buffer
(setq help-window-select t)

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

;;; Vervollständigung / Filtern von read input ist case sensitive
;;;
;; Das könnte an `ido-completing-read' liegen. Das liest z. B. bei
;; `mu4e' beim Jump zu einem Maildir den Input. Da kann man einfach
;;`ido-toggle-case' ausführen, und es ist wieder insensitive

;;; Command History
;;;
;; Das hier setzen, falls command-history Duplikate zeigt
;; (setq  history-delete-duplicates t)
;; Falls man die History begrenzen möchte in der Größe
;; (setq history-length 150)
;; (setq extended-command-history nil)  ; hiermit manuell die ganze Liste löschen

;;; Bug, still there?
;;;
;; This is a bug. The variable on the left side is old but still used, but
;; cannot be found anymore.
;; TODO: Delete at next update!
;; (setq org-priority-highest org-highest-priority)
;; (setq org-priority-lowest org-lowest-priority)

;;; org-refile funktioniert nicht wegen
;;;
;; "Invalid function: org-preserve-local-variables"
;; Dann: lösche kompilierte Dateien und rekompiliere:
;; `cd ~/.emacs.d && find . -name *.elc -print0 | xargs -0 rm`
;; Danach `M-x spacemacs/recompile-elpa`

;;; Alte Netzlive Elixir Einstellungen
;;;
;; (with-eval-after-load 'lsp-mode
;;   (setq lsp-restart 'auto-restart)
;;   (setq lsp-file-watch-threshold 25000)
;;   (push "[/\\]docker/temp$" lsp-file-watch-ignored)
;;   (push "temp$" lsp-file-watch-ignored))


;;;; TODO

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
