;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-emoji-aliases
   '((:woman_man_holding_hands: . ":couple:") (:thumbsup: . ":+1:")))
 '(custom-safe-themes
   '("87fa3605a6501f9b90d337ed4d832213155e3a2e36a512984f83e847102a42f4"
     default))
 '(eglot-confirm-server-edits nil nil nil "Customized with use-package eglot")
 '(elpy-rpc-virtualenv-path 'current)
 '(package-selected-packages
   '(aidermacs all-the-icons amx apheleia auctex auctex-latexmk
               auto-highlight-symbol clj-refactor company-emoji copilot
               counsel-projectile csv-mode default-text-scale diminish
               diredfl doom-themes eat elixir-mode ellama emojify envrc
               exec-path-from-shell flycheck gemini-cli
               git-timemachine gptel haskell-mode helpful hl-todo
               json-mode keycast lsp-ivy lsp-treemacs lsp-ui magit
               marginalia meow nerd-icons nix-mode obsidian org-appear
               org-mime org-present org-re-reveal org-roam-ui
               org-superstar pdf-tools plantuml-mode polymode popup
               projectile-ripgrep pyvenv racket-mode smartparens
               typescript-mode vterm wgrep yaml-mode))
 '(package-vc-selected-packages
   '((gemini-cli :url "https://github.com/linchen2chris/gemini-cli.el")))
 '(safe-local-variable-values
   '((eval progn
           (setq python-shell-interpreter
                 "/Users/kaan/Library/Caches/pypoetry/virtualenvs/lesekobold-3G8nTUWm-py3.14/bin/python")
           (setq lsp-pyright-python-executable-cmd
                 "/Users/kaan/Library/Caches/pypoetry/virtualenvs/lesekobold-3G8nTUWm-py3.14/bin/python"))))
 '(warning-suppress-log-types '((copilot copilot-no-mode-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-info ((t (:inherit default :weight bold :foreground "#f2fffc" :height 1.0 :underline nil))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#f2fffc" :height 1.25 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#f2fffc" :height 1.5))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#f2fffc" :height 1.3))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#f2fffc" :height 1.15))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#f2fffc" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#f2fffc"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#f2fffc"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#f2fffc"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#f2fffc"))))
 '(org-warning ((t (:inherit default :weight bold :foreground "#FFAAAA" :height 1.05 :underline nil)))))
