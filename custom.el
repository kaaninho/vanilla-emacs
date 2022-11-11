(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes
   '("a7b20039f50e839626f8d6aa96df62afebb56a5bbd1192f557cb2efb5fcfb662" "fe2539ccf78f28c519541e37dc77115c6c7c2efcec18b970b16e4a4d2cd9891d" "1bddd01e6851f5c4336f7d16c56934513d41cc3d0233863760d1798e74809b4b" "42ec9eaa86da5f052feed0e35b578681015b9e21ab7b5377a5a34ea9a0a9e1b9" "4a5aa2ccb3fa837f322276c060ea8a3d10181fecbd1b74cb97df8e191b214313" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "e19ac4ef0f028f503b1ccafa7c337021834ce0d1a2bca03fcebc1ef635776bea" default))
 '(elpy-rpc-virtualenv-path 'current)
 '(exwm-floating-border-color "#3c454a")
 '(fci-rule-color "#5a6568")
 '(highlight-tail-colors ((("#33433c") . 0) (("#2f4148") . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#131313" "#ffed72"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#131313" "#a2e57b"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#131313" "#545f62"))
 '(objed-cursor-color "#ff6d7e")
 '(package-selected-packages
   '(org-mime default-text-scale defualt-text-scale-mode defualt-text-scale plantuml-mode plantuml diredfl dired-fl zweilight-theme naga-theme haskell-mode haskell counsel-projectile projectile-ripgrep json-mode pkg-info nix-mode yaml-mode yaml wgrep org-re-reveal org-appear lsp-pyright all-the-icons elpy keycast org-superstar doom-themes org-bullets diminish amx smex csv-mode csv mu4e-alert auto-highlight-symbol idle-highlight-mode marginalia projectile clj-refactor cider clojure-mode git-timemachine org-present project counsel swiper lsp-ivy lsp-ui-mode lsp-ui elixir-mode elixir lsp scala lsp-metals sbt-mode org-notify company which-key mu4e rainbow-delimiters paredit markdown-mode))
 '(pdf-view-midnight-colors (cons "#f2fffc" "#273136"))
 '(rustic-ansi-faces
   ["#273136" "#ff6d7e" "#a2e57b" "#ffed72" "#7cd5f1" "#7cd5f1" "#7cd5f1" "#f2fffc"])
 '(safe-local-variable-values
   '((eval add-hook 'before-save-hook
           (lambda nil
             (cider-format-buffer)
             (recenter-top-bottom))
           nil t)
     (eval add-hook 'before-save-hook 'cider-format-buffer nil t)))
 '(vc-annotate-background "#273136")
 '(vc-annotate-color-map
   (list
    (cons 20 "#a2e57b")
    (cons 40 "#c1e778")
    (cons 60 "#e0ea75")
    (cons 80 "#ffed72")
    (cons 100 "#ffd971")
    (cons 120 "#ffc570")
    (cons 140 "#ffb270")
    (cons 160 "#d3bd9b")
    (cons 180 "#a7c9c6")
    (cons 200 "#7cd5f1")
    (cons 220 "#a7b2ca")
    (cons 240 "#d38fa4")
    (cons 260 "#ff6d7e")
    (cons 280 "#d46977")
    (cons 300 "#a96670")
    (cons 320 "#7e6269")
    (cons 340 "#5a6568")
    (cons 360 "#5a6568")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-info ((t (:inherit default :weight bold :foreground "black" :font "DejaVu Sans Mono" :height 1.0 :underline nil))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "black" :font "DejaVu Sans Mono" :height 1.25 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "black" :font "NotoSerif" :height 1.5))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "black" :font "NotoSerif" :height 1.3))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "black" :font "NotoSerif" :height 1.15))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "black" :font "NotoSerif" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "black" :font "NotoSerif"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "black" :font "NotoSerif"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "black" :font "NotoSerif"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "black" :font "NotoSerif"))))
 '(org-warning ((t (:inherit default :weight bold :foreground "#FFAAAA" :font "DejaVu Sans Mono" :height 1.05 :underline nil)))))
