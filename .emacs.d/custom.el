(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote newframe))
 '(anzu-minimum-input-length 2)
 '(custom-safe-themes
   (quote
    ("36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(desktop-restore-frames nil)
 '(diff-hl-margin-mode t)
 '(gdb-many-windows t)
 '(git-gutter:update-interval 2)
 '(helm-advice-push-mark nil)
 '(helm-boring-buffer-regexp-list
   '("\\` " "\\`\\*helm for files" "\\`\\*helm find files" "\\`\\*Echo Area" "\\`\\*Minibuf"))
 '(helm-split-window-inside-p t)
 '(helm-swoop-split-with-multiple-windows t)
 '(helm-use-frame-when-more-than-two-windows nil)
 '(inhibit-startup-screen t)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-imenu-enable nil)
 '(lsp-ui-peek-enable nil)
 '(lsp-ui-sideline-enable nil)
 '(magit-bury-buffer-function 'magit-mode-quit-window)
 '(package-selected-packages
   (quote
    (window-purpose flycheck visual-regexp-steroids nadvice spinner auctex company-auctex racket-mode delight treemacs highlight-thing shackle imenu-list hydra windsize back-button helm-ag magit sml-mode pc-bufsw golden-ratio-scroll-screen avy multiple-cursors move-text rainbow-delimiters helm-swoop helm-descbinds helm git-gutter git-timemachine undo-tree lsp-haskell haskell-mode merlin tuareg company-lsp lsp-ui lsp-mode company use-package)))
 '(recentf-max-saved-items 40)
 '(safe-local-variable-values
   (quote
    ((ispell-local-dictionary . american)
     (ispell-local-dictionary . en_US)
     (ignore-trailing-whitespace . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-mode-line ((t (:foreground "color-220" :weight bold))))
 '(anzu-mode-line-no-match ((t (:inherit color-220))))
 '(lsp-face-highlight-read ((t (:background "gray13" :underline t))))
 '(lsp-face-highlight-textual ((t (:background "gray13" :underline t))))
 '(lsp-face-highlight-write ((t (:inherit nil :background "gray13" :underline t))))
 '(lsp-headerline-breadcrumb-separator-face ((t (:inherit aw-key-face))))
 '(lsp-headerline-breadcrumb-symbols-face ((t (:inherit avy-background-face))))
 '(lsp-ui-sideline-current-symbol ((t (:background "gray13" :foreground "white" :box (:line-width 2 :color "white") :weight bold :height 0.99))))
 '(merlin-type-face ((t (:background "gray13"))))
 '(origami-fold-replacement-face ((t (:background "green"))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "gold"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "green"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "magenta"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "dark turquoise"))))
 '(smerge-base ((t nil)))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "color-22"))))
 '(smerge-refined-removed ((t (:inherit smerge-refined-change :background "color-52")))))
