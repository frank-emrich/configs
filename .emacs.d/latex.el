


(use-package company-auctex
  :defer t
)

(use-package tex
  :defer t
  :ensure auctex
  :init
    (setq-default TeX-master nil)
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq TeX-source-correlate-mode t)
    (setq TeX-source-correlate-start-server t)
  :config
    (setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "Okular")
    (add-hook 'LaTeX-mode-hook
          (lambda() (define-key LaTeX-mode-map (kbd "C-j") nil)))
    (add-hook 'LaTeX-mode-hook 'imenu-list-minor-mode)
    (add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)
    (company-auctex-init)
    (add-hook 'LaTeX-mode-hook 'company-mode)
)



;; ; lsp latex stuff :

;; (add-to-list 'load-path "~/.emacs.d/custom-packages/lsp-latex/lsp-latex")
;; (require 'lsp-latex)
;; ;; "texlab.jar" must be located at a directory contained in `exec-path'
;; (setq lsp-latex-texlab-jar-file "/home/frank/.emacs.d/custom-packages/lsp-latex/texlab/build/libs/texlab.jar")
;; ;; If you want to put "texlab.jar" somewhere else,
;; ;; you can specify the path to "texlab.jar" as follows:
;; ;; (setq lsp-latex-texlab-jar-file "/path/to/texlab.jar")

;; (with-eval-after-load "tex-mode"
;;  (add-hook 'tex-mode-hook 'lsp)
;;  (add-hook 'latex-mode-hook 'lsp))
