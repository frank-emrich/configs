; requires ocaml language server: npm install ocaml-language-server


;(add-to-list 'load-path "~/.emacs.d/custom-packages/caml/caml-20171209.1232")
(use-package tuareg
 ; :defer t
  )

(use-package merlin)
(use-package ocp-indent)

;; (require 'caml)
;; ;; (use-package caml
;; ;; ;  :defer t
;; ;;   )

;(use-package lsp-ocaml
;  :hook tuareg-mode
;  ;:init (add-hook 'tuareg-mode-hook #'lsp-ocaml-enable)
;  ;:after tuareg
;  )

(add-hook 'tuareg-mode-hook 'merlin-mode)

(with-eval-after-load 'company
 (add-to-list 'company-backends 'merlin-company-backend))

;(add-hook 'tuareg-mode-hook #'lsp-ocaml-enable)
;(add-hook 'caml-mode-hook #'lsp-ocaml-enable)

;(add-hook 'ocaml-mode-hook 'flycheck-mode)
