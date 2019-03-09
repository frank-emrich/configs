; requires ocaml language server & merlin


(use-package tuareg)
(use-package merlin)
;(use-package ocp-indent)




;Load the appropriate modes
(if
  (executable-find "ocamlmerlin")
  (progn
    (add-hook 'tuareg-mode-hook 'merlin-mode)
    (if
      (executable-find "ocaml-language-server")
      (add-hook 'tuareg-mode-hook 'lsp-noquery))))




; When opening ml files, warn if helpers not available
(defun check-ocaml-helpers ()
  (if
      (executable-find "ocamlmerlin")
      (progn
	(unless
	  (executable-find "ocp-indent")
	  (warn-echo-area "ocp-indent not found"))
	(unless
	  (executable-find "ocaml-language-server")
	  (warn-echo-area "ocaml-language-server not found")))
      (warn-echo-area "ocamlmerlin not found")))



(add-hook 'tuareg-mode-hook (lambda () (run-at-time "3 sec" nil 'check-ocaml-helpers)))




;(with-eval-after-load 'company
; (add-to-list 'company-backends 'merlin-company-backend))

;(add-hook 'tuareg-mode-hook #'lsp-ocaml-enable)
;(add-hook 'caml-mode-hook #'lsp-ocaml-enable)

;(add-hook 'ocaml-mode-hook 'flycheck-mode)

(use-package sml-mode)
