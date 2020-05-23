; requires ocaml language server & merlin


(use-package tuareg
  :defer t)

(use-package merlin
  :commands merlin-mode
  :config
  ; dont check for errors after saving
  (setq merlin-error-after-save nil)

  ; update errors, then move line (irrelevant now?)
  (setq merlin-error-check-then-move nil)

  ; Only highlight first line of multi-line error messages
  (setq merlin-error-on-single-line t)
)

(use-package sml-mode
  :defer t)

(setq lsp-ocaml-lsp-server-command "ocamllsp")

;(with-eval-after-load 'lsp-clients)




;Load the appropriate modes
(if
  (executable-find "ocamlmerlin")
  (progn
    (add-hook 'tuareg-mode-hook 'merlin-mode)
    (if
      (executable-find lsp-ocaml-lsp-server-command)
      (progn
	(add-hook 'tuareg-mode-hook 'lsp-noquery)))))





; When opening ml files, warn if helpers not available
(defun check-ocaml-helpers ()
  (if
      (executable-find "ocamlmerlin")
      (progn
	(unless
	  (executable-find "ocp-indent")
	  (warn-echo-area "ocp-indent not found"))
	(unless
	  (executable-find lsp-ocaml-lsp-server-command)
	  (warn-echo-area "ocaml lsp server execuable not found")))
      (warn-echo-area "ocamlmerlin not found")))



(add-hook 'tuareg-mode-hook (lambda () (run-at-time "3 sec" nil 'check-ocaml-helpers)))




;(with-eval-after-load 'company
; (add-to-list 'company-backends 'merlin-company-backend))

;(add-hook 'tuareg-mode-hook #'lsp-ocaml-enable)
;(add-hook 'caml-mode-hook #'lsp-ocaml-enable)

;(add-hook 'ocaml-mode-hook 'flycheck-mode)
