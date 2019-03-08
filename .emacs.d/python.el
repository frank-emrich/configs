
; pip install jedi

; (use-package company-jedi)

;(defun my/python-mode-hook ()
;  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'lsp-noquery)
 (if
     (executable-find "pyls") ; python language server
     (add-hook 'python-mode-hook 'lsp-noquery))


; When opening python files, warn if helpers not available
(defun check-python-helpers ()
  (unless
      (executable-find "pyls")
      (warn-echo-area "python language server not found")))



(add-hook 'python-mode-hook (lambda () (run-at-time "3 sec" nil 'check-python-helpers)))
