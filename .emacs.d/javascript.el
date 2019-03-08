(if
    (executable-find "typescript-language-server")
    (add-hook 'js-mode-hook 'lsp-noquery))



; When opening js files, warn if helpers not available
(defun check-js-helpers ()
  (unless
      (executable-find "typescript-language-server")
      (warn-echo-area "typescript/javascript language server not found")))



(add-hook 'js-mode-hook (lambda () (run-at-time "3 sec" nil 'check-js-helpers)))
