;(use-package agda2-mode)


;(load "custom-packages/agda2-mode")
;(use-package agda2-mode
;  :load-path "custom-packages/agda")
;(require 'agda2-mode)



(defun init-agda-mode ()
   (let ((coding-system-for-read 'utf-8))
     (if
	 (executable-find "agda-mode")
         (progn
	   (load-file (shell-command-to-string "agda-mode locate"))
	   (agda2-mode)
	   (agda2-load))
       (warn-echo-area "agda-mode not found"))))

(add-to-list 'auto-mode-alist '("\\lagda.md\\'" . init-agda-mode))
(add-to-list 'auto-mode-alist '("\\.agda\\'"    . init-agda-mode))
(add-to-list 'auto-mode-alist '("\\.lagda\\'"   . init-agda-mode))
