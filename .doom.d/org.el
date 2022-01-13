(defun enable-tex-mode-input ()
  (set-input-method "TeX"))

(add-hook! org-mode 'enable-tex-mode-input)
