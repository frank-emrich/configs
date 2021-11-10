(defun TeX-view-choose ()
  (interactive)
  (let*
      ((viewers (mapcar 'car TeX-view-program-list))
       (builtin-viewers (mapcar 'car TeX-view-program-list-builtin))
       (all-viewers (append viewers builtin-viewers))
       (viewers-source (helm-build-sync-source "viewers"
                         :candidates all-viewers))
       (selection (helm :sources viewers-source
                        :buffer "*helm select PDF viewer*")))

                                        ; Update selection in all latex buffers
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (string= "latex-mode" major-mode)
          (setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) selection))))))

(defun save-server-name (arg)
  "write the current server name to a file"
  (interactive "P")
  (when
      (and (boundp 'server-name) (stringp server-name))
    (write-region server-name nil "/tmp/emacs-latex-server" nil 'quiet)))

(use-package! tex
  :defer t
  :init
  ;; (setq-default TeX-master nil)
  ;; (setq TeX-auto-save t)
  ;; (setq TeX-parse-self t)
  ;; (setq TeX-display-help nil)

  ;; (setq TeX-source-correlate-mode t)
  ;; (setq TeX-source-correlate-start-server t)
  ;; (add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
  :config

  (add-to-list
   'TeX-view-program-list
   '("remote_Okular" ("/home/frank/remote_okular.sh --unique %o" (mode-io-correlate "#src:%n%a")) "/home/frank/remote_okular.sh"))
  (add-to-list
   'TeX-view-program-list
   '("remote_SumatraPDF" ("/home/frank/remote_SumatraPDF.sh -reuse-instance %o" (mode-io-correlate " -forward-search \"%b\" %n") ) "/home/frank/remote_SumatraPDF.sh"))


  :init
  ;; (add-hook 'LaTeX-mode-hook
  ;;           (lambda()
  ;;             (define-key LaTeX-mode-map (kbd "C-j") nil)
  ;;             (define-key LaTeX-mode-map (kbd "$") nil)))

  ;; ; Old logic for conditionally deleting whitespace, should not be needed with Doom's
  ;; ; fancier deletion logic
  ;; (add-hook 'LaTeX-mode-hook
  ;;           (lambda () (add-to-list 'write-file-functions 'may-delete-whitespace)))

  ;; (add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)
  ;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  ;; (add-hook 'LaTeX-mode-hook  '(lambda() (setq fill-indent-according-to-mode t)))

  ;; (company-auctex-init)

  ;; (add-hook 'LaTeX-mode-hook 'company-mode)
  ;; (add-hook 'LaTeX-mode-hook 'TeX-view-update)

  (advice-add 'TeX-command-run-all :before #'save-server-name)
  ;; (define-key LaTeX-mode-map (kbd "\"") nil)
  ;; (define-key LaTeX-mode-map (kbd "{") nil)
  ;; (define-key LaTeX-mode-map (kbd "C-c RET") nil)
  ;; (define-key LaTeX-mode-map (kbd "C-c TAB") nil)
  )
