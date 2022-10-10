(defun TeX-view-choose ()
  (interactive)
  (let*
      ((viewers (mapcar 'car TeX-view-program-list))
      (builtin-viewers (mapcar 'car TeX-view-program-list-builtin))
      (all-viewers (append viewers builtin-viewers))
      (old-selection (nth 1 (assoc 'output-pdf TeX-view-program-selection)))
      (selection (ivy-read "Select PDF viewer: " all-viewers :require-match t :preselect old-selection)))

    ;; Update selection in all latex buffers
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (string= "latex-mode" major-mode)
          (setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) selection))))))

(defun TeX-view-choose-helm ()
  (interactive)
  (let*
      ((viewers (mapcar 'car TeX-view-program-list))
       (builtin-viewers (mapcar 'car TeX-view-program-list-builtin))
       (all-viewers (append viewers builtin-viewers))
       (viewers-source (helm-build-sync-source "viewers"
                         :candidates all-viewers))
       (selection (helm :sources viewers-source
                        :buffer "*helm select PDF viewer*")))

    ;; Update selection in all latex buffers
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

(defun my/fix-latex-key-bindings ()
  (dolist (key (list "^" "_" (kbd "C-c C-s")))
    (unbind-key key TeX-mode-map)))


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
   '("remote_Okular" ("/home/frank/.config/configs/scripts/remote_okular.sh --unique %o" (mode-io-correlate "#src:%n%a")) "/home/frank/.config/configs/scripts/remote_okular.sh"))
  (add-to-list
   'TeX-view-program-list
   '("SumatraPDF frank-pc TS direct" ("/home/frank/.config/configs/scripts/remote_SumatraPDF_forward.sh direct frank-pc -reuse-instance %o" (mode-io-correlate " -forward-search \"%b\" %n") ) "/home/frank/.config/configs/scripts/remote_SumatraPDF_forward.sh"))
  (add-to-list
   'TeX-view-program-list
   '("SumatraPDF frank-p50 TS direct" ("/home/frank/.config/configs/scripts/remote_SumatraPDF_forward.sh direct frank-p50 -reuse-instance %o" (mode-io-correlate " -forward-search \"%b\" %n") ) "/home/frank/.config/configs/scripts/remote_SumatraPDF_forward.sh"))
  (add-to-list
   'TeX-view-program-list
   '("SumatraPDF frank-pc TS rsync" ("/home/frank/.config/configs/scripts/remote_SumatraPDF_forward.sh rsync frank-pc -reuse-instance %o" (mode-io-correlate " -forward-search \"%b\" %n") ) "/home/frank/.config/configs/scripts/remote_SumatraPDF_forward.sh"))
  (add-to-list
   'TeX-view-program-list
   '("SumatraPDF frank-p50 TS rsync" ("/home/frank/.config/configs/scripts/remote_SumatraPDF_forward.sh rsync frank-p50 -reuse-instance %o" (mode-io-correlate " -forward-search \"%b\" %n") ) "/home/frank/.config/configs/scripts/remote_SumatraPDF_forward.sh"))
  (add-to-list
     'TeX-view-program-list
     '("display0_Okular" ("/home/frank/.config/configs/scripts/display0_okular.sh --noraise --unique %o" (mode-io-correlate "#src:%n%a")) "/home/frank/.config/configs/scripts/display0_okular.sh"))
  (add-to-list
   'TeX-view-program-list
   '("Skim sync" ("/Applications/Skim.app/Contents/SharedSupport/displayline -r %n %o %b") "/Applications/Skim.app/Contents/SharedSupport/displayline"))


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

  (add-hook 'LaTeX-mode-hook #'my/fix-latex-key-bindings)
  (add-hook 'LaTeX-mode-hook #'my/start-named-server)

  (advice-add 'TeX-command-run-all :before #'save-server-name)
  ;; (define-key LaTeX-mode-map (kbd "\"") nil)
  ;; (define-key LaTeX-mode-map (kbd "{") nil)
  ;; (define-key LaTeX-mode-map (kbd "C-c RET") nil)
  ;; (define-key LaTeX-mode-map (kbd "C-c TAB") nil)

  ;; do not format superscripts and subscripts as such
  (setq font-latex-fontify-script nil)
  )

(defun my/LaTeX-should-format-at-point (point)
  (save-excursion
    (goto-char point)
    (and (not (texmathp)) (string-equal (LaTeX-current-environment) "document"))))


(defun my/LaTeX-format ()
  (interactive)

  (if (use-region-p)
      (call-interactively 'LaTeX-fill-region)
    (save-mark-and-excursion
      (goto-char (point-min))

      (while (< (point) (point-max))
        (forward-sentence)

        (if (and (not (TeX-current-macro)) (my/LaTeX-should-format-at-point (point)))
            (progn
              (backward-char)
              (if (or (looking-at "? ") (looking-at "\. ") (looking-at "! "))
                  (progn
                    (forward-char)
                    (insert "\n%\n")
                    (delete-char 1 nil))
                (forward-char))

              ))))
    (goto-char (point-min))
    (while (< (point) (point-max))
      (forward-sentence)

      (let ((my-sentence-end (point))
            (my-sentence-start (save-mark-and-excursion (backward-sentence) (point))))

        (if (and (not (TeX-current-macro)) (my/LaTeX-should-format-at-point (point)))
            (progn
              (LaTeX-fill-region my-sentence-start my-sentence-end)
              ;; wrapping the filling in save-excursion doesn't work
              (if (<= my-sentence-end (point-max))
                  (goto-char my-sentence-end))))))))

