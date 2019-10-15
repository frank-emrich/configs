


(use-package company-auctex
  :defer t
)



(defun save-server-name (arg)
  "write the current server name to a file"
  (interactive "P")
  (when
      (and (boundp 'server-name) (stringp server-name))
    (write-region server-name nil "/tmp/emacs-latex-server" nil 'quiet)))




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

(defun guess-TeX-viewer (client-string file-path)
  (cond
    ((not (stringp client-string))
     "Okular")
    ((and (string-prefix-p "192.168.0." client-string)
	  (string-prefix-p "/machines/frank-lpc/paper-gits/" file-path))
     "remote_SumatraPDF")
    ((string-prefix-p "129.215" client-string)
     "remote_Okular")
    (t "Okular")))

(defun TeX-view-update ()
  "Sets viewer to the one used in other buffer or if no such buffer
   uses guess-TeX-viewer"
  (interactive)
  (make-local-variable 'TeX-view-program-selection)

  (let*
      ((ssh-client-string (getenv "SSH_CONNECTION"))
       (cur-file buffer-file-name)
       (determined-viewer (guess-TeX-viewer ssh-client-string cur-file))
       (existing-viewer nil))

    ; Check if there is another latex buffer already
    (dolist (b (buffer-list))
      (unless (eq (current-buffer) b)
	(with-current-buffer b
	  (when (string= "latex-mode" major-mode)
	    (setq existing-viewer (car (cdr (assoc 'output-pdf TeX-view-program-selection))))))))

    (let ((viewer  (or existing-viewer determined-viewer)))
      (setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) viewer)
      (when (called-interactively-p 'any)
	(warn-echo-area (concat "Setting pdf viewer to " viewer))))))





(use-package tex
  :defer t
  :ensure auctex
  :init
    (setq-default TeX-master nil)
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq TeX-display-help nil)
    (setq TeX-error-overview-open-after-TeX-run t)
    (setq TeX-source-correlate-mode t)
    (setq TeX-source-correlate-start-server t)
  :config
    ;(setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "Atril-no-dbus")
    ;(setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "remote_Okular")
    ;(setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) (viewer-from-ssh-client (getenv "SSH_CONNECTION")))
    (add-to-list
     'TeX-view-program-list
     '("Atril-no-dbus" ("atril" (mode-io-correlate " -i %(outpage)") " %o") "atril"))
    (add-to-list
     'TeX-view-program-list
     '("Atril-no-dbus-display-1" ("atril" (mode-io-correlate " --display :1 -i %(outpage)") " %o") "atril"))
    ;(add-to-list
    ; 'TeX-view-program-list
    ; '("Atril-remote" ("/home/frank/remote_atril.sh" (mode-io-correlate " -i %(outpage)") " %o") "/home/frank/remote_atril.sh"))
    (add-to-list
     'TeX-view-program-list
     '("remote_Okular" ("/home/frank/remote_okular.sh --unique %o" (mode-io-correlate "#src:%n%a")) "/home/frank/remote_okular.sh"))
    (add-to-list
     'TeX-view-program-list
     '("remote_SumatraPDF" ("/home/frank/remote_SumatraPDF.sh -reuse-instance %o" (mode-io-correlate " -forward-search \"%b\" %n") ) "/home/frank/remote_SumatraPDF.sh"))
    (add-to-list
     'TeX-view-program-list
     '("display1_Okular" ("/home/frank/display1_okular.sh --noraise --unique %o" (mode-io-correlate "#src:%n%a")) "/home/frank/display1_okular.sh"))
    (add-to-list
     'TeX-view-program-list
     '("Okular" ("okular --noraise --unique %o" (mode-io-correlate "#src:%n%a")) "okular"))

    (add-hook 'LaTeX-mode-hook
          (lambda()
	    (define-key LaTeX-mode-map (kbd "C-j") nil)
	    (define-key LaTeX-mode-map (kbd "$") nil)))
    (add-hook 'LaTeX-mode-hook
	  (lambda () (add-to-list 'write-file-functions 'may-delete-whitespace)))
    (add-hook 'LaTeX-mode-hook 'imenu-list-minor-mode)
    (add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (company-auctex-init)
    (add-hook 'LaTeX-mode-hook 'company-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-view-update)
    (advice-add 'TeX-LaTeX-sentinel :after #'my/TeX-LaTeX-sentinel)
    (advice-add 'TeX-command-run-all :before #'save-server-name)
)


(add-hook 'bibtex-mode-hook (lambda () (define-key bibtex-mode-map (kbd "C-j") nil)))
(add-hook 'BibTeX-mode-hook (lambda () (define-key bibtex-mode-map (kbd "C-j") nil)))

; auto closing of error buffer:

(defun my/TeX-LaTeX-sentinel (process name)
  "If showing error overview, re-focus on the tex window"
  (if (window-live-p TeX-error-overview-orig-window)
       (select-window TeX-error-overview-orig-window)))



(defun TeX-error-delete-window ()
  "Delete TeX error window when there are no errors to show."
  (let ((w (get-buffer-window))
    (b (get-buffer "*TeX Help*")))
    (when w
      (delete-window w))
    (when b
      (setq w (get-buffer-window b))
      (when w
    (delete-window w)))))

(defun TeX-error-install-delete-window-hook ()
  "Install `TeX-error-delete-window' in buffer-local `kill-buffer-hook'."
  (add-hook 'kill-buffer-hook #'TeX-error-delete-window nil t))

(add-hook 'TeX-error-overview-mode-hook #'TeX-error-install-delete-window-hook)

(defun beautify-unicode ()
  (interactive)
    (let ((case-fold-search nil)
	  (replacements
	   '(
	     ("\\forall" "∀")
	     ("\\Delta" "Δ")
	     ;("\\delta" "δ")
	     ("\\vdash" "꜔")
	     ;("\\theta" "θ")
	     ("\\Theta" "Θ")
	     ("\\lambda" "λ")
	     ("\\Lambda" "Λ")
	     ("\\alpha" "α")
	     ("\\Gamma" "Γ")
	     ("\\not\\in" "∉")
	     ("\\in" "∈")
	    )))

      (dolist (elt replacements)
	(goto-char (point-min))
	;(message (car (cdr elt))))
	(while (search-forward (car elt) nil t)
	  (replace-match (car (cdr elt)))
	))

      ;; (goto-char (point-min))
      ;; (while (search-forward "\\forall" nil t)
      ;; 	(replace-match "∀"))

      ;; (goto-char (point-min))
      ;; (while (search-forward "\\Delta" nil t)
      ;; 	(replace-match "Δ"))

      ;; repeat for other string pairs
      ))


;(add-hook 'TeX-after-compilation-finished-hook 'handle-TeX-help)

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
