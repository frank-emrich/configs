(defun my/enable-tex-mode-input ()
  (set-input-method "TeX")
  ;; disable, so it's not active by default
  (toggle-input-method))


;; TODO: only create tmp file if remote
(defun my/add-latex-macros-to-org-header (&rest r)
  (unless macros-added-to-latex-header
    (let ;; ((macros-file  (expand-file-name (concat org-directory "macros.tex"))))
        ((macros-file  (concat default-directory "macros.tex")))
      (when (file-exists-p macros-file)
        (let ((tmp-macros-file (make-temp-file "org-macros")))
          (copy-file macros-file tmp-macros-file t)
          (setq org-format-latex-header
                (concat original-org-format-latex-header "\\input{" tmp-macros-file "}")))))
    (setq macros-added-to-latex-header t)))



(defun my/org-setup ()
  (my/enable-tex-mode-input)

  (plist-put org-format-latex-options :scale 1.0))


(add-hook! org-mode #'my/org-setup)

(after! org


  ;; prevents org mode from stealing shift + arrow bindings
  (setq org-support-shift-select 'always)

  (defun my/org-create-formula-image
      (old-fun &rest args)
    "The actual org-create-formula-image uses absolute /tmp paths but gets
     confused if default-directory is a remote dir"
    (let ((default-directory "/tmp"))
      ;;((temporary-file-directory (temporary-file-directory)))
      (apply old-fun args)))
  (advice-add 'org-create-formula-image :around #'my/org-create-formula-image)

  (defvar original-org-format-latex-header org-format-latex-header)

  (setq org-preview-latex-default-process 'dvipng)

  ;; Note that we also set the scale value in my/org-setup more cleanly.
  ;; The value here affects files with +STARTUP: latexpreview
  (setq org-format-latex-options
        '(:foreground default
          :background default
          :scale 1.0
          :html-foreground "Black"
          :html-background "Transparent"
          :html-scale 1.0 :matchers
          ("begin" "$1" "$" "$$" "\\(" "\\[")))


  ;; org-latex-preview is called very early during org setup when using
  ;; +STARTUP: latexpreview, before any mode hooks run.
  ;; Make sure that we insert the latex macros exactly once.
  (defvar-local macros-added-to-latex-header nil)
  (advice-add 'org-latex-preview :before #'my/add-latex-macros-to-org-header))

;; (defun my/org-compile-file (source process ext &optional err-msg log-buf spec)
;;   "Compile a SOURCE file using PROCESS.

;; PROCESS is either a function or a list of shell commands, as
;; strings.  EXT is a file extension, without the leading dot, as
;; a string.  It is used to check if the process actually succeeded.

;; PROCESS must create a file with the same base name and directory
;; as SOURCE, but ending with EXT.  The function then returns its
;; filename.  Otherwise, it raises an error.  The error message can
;; then be refined by providing string ERR-MSG, which is appended to
;; the standard message.

;; If PROCESS is a function, it is called with a single argument:
;; the SOURCE file.

;; If it is a list of commands, each of them is called using
;; `shell-command'.  By default, in each command, %b, %f, %F, %o and
;; %O are replaced with, respectively, SOURCE base name, name, full
;; name, directory and absolute output file name.  It is possible,
;; however, to use more place-holders by specifying them in optional
;; argument SPEC, as an alist following the pattern

;;   (CHARACTER . REPLACEMENT-STRING).

;; When PROCESS is a list of commands, optional argument LOG-BUF can
;; be set to a buffer or a buffer name.  `shell-command' then uses
;; it for output."
;;   (message "default-directory %s" default-directory)
;;   (message "source %s" source)
;;   (let* ((base-name (file-name-base source))
;; 	 (full-name (file-truename source))
;; 	 (out-dir (or (file-name-directory source) "./"))
;; 	 (output (expand-file-name (concat base-name "." ext) out-dir))
;; 	 (time (current-time))
;; 	 (err-msg (if (stringp err-msg) (concat ".  " err-msg) "")))
;;     (message "base name %s" base-name)
;;     (message "full name %s" full-name)
;;     (save-window-excursion
;;       (pcase process
;; 	((pred functionp) (funcall process (shell-quote-argument source)))
;; 	((pred consp)
;; 	 (let ((log-buf (and log-buf (get-buffer-create log-buf)))
;; 	       (spec (append spec
;; 			     `((?b . ,(shell-quote-argument base-name))
;; 			       (?f . ,(shell-quote-argument source))
;; 			       (?F . ,(shell-quote-argument full-name))
;; 			       (?o . ,(shell-quote-argument out-dir))
;; 			       (?O . ,(shell-quote-argument output))))))
;; 	   (dolist (command process)
;;              (message (format-spec command spec))
;; 	     (shell-command (format-spec command spec) log-buf))
;; 	   (when log-buf (with-current-buffer log-buf (compilation-mode)))))
;; 	(_ (error "No valid command to process %S%s" source err-msg))))
;;     ;; Check for process failure.  Output file is expected to be
;;     ;; located in the same directory as SOURCE.
;;     (unless (org-file-newer-than-p output time)
;;       (error (format "File %S wasn't produced%s" output err-msg)))
;;     output))


;; (advice-add 'org-compile-file :override #'my/org-compile-file)
