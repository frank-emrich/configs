;; Replaced by window-purpose
;; (use-package shackle
;;   :config
;;   (progn
;;     (shackle-mode 1)
;;     ;(setq helm-display-function #'pop-to-buffer)
;;     (setq shackle-rules
;;       '(("*TeX errors*" :popup t :align 'right :ratio 0.10)))))



;; (defun display-buffer-below-given (window-to-split buffer alist)
;;   "Try displaying BUFFER in a window below the selected window.
;; If there is a window below the selected one and that window
;; already displays BUFFER, use that window.  Otherwise, try to
;; create a new window below the selected one and show BUFFER there.
;; If that attempt fails as well and there is a non-dedicated window
;; below the selected one, use that window."
;;   (let (window)
;;     (or (and (setq window (window-in-direction 'below))
;; 	     (eq buffer (window-buffer window))
;; 	     (window--display-buffer buffer window 'reuse alist))
;; 	(and (not (frame-parameter nil 'unsplittable))
;; 	     (let ((split-height-threshold 0)
;; 		   split-width-threshold)
;; 	       (setq window (window--try-to-split-window
;;                              window-to-split alist)))
;; 	     (window--display-buffer
;; 	      buffer window 'window alist display-buffer-mark-dedicated))
;; 	(and (setq window (window-in-direction 'below))
;; 	     (not (window-dedicated-p window))
;; 	     (window--display-buffer
;; 	      buffer window 'reuse alist display-buffer-mark-dedicated)))))



;; Shared logic for helm and purpose windows

(defconst popup-size-percentage 0.3)

(defun all-children-have-purpose (window purpose)
  "Return true if window is a leave with the given purpose or an
   internal window all of whose children satisfy the check.  Returns
   nil if window is nil"
  (when window
    (unless (windowp window)
      (warn "Type of window is %s " (type-of window))
      (error "Passed non-window to all-children-have-purpose"))
    (if (window-live-p window) ;; is window live? (= not internal, not deleted)
	(eq (purpose-window-purpose window) purpose)
      (let ((cur-child (window-child window))
	    (result t))
	(while (and cur-child result)
	  (setq result (and result (all-children-have-purpose cur-child purpose)))
	  (setq cur-child (window-next-sibling cur-child)))
	result))))



(defun purpose-window-purpose-safe (&optional window)
  "The original purpose-window-purpose fails on windows without
   buffers (i.e., non-ive ones, like internal or dead). This version
   returns nil for such windows. Uses the active window if window is nil."
  (let ((actual-window (or window (selected-window))))
    (when (window-live-p actual-window)
      (purpose-window-purpose actual-window))))


;; advice :around split-window
;; it's important that this works correctly if window is nil,
;; then using the active window.
(defun split-window-main-window-aware (orig-fun &rest args)
  "Replacement of split-window that, if purpose is enabled, makes sure
   that main windows are always split in a way such that a *new*
   parent window is created, containing the existing window and the
   newly created one. In particular, it prevents reusing an existing
   internal window as the parent."
  (if purpose--active-p ;; this variable is used by without-purpose
      (let* ((win (car args))
	     (win-purpose (purpose-window-purpose-safe win))

	     ;; temporarily override window-combination-limit if splitting
	     ;; 'main window
	     (window-combination-limit
	      (if (eq win-purpose 'main)
		  t
		window-combination-limit)))
	(apply orig-fun args))
    (apply orig-fun args)))


(defun purpose-any-window-with-purpose (purpose)
  (let ((buffs (purpose-buffers-with-purpose purpose)))
    (get-buffer-window (car buffs))))

(defun purpose-furthest-parent-with-all-purpose-children (window purpose)
 "Walk the parent-chain from WINDOW upwards, starting at WINDOW
  itself.  Returns the furthest parent p such that
  (all-children-have-purpose p purpose) still holds. Returns nil if
  window is originally nil."
 (when window
   (if  (all-children-have-purpose window purpose)
       (let ((cur-window (window-parent window))
	     (prev-window window))
	 (while (all-children-have-purpose cur-window purpose)
	   (setq prev-window cur-window)
	   (setq cur-window (window-parent cur-window)))
	 prev-window)
     (error "Passed initial window to
     purpose-furthest-parent-with-all-purpose-children that doesn't
     have the given purpose itself"))))





;; Functions specific to handling purpose-managed windows

(defun purpose-popup-creation-function (buffer alist &optional height)
  (purpose-display--at
   #'(lambda ()
       (let
	   (main-window (purpose-any-window-with-purpose 'main))
	 (window-in-direction 'below main-window)))
   #'(lambda ()
       (let* ((height (purpose--normalize-height popup-size-percentage))
               (height (when height (- height)))
	       (main-window (purpose-any-window-with-purpose 'main)))
         (ignore-errors
	   (with-selected-window main-window
	     (split-window main-window height 'below)))))
   buffer
   alist))


;; Functions specific to handling helm windows


;; Buffers whose name matches one of these will be put into the info purpose
;; rather than being handled as popups
(defconst helm-info-buffers-regexps
  '("helm-ag" "Helm Swoop"))



(defun create-new-helm-popup-window (buffer _resume)
  (let*
      ((height (purpose--normalize-height popup-size-percentage))
       (height (when height (- height)))
       (outermost-main-containing-window
	(purpose-furthest-parent-with-all-purpose-children
	 (purpose-any-window-with-purpose 'main) 'main))
       (main-window
	(or
	 outermost-main-containing-window
	 (frame-root-window)))
       (new-window
	;;(with-selected-window main-window
	(without-purpose ;; to disable our own split-window advice
		     (split-window main-window height 'below)
		     )
	))
    (select-window new-window)
    (set-window-buffer new-window buffer)
    (without-purpose
      (switch-to-buffer buffer t t))))


(defun my-helm-display-function (buffer _resume)
  (let* ((name
	  (if (stringp buffer) buffer (buffer-name buffer)))
	 (matched
	  (seq-find
	   (lambda (re) (string-match re name))
	   helm-info-buffers-regexps))
	 (info-window (purpose-any-window-with-purpose 'info)))
    (if (and matched info-window)
	 (progn
	   (set-window-buffer info-window buffer)
	   (select-window info-window)
	   (without-purpose
	     (switch-to-buffer buffer t t)))
      (create-new-helm-popup-window buffer _resume))))



;; Loading


(use-package window-purpose
  :ensure t
  ;; :after (undo-tree helm)
  :init (require 'helm)
  :config
    (purpose-mode)
    (require 'window-purpose-x)
    (purpose-x-popwin-setup)
    ;; (set 'purpose-x-popwin-position nil)
    ;; (fset 'purpose-x-popwin-position 'purpose-popup-creation-function)
    (setq purpose-x-popwin-position (quote purpose-popup-creation-function))
    ;;(setq default-purpose 'main)
    ;;(setq purpose-use-default-configuration t) ;; use default-purpose if no other match

    (setq purpose-user-mode-purposes
          '(
            (tuareg-mode . main)
            (magit-diff-mode . main)
	    (magit-log-mode . main)
	    (custom-mode . main)
            (magit-status-mode . info)
            (compilation-mode . info)
	    (prog-mode . main)
	    (text-mode . main)
	    (fundamental-mode . main)
	    ;; in Emacs 24.5-, `css-mode' doesn't derive from `prog-mode'
	    (css-mode . main)
	    (comint-mode . main)
	    (eshell-mode . main)
	    (term-mode . main)
	    (dired-mode . popup)
	    (TeX-output-mode . info)
	    (occur-mode . info)
	    (grep-mode . info)
	    (compilation-mode . info)
	    (image-mode . image)
	    (package-menu-mode . main)
	    ;; (undo-tree-visualizer-mode . messages)
            ;; (treemacs-mode . side-info)
	    ))
   (add-to-list 'purpose-user-name-purposes '("*Messages*" . messages))
   (add-to-list 'purpose-user-name-purposes '("*Ilist*" . ilist))
   (add-to-list 'purpose-user-name-purposes '("*Warnings*" . messages))
   (add-to-list 'purpose-user-name-purposes '("*undo-tree Diff*" . popup))
   (add-to-list 'purpose-user-name-purposes '(" *undo-tree*" . info)) ;; note the leading space
   (add-to-list 'purpose-user-name-purposes '("*scratch*" . info))
   (add-to-list 'purpose-user-name-purposes '("Treemacs" . treemacs))
   (add-to-list 'purpose-user-name-purposes '("*magit-diff-popup*" . messages))
   (add-to-list 'purpose-user-name-purposes '("*Help*" . info))
   (add-to-list 'purpose-user-regexp-purposes '(".*Customize Group.*" . main))
   (add-to-list 'purpose-user-regexp-purposes '(".*Flycheck errors.*" . messages))

   (add-to-list 'purpose-user-name-purposes '(".gitignore" . main))
   (add-to-list 'purpose-user-name-purposes '("COMMIT_EDITMSG" . info))
   (add-to-list 'purpose-user-name-purposes '(".hgignore" . main))
   (add-to-list 'purpose-user-name-purposes '("*shell*" . terminal))
   (add-to-list 'purpose-user-name-purposes '("*TeX errors*" . messages))


   (add-to-list 'purpose-user-regexp-purposes '("^ \\*Minibuf-[0-9]*\\*$" . minibuf))






   ;;(add-to-list 'purpose-x-popwin-buffer-names "*magit-diff-popup*")
   ;; (add-to-list 'purpose-user-name-purposes '("*Ilist*" . side-info))

   ;; (add-to-list 'purpose-special-action-sequences
   ;;           '(main
   ;;             purpose-display-reuse-window-buffer
   ;;             purpose-display-reuse-window-purpose
   ;;             purpose-display-pop-up-window))
   ;; (add-to-list 'purpose-special-action-sequences
   ;;           '(messages
   ;;             purpose-display-reuse-window-buffer
   ;;             purpose-display-reuse-window-purpose
   ;;             purpose-display-pop-up-window))
   ;; (add-to-list 'purpose-special-action-sequences
   ;;           '(info
   ;;             purpose-display-reuse-window-buffer
   ;;             purpose-display-reuse-window-purpose
   ;;             purpose-display-pop-up-window))
   (purpose-compile-user-configuration)
   ;; Prevent purpose from stealing these bindings
   (define-key purpose-mode-map (kbd "C-x f") nil)
   (define-key purpose-mode-map (kbd "C-x C-f") nil)
   (define-key purpose-mode-map (kbd "C-x b") nil)
   (define-key purpose-mode-map (kbd "C-x C-b") nil)

   ;; install our own function for creating helm popups
   (setq helm-display-function #'my-helm-display-function)


   ;; modify split-window with special handling of 'main windows
   ;; maybe this should go somewhere else?
   (advice-add 'split-window :around #'split-window-main-window-aware)
)


(defalias 'helm-find-files-without-purpose
  (without-purpose-command #'helm-find-files))


(defalias 'helm-for-files-without-purpose
  (without-purpose-command #'helm-for-files))


(defalias 'helm-buffers-list-without-purpose
  (without-purpose-command #'helm-buffers-list))

(define-key global-map (kbd "C-x C-f") 'helm-find-files-without-purpose)
(define-key global-map (kbd "C-x b") 'helm-for-files-without-purpose)
(define-key global-map (kbd "C-x C-b") 'helm-buffers-list-without-purpose)
