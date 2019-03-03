(require 'package)


(setq load-prefer-newer t) ;do not load outdated .elc files



; correctly handle tmux' xterm keys
;(defadvice terminal-init-screen
;  ;; The advice is named `tmux', and is run before `terminal-init-screen' runs.
;  (before tmux activate)
;  ;; Docstring.  This describes the advice and is made available inside emacs;
;  ;; for example when doing C-h f terminal-init-screen RET
;  "Apply xterm keymap, allowing use of keys passed through tmux."
;  ;; This is the elisp code that is run before `terminal-init-screen'.
;  (if (getenv "TMUX")
;    (let ((map (copy-keymap xterm-function-map)))
;    (set-keymap-parent map (keymap-parent input-decode-map))
;    (set-keymap-parent input-decode-map map))))


;; handle tmux's xterm-keys
;; put the following line in your ~/.tmux.conf:
;;   setw -g xterm-keys on
(if (getenv "TMUX")
    (progn
      (let ((x 2) (tkey ""))
    (while (<= x 8)
      ;; shift
      (if (= x 2)
          (setq tkey "S-"))
      ;; alt
      (if (= x 3)
          (setq tkey "M-"))
      ;; alt + shift
      (if (= x 4)
          (setq tkey "M-S-"))
      ;; ctrl
      (if (= x 5)
          (setq tkey "C-"))
      ;; ctrl + shift
      (if (= x 6)
          (setq tkey "C-S-"))
      ;; ctrl + alt
      (if (= x 7)
          (setq tkey "C-M-"))
      ;; ctrl + alt + shift
      (if (= x 8)
          (setq tkey "C-M-S-"))

      ;; arrows
      (define-key key-translation-map (kbd (format "M-[ 1 ; %d A" x)) (kbd (format "%s<up>" tkey)))
      (define-key key-translation-map (kbd (format "M-[ 1 ; %d B" x)) (kbd (format "%s<down>" tkey)))
      (define-key key-translation-map (kbd (format "M-[ 1 ; %d C" x)) (kbd (format "%s<right>" tkey)))
      (define-key key-translation-map (kbd (format "M-[ 1 ; %d D" x)) (kbd (format "%s<left>" tkey)))
      ;; home
      (define-key key-translation-map (kbd (format "M-[ 1 ; %d H" x)) (kbd (format "%s<home>" tkey)))
      ;; end
      (define-key key-translation-map (kbd (format "M-[ 1 ; %d F" x)) (kbd (format "%s<end>" tkey)))
      ;; page up
      (define-key key-translation-map (kbd (format "M-[ 5 ; %d ~" x)) (kbd (format "%s<prior>" tkey)))
      ;; page down
      (define-key key-translation-map (kbd (format "M-[ 6 ; %d ~" x)) (kbd (format "%s<next>" tkey)))
      ;; insert
      (define-key key-translation-map (kbd (format "M-[ 2 ; %d ~" x)) (kbd (format "%s<delete>" tkey)))
      ;; delete
      (define-key key-translation-map (kbd (format "M-[ 3 ; %d ~" x)) (kbd (format "%s<delete>" tkey)))
      ;; f1
      (define-key key-translation-map (kbd (format "M-[ 1 ; %d P" x)) (kbd (format "%s<f1>" tkey)))
      ;; f2
      (define-key key-translation-map (kbd (format "M-[ 1 ; %d Q" x)) (kbd (format "%s<f2>" tkey)))
      ;; f3
      (define-key key-translation-map (kbd (format "M-[ 1 ; %d R" x)) (kbd (format "%s<f3>" tkey)))
      ;; f4
      (define-key key-translation-map (kbd (format "M-[ 1 ; %d S" x)) (kbd (format "%s<f4>" tkey)))
      ;; f5
      (define-key key-translation-map (kbd (format "M-[ 15 ; %d ~" x)) (kbd (format "%s<f5>" tkey)))
      ;; f6
      (define-key key-translation-map (kbd (format "M-[ 17 ; %d ~" x)) (kbd (format "%s<f6>" tkey)))
      ;; f7
      (define-key key-translation-map (kbd (format "M-[ 18 ; %d ~" x)) (kbd (format "%s<f7>" tkey)))
      ;; f8
      (define-key key-translation-map (kbd (format "M-[ 19 ; %d ~" x)) (kbd (format "%s<f8>" tkey)))
      ;; f9
      (define-key key-translation-map (kbd (format "M-[ 20 ; %d ~" x)) (kbd (format "%s<f9>" tkey)))
      ;; f10
      (define-key key-translation-map (kbd (format "M-[ 21 ; %d ~" x)) (kbd (format "%s<f10>" tkey)))
      ;; f11
      (define-key key-translation-map (kbd (format "M-[ 23 ; %d ~" x)) (kbd (format "%s<f11>" tkey)))
      ;; f12
      (define-key key-translation-map (kbd (format "M-[ 24 ; %d ~" x)) (kbd (format "%s<f12>" tkey)))
      ;; f13
      (define-key key-translation-map (kbd (format "M-[ 25 ; %d ~" x)) (kbd (format "%s<f13>" tkey)))
      ;; f14
      (define-key key-translation-map (kbd (format "M-[ 26 ; %d ~" x)) (kbd (format "%s<f14>" tkey)))
      ;; f15
      (define-key key-translation-map (kbd (format "M-[ 28 ; %d ~" x)) (kbd (format "%s<f15>" tkey)))
      ;; f16
      (define-key key-translation-map (kbd (format "M-[ 29 ; %d ~" x)) (kbd (format "%s<f16>" tkey)))
      ;; f17
      (define-key key-translation-map (kbd (format "M-[ 31 ; %d ~" x)) (kbd (format "%s<f17>" tkey)))
      ;; f18
      (define-key key-translation-map (kbd (format "M-[ 32 ; %d ~" x)) (kbd (format "%s<f18>" tkey)))
      ;; f19
      (define-key key-translation-map (kbd (format "M-[ 33 ; %d ~" x)) (kbd (format "%s<f19>" tkey)))
      ;; f20
      (define-key key-translation-map (kbd (format "M-[ 34 ; %d ~" x)) (kbd (format "%s<f20>" tkey)))

      (setq x (+ x 1))
      ))
    )
  )


;set priorities for repos:
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))



;(setq warning-minimum-level :emergency)

(package-initialize)
;

(unless package-archive-contents
  (package-refresh-contents))

; install use-package if missing
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))


(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;;(add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))
(setq use-package-always-ensure t) ;use-package will download missing ones





;(use-package benchmark-init
;  :ensure t
;  :config
;  ;; To disable collection of benchmark data after init is done.
;  (add-hook 'after-init-hook 'benchmark-init/deactivate))


;; (use-package auto-compile)
;; (auto-compile-on-load-mode)
;; (auto-compile-on-save-mode)
;; (setq auto-compile-display-buffer nil)
;; (setq auto-compile-mode-line-counter t)




(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)







; dont ask about exiting anyway in presence of unsaved buffers
(defun my-save-buffers-kill-emacs (&optional arg)
  "Offer to save each buffer(once only), then kill this Emacs process.
With prefix ARG, silently save all file-visiting buffers, then kill."
  (interactive "P")
  (save-some-buffers arg t)
  (and (or (not (fboundp 'process-list))
       ;; process-list is not defined on MSDOS.
       (let ((processes (process-list))
         active)
         (while processes
           (and (memq (process-status (car processes)) '(run stop open listen))
            (process-query-on-exit-flag (car processes))
            (setq active t))
           (setq processes (cdr processes)))
         (or (not active)
         (progn (list-processes t)
            (yes-or-no-p "Active processes exist; kill them and exit anyway? ")))))
       ;; Query the user for other things, perhaps.
       (run-hook-with-args-until-failure 'kill-emacs-query-functions)
       (or (null confirm-kill-emacs)
       (funcall confirm-kill-emacs "Really exit Emacs? "))
       (kill-emacs)))
(fset 'save-buffers-kill-emacs 'my-save-buffers-kill-emacs)

; no tool bar
(tool-bar-mode -1)

;hide menu bar (the one at the top)
(menu-bar-mode -1)

; auto completion
(use-package company
;  :hook prog-mode
  )
(add-hook 'after-init-hook 'global-company-mode)
(setq company-async-timeout 5)
(setq company-auto-complete 'company-explicit-action-p)


; move C-SPC to C-S-SPC so that company can use C-SPC
(global-set-key (kbd "C-S-SPC") 'set-mark-command)
(global-set-key (kbd "\200") 'set-mark-command)

(global-set-key (kbd "C-SPC") 'company-complete)
(global-set-key (kbd "C-@") 'company-complete)



(setq indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)


; on-the-fly error checking
(use-package flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)





;languages:
(load "~/.emacs.d/ocaml")
(load "~/.emacs.d/python")
(load "~/.emacs.d/haskell")
(load "~/.emacs.d/links")







(setq make-backup-files nil) ; stop creating backup~ files

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))) ; store auto save files in tmp dir




(fset 'yes-or-no-p 'y-or-n-p) ; replace yes no questions by y n


(use-package ample-theme)
(load-theme 'ample t t)
(enable-theme 'ample)


(global-linum-mode t) ;show line numbers
(setq linum-format "%4d \u2502 ") ; separate line number from text with solid line
;(setq-default left-margin-width 10 right-margin-width 8) ; Define new widths.
; (set-window-buffer nil (current-buffer)) ; Use them now.
;(set-face-attribute 'fringe nil :background "#2E2920" :foreground "#2E2920")
(column-number-mode 1)
(show-paren-mode 1) ; show matching parantheses




(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;(use-package git-timemachine)

; highlight differences with version control base
(use-package git-gutter)
(global-git-gutter-mode)
(git-gutter:linum-setup)


(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)





; do not ask whether to close emacs if clients exist
;(defun my-server-kill-emacs-query-function () t)
;(fset 'server-kill-emacs-query-function 'my-server-kill-emacs-query-function)











;(if window-system
;(setq tabbar-ruler-global-tabbar t)    ; get tabbar
;(setq tabbar-ruler-global-ruler t)     ; get global ruler
;(setq tabbar-ruler-popup-menu t)       ; get popup menu.
;(setq tabbar-ruler-popup-toolbar t)    ; get popup toolbar
;(setq tabbar-ruler-popup-scrollbar t)  ; show scroll-bar on mouse-move
;(use-package tabbar-ruler
;  :if window-system
;  )
;)


;(defun tabbar-buffer-groups ()
;  "Return the list of group names the current buffer belongs to.
;This function is a custom function for tabbar-mode's tabbar-buffer-groups.
;This function group all buffers into 3 groups:
;Those Dired, those user buffer, and those emacs buffer.
;Emacs buffer are those starting with “*”."
;  (list
;   (cond
;    ((string-equal "*" (substring (buffer-name) 0 1))
;     "Emacs Buffer"
;     )
;    ((eq major-mode 'dired-mode)
;     "Dired"
;     )
;    (t
;     "User Buffer"
;     )
;    )))
;(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
;
;(global-set-key (kbd "C-M-S-<left>") 'tabbar-ruler-backward)
;(global-set-key (kbd "C-M-S-<right>") 'tabbar-ruler-forward)
;(global-set-key (kbd "C-M-S-<up>") 'tabbar-ruler-up)
;(global-set-key (kbd "C-M-S-<down>") 'tabbar-ruler-up)


(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-for-files)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
;         ("C-x c o" . helm-occur)
;         ("C-x c s" . helm-swoop)
;         ("C-x c y" . helm-yas-complete)
;         ("C-x c Y" . helm-yas-create-snippet-on-region)
 ;        ("C-x c b" . my/helm-do-grep-book-notes)
         ("C-x c SPC" . helm-all-mark-rings)))

(use-package helm-descbinds
  :defer t
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

; quickly shows all occurences inside the current file
(use-package helm-swoop)






					; save and restore buffers
(if window-system
(desktop-save-mode 1) )





; matching parantheses are colored
(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)






; move current line or selected region up/down
(use-package move-text)
(global-set-key [M-S-down] 'move-text-down)
(global-set-key [M-S-up]   'move-text-up)




;debugger
;(load-library "realgud")




;(defun switch-between-minibuffer-window ()
;  "switch to minibuffer window (if active)"
;  (interactive)
;  (if (window-minibuffer-p) ;(active-minibuffer-window)
;    ;((select-frame-set-input-focus (window-frame (active-minibuffer-window)))
;     (windmove-up)
;     (select-window (active-minibuffer-window))
;    ))
;
;(global-set-key (kbd "<f7>") 'switch-between-minibuffer-window)





;(defun my/kill-this-buffer ()
;  "Kill the current buffer."
;  (interactive)
;  (kill-buffer (current-buffer)))
;(global-set-key (kbd "C-q") 'my/kill-this-buffer)



;(global-set-key (kbd "<mouse-2>") 'x-clipboard-yank)







;synchronize clipboard between emacs and X server, even in terminal mode
(load "~/.emacs.d/custom-packages/termclipboard.el")



;automatically reload unchanged files
(load "~/.emacs.d/custom-packages/revbufs.el")
(require 'revbufs)
(global-auto-revert-mode)




;; (defun compile-pkg (&optional command startdir)
;;   "Compile a package, moving up to the parent directory
;;   containing configure.ac, if it exists. Start in startdir if defined,
;;   else start in the current directory."
;;   (interactive)

;;   (let ((dirname)
;; 	(dir-buffer nil))
;;     (setq startdir (expand-file-name (if startdir startdir ".")))
;;     (setq command  (if command command compile-command))

;;     ;(setq dirname (upward-find-file "configure.ac" startdir))
;;     (setq dirname (upward-find-file "Makefile" startdir))
;;     (setq dirname (if dirname dirname (expand-file-name ".")))
;;     ; We've now worked out where to start. Now we need to worry about
;;     ; calling compile in the right directory
;;     (save-excursion
;;       (setq dir-buffer (find-file-noselect dirname))
;;       (set-buffer dir-buffer)
;;       (compile command)
;;       (kill-buffer dir-buffer))))


;; (defun upward-find-file (filename &optional startdir)
;;   "Move up directories until we find a certain filename. If we
;;   manage to find it, return the containing directory. Else if we
;;   get to the toplevel directory and still can't find it, return
;;   nil. Start at startdir or . if startdir not given"

;;   (let ((dirname (expand-file-name
;; 		  (if startdir startdir ".")))
;; 	(found nil) ; found is set as a flag to leave loop if we find it
;; 	(top nil))  ; top is set when we get
;; 		    ; to / so that we only check it once

;;     ; While we've neither been at the top last time nor have we found
;;     ; the file.
;;     (while (not (or found top))
;;       ; If we're at / set top flag.
;;       (if (string= (expand-file-name dirname) "/")
;; 	  (setq top t))

;;       ; Check for the file
;;       (if (file-exists-p (expand-file-name filename dirname))
;; 	  (setq found t)
;; 	; If not, move up a directory
;; 	(setq dirname (expand-file-name ".." dirname))))
;;     ; return statement
;;     (if found dirname nil)))

;; (defun std-compile ()
;;   "Like 'compile', but uses compile-pkg"
;;   (interactive)
;;   (compile-pkg compile-command))
;; (global-set-key (kbd "C-c c") 'std-compile)

;; ;; (defun compile-next-makefile ()
;; ;;   (interactive)
;; ;;   (let* ((default-directory (or (upward-find-file "Makefile") "."))
;; ;;          (compile-command (concat "cd " default-directory " && "
;; ;;                                   compile-command)))
;; ;;     (compile compile-command)))


;;(use-package projectile
;;  ;:bind ("C-c C-c" . projectile-compile-project)
;;  )
;;(global-set-key (kbd "C-c C-c") (lambda () (projectile-compile-project "arg")) )
;;(global-set-key (kbd "C-c C-c") (lambda () (interactive) (projectile-compile-project 1)))
;;(global-set-key (kbd "C-c C-c") #'my/projectile-compile-project)
;;(define-key projectile-mode-map [\?C-c C-c] 'projectile-find-dir)
;;(define-key projectile-mode-map [?\C-t] 'projectile-compile-project)
;(define-key projectile-mode-map (kbd "C-x m") 'projectile-compile-project)
;;(define-key projectile-mode-map (kbd "C-c C-c") 'projectile-compile-project)
;;(global-set-key (kbd "C-c C-c") (lookup-key projectile-mode-map (kbd "c")))
;;(define-key 'mode-specific-command-prefix (kbd "C-c") (lookup-key projectile-mode-map (kbd "c")))
;
;(defun notify-compilation-result(buffer msg)
;  "Notify that the compilation is finished,cccccccccccccc
;close the *compilation* buffer if the compilation is successful,
;and set the focus back to Emacs frame"
;  (if (string-match "^finished" msg)
;    (progn
;     (tooltip-show "\n Compilation Successful :-) \n "))
;    (progn
;      (tooltip-show "\n Compilation Failed :-( \n ")
;      (switch-to-buffer-other-window "*compilation*")
;      ))
;  (setq current-frame (car (car (cdr (current-frame-configuration)))))
;  (select-frame-set-input-focus current-frame)
;  )
;(add-to-list 'compilation-finish-functions
;	     'notify-compilation-result)
;
;
;; prevent compile from showing buffer
;(defadvice compilation-start
;  (around inhibit-display
;      (command &optional mode name-function highlight-regexp))
;  (if (not (string-match "^\\(find\\|grep\\)" command))
;      (flet ((display-buffer)
;         (set-window-point)
;         (goto-char))
;    (fset 'display-buffer 'ignore)
;    (fset 'goto-char 'ignore)
;    (fset 'set-window-point 'ignore)
;    (save-window-excursion
;      ad-do-it))
;    ad-do-it))
;(ad-activate 'compilation-start)




;(defun toggle-query-replace-case () (interactive) (setq case-fold-search (not case-fold-search)))
;(define-key query-replace-map "C" 'toggle-case)


;(defun drew/toggle-case ()
;  "Toggle the value of `case-fold-search' between `nil' and non-nil."
;  (interactive)
;  ;; `case-fold-search' automatically becomes buffer-local when set
;  (setq case-fold-search (not case-fold-search)))
;(define-key query-replace-map (kbd "M-c") #'drew/toggle-case)


;(use-package visual-regexp)
;(use-package visual-regexp-steroids)
;(define-key global-map (kbd "C-c r") 'vr/replace)
;(define-key global-map (kbd "C-c q") 'vr/query-replace)
;(define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
;(define-key esc-map (kbd "C-s") 'vr/isearch-forward) ;; C-M-s


(setq split-height-threshold nil)
(setq split-width-threshold 160)


(use-package  multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)



(use-package avy) ;quickly jump around
(define-key global-map (kbd "<S-return>") 'avy-goto-word-0)
(global-set-key (kbd "C-j") 'avy-goto-word-0)



(use-package golden-ratio-scroll-screen)
(global-set-key (kbd "<C-prior>") 'golden-ratio-scroll-screen-down)
(global-set-key (kbd "<C-next>") 'golden-ratio-scroll-screen-up)


(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)


(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  ;(windmove-default-keybindings)
  )
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)



;;(use-package imenu-list)
;;(global-set-key (kbd "C-S-i") #'imenu-list-smart-toggle)


;; (defun my-smart-backspace ()
;;   (interactive)
;;   (if (string-match "^[[:space:]]*$" (thing-at-point 'line))
;;       (delete-indentation)
;;     (backward-delete-char-untabify 1)))
;; (global-set-key (kbd "<backspace>") 'my-smart-backspace)


;; (use-package hungry-delete)
;; ;(global-hungry-delete-mode)
;; (setq hungry-delete-chars-to-skip " \t")

;; (defun delete-word (arg)
;;   "Delete characters forward until encountering the end of a word.
;; With argument, do this that many times."
;;   (interactive "p")
;;   (if (use-region-p)
;;       (delete-region (region-beginning) (region-end))
;;     (delete-region (point) (progn (forward-word arg) (point)))))

;; (defun backward-delete-word (arg)
;;   "Delete characters backward until encountering the end of a word.
;; With argument, do this that many times."
;;   (interactive "p")
;;   (if (looking-at "[ \t\n]")
;;       (let ((p (point)))
;;         (re-search-forward "[^ \t\n]" nil :no-error)
;;         (backward-char)
;;         (kill-region p (point)))
;;     (kill-word 1)))


;; (defun my-kill-back ()
;;   (interactive)
;;   (if (bolp)  ; beginnning of line, just delete 1
;;       (backward-delete-char 1)
;;     (if (string-match "[^[:space:]]" (buffer-substring (point-at-bol) (point)))
;;         ; There's a word on the line, delete it
;;         (backward-kill-word 1)
;;       (delete-region (point-at-bol) (point))))) ; all whitespace, delete it

;; ;(global-set-key (kbd "<C-DEL>") 'delete-word)
;; (global-set-key (kbd "<C-backspace>") 'my-kill-back)

;; (setq x-select-enable-clipboard t)
;; (setq save-interprogram-paste-before-kill t)


;terminal mouse integration
(unless window-system
  (xterm-mouse-mode 1)
  (global-set-key [mouse-4] (lambda ()
                  (interactive)
                  (scroll-down 3)))
  (global-set-key [mouse-5] (lambda ()
                  (interactive)
                  (scroll-up 3))))



;no automatic indent on newline
(electric-indent-mode 0)

;highlight current line
(global-hl-line-mode +1)
(set-face-attribute 'hl-line nil :inherit nil :background "gray6")


;delete trailing whitespace on save
(add-hook 'prog-mode-hook
	  (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))



(define-key undo-tree-map "\C-_" nil)
(global-set-key (kbd "C-_") 'backward-kill-word)

(global-set-key (kbd "C-k") 'kill-whole-line)



; cycle through buffers with C-Tab
(use-package pc-bufsw)
(setq pc-bufsw-quit-time 3)
(pc-bufsw t)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
