(require 'help-fns)

(defun require--tracing-function (orig-fun &rest args)
"When testing with `emacs -q`, start by requiring `help-fns.el`."
  (message "`require' called with args %S" args)
  (with-current-buffer (get-buffer-create "*TRACE*")
    (let* ((standard-output (current-buffer))
           (print-escape-newlines t)
           (print-level 8)
           (print-length 50)
           beg end)
        (goto-char (point-max))
        (setq beg (point))
        (setq truncate-lines t)
        (set-buffer-multibyte t)
        (setq buffer-undo-list t)
        (backtrace)
        (insert "===============================\n")
        (setq end (point))
        (narrow-to-region beg end)
        (let ((regex
                (concat
                  "^\s+byte-code\("
                  "\\(\\(?:.\\)*?\\)"
                  "\s"
                  "\\[\\(.*\\)\\]"
                  "\s"
                  "\\([0-9]+\\)"
                  "\)"))
              (bytestr (propertize "BYTESTR" 'face '(:foreground "RoyalBlue")))
              (maxdepth (propertize "MAXDEPTH" 'face '(:foreground "RoyalBlue"))))
          (goto-char (point-max))
          (while (re-search-backward regex nil t)
            (when (match-string 1)
              (replace-match bytestr nil nil nil 1))
            (when (match-string 2)
              (let ((constants
                     (propertize (match-string 2) 'face '(:foreground "purple"))))
                (replace-match constants nil 'literal nil 2)))
            (when (match-string 3)
              (replace-match maxdepth nil nil nil 3))))
        ;;; See the Emacs Lisp manual:  Byte-Code Function Objects
        (let ((regex
                (concat
                   "#\\["
                   ;;; argdesc
                   "\\([0-9]+\\)"
                   ;;; byte-code
                   "\\(?:\s\\(.*?\\)\\)?"
                   "\s"
                   ;;; constants
                   "\\[\\(.*\\)\\]"
                   "\s"
                   ;;; stacksize
                   "\\([0-9]+\\)"
                   ;;; docstring
                   "\\(?:\s\\(.*?\\)\\)?"
                   ;;; interactive
                   "\\(?:\s\\(.*?\\)\\)?"
                   "\\]"))
              (argdesc
                (propertize "ARGDESC" 'face '(:foreground "orange")))
              (byte-code
                (propertize "BYTE-CODE" 'face '(:foreground "orange")))
              (stacksize
                (propertize "STACKSIZE" 'face '(:foreground "orange")))
              (docstring
                (propertize "DOCSTRING" 'face '(:foreground "orange")))
              (interactive
                (propertize "INTERACTIVE" 'face '(:foreground "orange"))))
          (goto-char (point-max))
          (while (re-search-backward regex nil t)
            (when (match-string 1)
              (replace-match argdesc nil nil nil 1))
            (when (match-string 2)
              (replace-match byte-code nil nil nil 2))
            (when (match-string 3)
              (let ((constants
                      (propertize
                        (match-string 3) 'face '(:foreground "ForestGreen"))))
                (replace-match constants nil 'literal nil 3)))
            (when (match-string 4)
              (replace-match stacksize nil nil nil 4))
            (when (match-string 5)
              (replace-match docstring nil nil nil 5))
            (when (match-string 6)
              (replace-match interactive nil nil nil 6))))
        (let ((regex
                (concat
                  "^\s+\(let\\*\s\(\(standard-output.*\(current-buffer\)\)\)$"
                  "\\|"
                  "^\s+\(let\s\(\(res\s.*res\)\sres\)$"
                  "\\|"
                  (concat "^\s+\(save-current-buffer\s\(set-buffer.*"
                          "\(current-buffer\)\)\)\)$")
                  "\\|"
                  "^\s+backtrace\(\)$"
                  "\\|"
                  "^\s+apply\(require--tracing-function .*\)$"
                  "\\|"
                  "^\s+require--tracing-function\(.*\)$")))
          (goto-char (point-max))
          (while (re-search-backward regex nil t)
            (delete-region (match-beginning 0) (1+ (match-end 0)))))
        (goto-char (point-min))
        ;;; A slight variation of the built-in `debugger-make-xrefs'.
        (while (progn
           (goto-char (+ (point) 2))
           (skip-syntax-forward "^w_")
           (not (eobp)))
          (let* ((beg (point))
                 (end (progn (skip-syntax-forward "w_") (point)))
                 (fn (function-called-at-point))
                 (sym (intern-soft (buffer-substring-no-properties beg end)))
                 (file
                   (if fn
                     (let* (
                          (function fn)
                          (advised (and (symbolp function)
                              (featurep 'nadvice)
                              (advice--p (advice--symbol-function function))))
                          ;; If the function is advised, use the symbol that has the
                          ;; real definition, if that symbol is already set up.
                          (real-function
                            (or (and advised
                                     (advice--cd*r
                                       (advice--symbol-function function)))
                               function))
                          ;; Get the real definition.
                         (def (if (symbolp real-function)
                                 (or (symbol-function real-function)
                                     (signal 'void-function (list real-function)))
                                 real-function))
                         (aliased (or (symbolp def)
                               ;; Advised & aliased function.
                               (and advised (symbolp real-function)
                              (not (eq 'autoload (car-safe def))))))
                         (file-name
                           (find-lisp-object-file-name
                             function (if aliased 'defun def))))
                      file-name)
                  (and sym (symbol-file sym 'defun)))))
            (when file
              (goto-char beg)
              ;; help-xref-button needs to operate on something matched
              ;; by a regexp, so set that up for it.
              (re-search-forward "\\(\\sw\\|\\s_\\)+")
              (help-xref-button 0 'help-function-def sym file)))
          (forward-line 1))
        (widen)
      (display-buffer (current-buffer))))
  (let ((res (apply orig-fun args)))
    (message "`require' returned %S" res)
    res))

; enable to profile package loading
;(advice-add 'require :around #'require--tracing-function)


(require 'package)


;(setq load-prefer-newer t) ;do not load outdated .elc files





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
      '(
        ("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA-Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
	; To make use of mirror, comment out other archives
	; To update mirror, require elpa-mirror, then execute elpamr-....
        ("My GNU ELPA Mirror" . "https://raw.githubusercontent.com/frank-emrich/my-elpa/master/"))

      package-archive-priorities
      '(("MELPA-Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 1)
	("My GNU ELPA Mirror" . 0)))



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
(require 'bind-key)                ;; if you use any :bind variant

(require 'use-package-ensure)
(setq use-package-always-ensure t) ;use-package will download missing ones

(add-to-list 'load-path "~/.emacs.d/custom-packages/")


(use-package delight)



;; run elpamr-create-mirror-for-installed to dump current packages tp ~/myelpa
(use-package elpa-mirror
  :defer t
  :init
    (setq elpamr-default-output-directory "~/.emacs.d/my-elpa-mirror"))


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
;(fset 'save-buffers-kill-emacs 'my-save-buffers-kill-emacs)

; no tool bar
(tool-bar-mode -1)

;hide menu bar (the one at the top)
(menu-bar-mode -1)


(use-package cl-lib)


; highlight differences with version control base
(use-package git-gutter :delight)


(defun setup-magit-inbuffer-keys ()
  ;; (define-prefix-command 'emacs-inbuffer-map)
  ;; (define-key emacs-inbuffer-map (kbd "s") 'magit-stage-file)
  ;; (define-key emacs-inbuffer-map (kbd "u") 'magit-unstage-file)
  ;; (global-set-key (kbd "C-x G") 'emacs-inbuffer-map)
  (define-key magit-file-mode-map (kbd "C-x G") 'magit-file-popup)

)

(use-package magit
  ;; :defer t
  :bind (("C-x g" . magit-status))
  :init
    (setq magit-auto-revert-mode nil)
  :config
    ;get rid of rebase mode
    (setq
      auto-mode-alist
      (cl-remove "/git-rebase-todo\\'" auto-mode-alist :test 'equal :key 'car))

    (setq ediff-split-window-function 'split-window-horizontally)
    (add-hook 'magit-post-commit-hook 'git-gutter:update-all-windows)
    (setup-magit-inbuffer-keys)

    ; add switches for rebasing and autostashing to pull menu
    (magit-define-popup-switch 'magit-pull-popup ?r "Rebase" "--rebase")
    (magit-define-popup-switch 'magit-pull-popup ?a "Autostash" "--autostash")

    ; Force magit-diff-visit-file to always open in other window
    (setq magit-display-file-buffer-function 'magit-display-file-buffer-other-window)
)


(defun git-stage-region ()
  (interactive)
  (let ((git-gutter:ask-p nil)
        (start (region-beginning))
        (end (region-end)))
    (save-excursion
      (goto-char start)
      (git-gutter:next-hunk 1)
      (while (< (point) end)
        (git-gutter:stage-hunk)
        ;; This is a hack to wait for git-gutter to finish
        ;; updating information (git-gutter kicks
        ;; of a process to update the diff information
        ;; and does not block)
        (while (get-buffer (git-gutter:diff-process-buffer (git-gutter:base-file)))
          (sit-for 0.05))
        (git-gutter:next-hunk 1)))))



(use-package projectile
  :init (projectile-mode +1))


; auto completion
(use-package company
  :defer t
  :bind
    ("C-S-SPC" . company-complete)
    ("\200"    . company-complete)
  :config
    (setq company-dabbrev-downcase nil) ; make company-dabbrev case-sensitive
    (setq company-idle-delay nil)
    ; only complete when specifically asked to
    (setq company-auto-complete 'company-explicit-action-p)
    (setq company-backends (quote (company-capf company-dabbrev))))




;; code folding
(use-package origami
  :bind ("M-+" . origami-toggle-node)
  :init
    (add-hook 'prog-mode-hook 'origami-mode))



(defun warn-echo-area (format &rest args)
  "Display warn message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "Warning" 'face 'warning) (apply #'format format args)))





(use-package lsp-mode
  :commands lsp
  :defer t
  :pin MELPA-Stable
  :init
    (setq lsp-prefer-flymake nil)
  :config

   ;; do not highlight symbol under cursor + its other references
   ;; can be changed using lsp-toggle-symbol-highlight
   (setq lsp-enable-symbol-highlighting nil)

   (setq lsp-enable-snippet nil)
   (setq lsp-eldoc-enable-hover nil) ;; disable showing info in minibuffer about current symbol... it's a bit annoying
   (setq lsp-eldoc-enable-signature-help nil))




; version of function lsp that will not interactively ask to specify
; project root
(defun lsp-noquery ()
  (interactive)
   ;; make sure that lsp--find-root-interactively exists:
   (require 'lsp-mode)

   (defun lsp--find-root-interactively-noop (_session) nil)

   (cl-letf
       (((symbol-function 'lsp--find-root-interactively)
	 'lsp--find-root-interactively-noop) )
     ;; call lsp with lsp--find-root-interactively being temporarily
     ;; overriden to always return nil
     (lsp)))


; after loading file with active LSP, set the LSP workspace root as default-directory
(defun set-default-dir-lsp-root ()
  (let* ((workspace-root  (lsp-workspace-root))
	 (workspace-root-with-slash
	  (if (string-match ".*/$" workspace-root)
	      workspace-root
	    (concat workspace-root "/"))))
	  (setq default-directory workspace-root-with-slash)))
(add-hook 'lsp-after-open-hook 'set-default-dir-lsp-root)


;;  lsp-ui-flycheck gets initialized during lsp-mode auto config
(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :pin MELPA-Stable
)



;; enable this to get error messages only when manually invoking flycheck-buffer
;; (with-eval-after-load 'lsp-ui-flycheck
;;   (setq lsp-ui-flycheck-live-reporting nil))



(use-package lsp-origami
  :pin MELPA-Stable
  :defer t)

;; When invoking xref-find-*, don't ask which identifier to use if xref can figure it out automatically
(setq xref-prompt-for-identifier nil)





(setq indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)






;languages:
(load "~/.emacs.d/ocaml")
(load "~/.emacs.d/c_cpp")
(load "~/.emacs.d/python")
(load "~/.emacs.d/haskell")
(load "~/.emacs.d/links")
(load "~/.emacs.d/javascript")
(load "~/.emacs.d/latex")
(load "~/.emacs.d/lisp")
(load "~/.emacs.d/racket")
(load "~/.emacs.d/agda")
(load "~/.emacs.d/llvm")
(load "~/.emacs.d/hack")







(setq make-backup-files nil) ; stop creating backup~ files

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))) ; store auto save files in tmp dir




(fset 'yes-or-no-p 'y-or-n-p) ; replace yes no questions by y n

;load my modified (i.e., darker) ample theme
(require 'darker-ample-theme)
(load-theme 'darker-ample t t)
(enable-theme 'darker-ample)



(if (version< emacs-version "26")
     (progn (setq linum-format "%4d \u2502 ")
	    (global-linum-mode t)
	    (global-git-gutter-mode t)
	    (git-gutter:linum-setup)
	    (add-hook 'after-revert-hook `linum-update-current))
  (global-display-line-numbers-mode t)
  (setq display-line-numbers "%4d \u2502 ")
  (global-git-gutter-mode t))
;(setq-default left-margin-width 10 right-margin-width 8) ; Define new widths.
; (set-window-buffer nil (current-buffer)) ; Use them now.
;(set-face-attribute 'fringe nil :background "#2E2920" :foreground "#2E2920")
(column-number-mode 1)
(show-paren-mode 1) ; show matching parantheses







(use-package undo-tree
  :delight
  :defer t
  :bind
    (("C-x u" . undo-tree-visualize))
  :config
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t) ;; show diff view by default
    (define-key undo-tree-map "\C-_" nil))



(eval-after-load "undo-tree"
  '(defun undo-tree-visualizer-show-diff (&optional node)
     "Override undo-tree-visualizer-show-diff such that it calls
      switch-to-buffer-other-window instead of split-window and
      doesn't shrink it"

  ;; show visualizer diff display
  (setq undo-tree-visualizer-diff t)
  (let ((buff (with-current-buffer undo-tree-visualizer-parent-buffer
		(undo-tree-diff node)))
	(display-buffer-mark-dedicated 'soft)
	(win (get-buffer-window " *undo-tree*")) )
    (switch-to-buffer-other-window buff)

    ;; go back to the undo-tree-visualizer window
    (select-window win))))









;easily step through file's history
(use-package git-timemachine :defer t)



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







;; Note that helm-display-function is changed once window-purpose is loaded!!
(use-package helm
  :ensure t
  :delight
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
(use-package helm-swoop
  ;; currently pinned to non-stable due to nil being passed, causing
  ;; all lines to be matched. re-visit later.
  :pin MELPA
)

(use-package helm-ag :defer t)

(use-package helm-projectile
  :init
  (helm-projectile-on)
)




					; save and restore buffers
(if window-system
(desktop-save-mode 1) )





; matching parantheses are colored
(use-package rainbow-delimiters
  :init
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))







; move current line or selected region up/down
(use-package move-text
  :bind
    ([M-S-down] . move-text-down)
    ([M-S-up]   . move-text-up)
)










;synchronize clipboard between emacs and X server, even in terminal mode
(load "~/.emacs.d/custom-packages/termclipboard.el")



;automatically reload unchanged files
(load "~/.emacs.d/custom-packages/revbufs.el")
(require 'revbufs)
(global-auto-revert-mode)






;(use-package visual-regexp)
;(use-package visual-regexp-steroids)
;(define-key global-map (kbd "C-c r") 'vr/replace)
;(define-key global-map (kbd "C-c q") 'vr/query-replace)
;(define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
;(define-key esc-map (kbd "C-s") 'vr/isearch-forward) ;; C-M-s



; isearch+ wants to be loaded as follows:
 (eval-after-load "isearch" '(require 'isearch+))
(setq isearchp-deactivate-region-flag nil)
(setq isearchp-restrict-to-region-flag t)

;(define-key minibuffer-local-isearch-map (kbd "<left>") 'isearch-edit-string)
;(define-key minibuffer-local-isearch-map (kbd "<right>") 'isearch-edit-string)

; case sensitive search by default
(setq case-fold-search nil)

(setq split-height-threshold nil)
(setq split-width-threshold 200)


(use-package  multiple-cursors
  :defer t
  :init
   (global-set-key (kbd "M-£") 'mc/edit-lines)
   (global-set-key (kbd "M-§") 'mc/edit-lines)
  :config
    ; make return in mc mode insert new line rather than aborting
     (define-key mc/keymap (kbd "<return>") nil))



;quickly jump around
(use-package avy
  :pin MELPA ; the MELPA-stable version is outdated
  :init

  :config
    (setq avy-all-windows nil)
    (setq avy-goto-word-0-regexp
      "\\([-_a-zA-Z0-9]\\{3,1000\\}\\|[-_a-zA-Z0-9]\\{2\\}$\\|[-_a-zA-Z0-9]\\{2\\}[^-_a-zA-Z0-9]\\{2\\}\\|[-_a-zA-Z0-9]$\\|[-_a-zA-Z0-9][^-_a-zA-Z0-9]\\{3\\}\\|^[ \\t]*?[ \\t]\\{0,3\\}[;/,(){}]+$\\)")
   (setq avy-style (quote words))
   ;(setq avy-words
     ;(quote
     ;  ("at" "by" "if" "is" "it" "my" "ob" "oh" "ok" "ox" "up" "ace" "act" "add" "age" "ago" "aim" "air" "ale" "all" "and" "ant" "any" "ape" "apt" "arc" "are" "arm" "art" "ash" "ate" "avy" "awe" "axe" "bad" "bag" "ban" "bar" "bat" "bay" "bed" "bee" "beg" "bet" "bid" "big" "bit" "bob" "bot" "bon" "bow" "box" "boy" "but" "cab" "can" "cap" "car" "cat" "cog" "cop" "cow" "cry" "cup" "cut" "day" "dew" "did" "die" "dig" "dim" "dip" "dog" "dot" "dry" "dub" "dug" "dye" "ear" "eat" "eel" "egg" "ego" "elf" "eve" "eye" "fan" "far" "fat" "fax" "fee" "few" "fin" "fit" "fix" "flu" "fly" "foe" "fog" "for" "fox" "fry" "fun" "fur" "gag" "gap" "gas" "gel" "gem" "get" "gig" "gin" "gnu" "god" "got" "gum" "gun" "gut" "guy" "gym" "had" "hag" "ham" "has" "hat" "her" "hid" "him" "hip" "hit" "hop" "hot" "how" "hub" "hue" "hug" "hut" "ice" "icy" "imp" "ink" "inn" "ion" "ire" "ivy" "jab" "jam" "jar" "jaw" "jet" "job" "jog" "joy" "key" "kid" "kit" "lag" "lap" "lay" "let" "lid" "lie" "lip" "lit" "lob" "log" "lot" "low" "mad" "man" "map" "mat" "may" "men" "met" "mix" "mob" "mop" "mud" "mug" "nag" "nap" "new" "nil" "nod" "nor" "not" "now" "nun" "oak" "odd" "off" "oil" "old" "one" "orb" "ore" "ork" "our" "out" "owl" "own" "pad" "pan" "par" "pat" "paw" "pay" "pea" "pen" "pet" "pig" "pin" "pit" "pod" "pot" "pry" "pub" "pun" "put" "rag" "ram" "ran" "rat" "raw" "ray" "red" "rib" "rim" "rip" "rob" "rod" "rot" "row" "rub" "rug" "rum" "run" "sad" "sat" "saw" "say" "sea" "see" "sew" "she" "shy" "sin" "sip" "sit" "six" "ski" "sky" "sly" "sob" "son" "soy" "spy" "sum" "sun" "tab" "tad" "tag" "tan" "tap" "tar" "tax" "tea" "the" "tie" "tin" "tip" "toe" "ton" "too" "top" "toy" "try" "tub" "two" "urn" "use" "van" "war" "was" "wax" "way" "web" "wed" "wet" "who" "why" "wig" "win" "wit" "woe" "won" "wry" "you" "zap" "zip" "zit" "zoo" "ade" "bei" "das" "der" "dem" "den" "ein" "eng" "eva" "feg" "geb" "geh" "hin" "ode" "nem" "nen" "neu" "reg" "reh" "wer" "wie" "wir" "wo" "weh" "wen" "zen" "aba" "gop" "blo" "bla" "klo" "gar" "glo" "per" "mer" "ret" "spe" "zer" "ala" "era" "lad" "raa" "hof" "mof" "tof" "got" "mot" "tra" "fra" "aaa" "aad" "ada" "daa" "aar" "ara" "aag" "aga" "gaa" "aah" "aha" "haa" "aaj" "aja" "jaa" "aak" "aka" "kaa" "aal" "sss" "ssd" "sds" "dss" "ssf" "sfs")))

    (face-spec-set 'avy-lead-face   '((t (:background "brightwhite" :foreground "color-52"))))
    (face-spec-set 'avy-lead-face-0 '((t (:background "brightwhite" :foreground "color-17"))))
    (face-spec-set 'avy-lead-face-2 '((t (:background "brightwhite" :foreground "color-22"))))
)

(define-key global-map (kbd "<S-return>") 'avy-goto-word-0)
(global-set-key (kbd "C-j") 'avy-goto-word-0)



(use-package golden-ratio-scroll-screen
  :bind
    ("<C-prior>" . golden-ratio-scroll-screen-down)
    ("<C-next>" . golden-ratio-scroll-screen-up)
)


(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)


(use-package windmove
  :config
  :bind
    ("C-x <left>"  . windmove-left)
    ("C-x <right>" . windmove-right)
    ("C-x <up>"    . windmove-up)
    ("C-x <down>"  . windmove-down)
)




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
(set-face-attribute 'hl-line nil :inherit nil :background "gray13")

(defvar ignore-trailing-whitespace nil)

(defun may-delete-whitespace ()
  (unless ignore-trailing-whitespace
    (delete-trailing-whitespace)))

;delete trailing whitespace on save
(add-hook 'prog-mode-hook
	  (lambda () (add-to-list 'write-file-functions 'may-delete-whitespace)))




(global-set-key (kbd "C-_") 'backward-kill-word)

(global-set-key (kbd "C-k") 'kill-whole-line)



; cycle through buffers with C-Tab
(use-package pc-bufsw
  :init
  (setq pc-bufsw-quit-time 3)
  ; Use a high-contact face ("tooltip") for the currenctly select buffer
  (setq pc-bufsw-selected-buffer-face (quote tooltip))
  (pc-bufsw t)
)




(defun kill-buffer-and-frame ()
  (interactive)
  (kill-buffer)
  (delete-frame)
)

(global-set-key (kbd "M-C-Q")  'kill-buffer-and-frame)




(use-package cl)

(add-to-list 'load-path "~/.emacs.d/custom-packages/e-sink")
(require 'tty-format)
(require 'ansi-color)


(defun display-ansi-colors ()
  (interactive)
  ; locally disable the "file changed on disk while you edited" check
  (flet ((ask-user-about-supersession-threat (fn)
         nil))
  (format-decode-buffer 'ansi-colors)
  )
)


(setq revert-without-query '(".*"))


(defun open-color-file (filename)
  (find-file filename)
  (buffer-disable-undo)
  (read-only-mode)
  (display-ansi-colors)
  (end-of-buffer)
  (setq auto-revert-interval 0.5)
  (auto-revert-set-timer)
  (add-hook 'after-revert-hook `display-ansi-colors nil t)
)



; modify C-w and M-w such that they use the system clipboard
; it seems like advising the functions works poorly in
; combination with interactive functions
; If interprogram-paste-function stays active,
; this effectively makes C-y use the clipboard only
(defun clipboard-enabled-kill-ring-save  ()
  (interactive)

  (setq interprogram-cut-function 'xsel-cut-function)
  (call-interactively 'kill-ring-save)
  (setq interprogram-cut-function nil)
)
(defun clipboard-enabled-kill-region ()
  (interactive)
  (setq interprogram-cut-function 'xsel-cut-function)
  (call-interactively 'kill-region)
  (setq interprogram-cut-function nil)
)
(global-set-key (kbd "M-w") 'clipboard-enabled-kill-ring-save)
(global-set-key (kbd "C-w") 'clipboard-enabled-kill-region)



(use-package back-button)

(advice-add 'push-mark  :override
            'back-button-push-mark-local-and-global)
;(back-button-mode 1)
(global-set-key (kbd "<M-left>") 'back-button-global-backward)
(global-set-key (kbd "<M-right>") 'back-button-global-forward)


(advice-add 'xref-push-marker-stack :after
  (lambda  (&optional m)
    (back-button-push-mark-local-and-global (or m (point-marker)))))


(global-set-key (kbd "<M-up>")  (lambda () (interactive) (scroll-up -10)))
(global-set-key (kbd "<M-down>")  (lambda () (interactive) (scroll-down -10)))



(use-package windsize
  :commands (windsize-right windsize-left))



(use-package hydra)
; stays active while C-right/C-left keeps being pressed
(defhydra hydra-resize
  (global-map "C-x")
  "resize window with C + left/right arrow keys"
  ("C-<right>" (windsize-right 10) nil)
  ("C-<left>" (windsize-left 10) nil)
  ("C-<up>" (windsize-up 10) nil)
  ("C-<down>" (windsize-down 10) nil)
)



(use-package imenu-list
  :defer t
  :init
    (setq imenu-list-size 45)
  :config
    ;; This was once a workaround for some display issue .... dead?
    ;; (add-hook 'imenu-list-update-hook
    ;; 	      (lambda ()
    ;; 		(overlay-put
    ;; 		 (buffer-local-value
    ;; 		  'hl-line-overlay
    ;; 		  (imenu-list-get-buffer-create))
    ;; 		 'face
    ;; 		 '(:background "#656565"))))
    )



(defun skip-chars-unless-leads-to (scf p)
  (let ((op (point)))
    (funcall scf  " \t")
    (if (eq (point) p)
        (goto-char op)))
)


(defun my/forward-word (&optional skip-post-ws count)
  (interactive)
  (setq count (or count 1))
  ; The following tricks right-char into handling shift
  ; withought visibly moving the point
  (save-excursion (call-interactively 'right-char))
  ;(backward-word 1)
  (while (/= count 0)

    (let*
	((forward
	  (> count 0))
	 (line-boundary
	  (if forward (line-end-position) (line-beginning-position)))
	 (skip-chars-fun
	  (if forward 'skip-chars-forward 'skip-chars-backward))
	 (skip-syntax-fun
	  (if forward 'skip-syntax-forward 'skip-syntax-backward))
	 (cur-char
	  (if forward (char-after) (char-before))))

      ;(message (string (char-syntax cur-char)))
      (cond
       ((eq line-boundary (point))
	(funcall skip-chars-fun " \n\t")
	(if forward (beginning-of-line) (end-of-line)))

       ((eq (char-syntax cur-char) ?\s )
	(funcall skip-chars-fun " \t"))


       ((eq (char-syntax cur-char) ?>) ; special case: syntax class > may contain \n
        (funcall skip-syntax-fun ">")
	(if skip-post-ws
	    (skip-chars-unless-leads-to skip-chars-fun line-boundary)))

       ((member (char-syntax cur-char) '(?w ?_ ?\\))
	(funcall skip-syntax-fun "w_\\")
	(if skip-post-ws
	    (skip-chars-unless-leads-to skip-chars-fun line-boundary)))


       (t
	(funcall skip-syntax-fun ".()\"<$'") ;was .()\"/\\'<>
	(if skip-post-ws
	    (skip-chars-unless-leads-to skip-chars-fun line-boundary)))))

    (setq count (- count (signum count)))))


(defun my/kill-word (arg)
  (interactive "p")
  (kill-region (point) (progn (my/forward-word nil arg) (point))))

(defun my/backward-kill-word (arg)
  (interactive "p")
  (my/kill-word (- arg)))


(global-set-key (kbd "<C-right>") (lambda () (interactive) (my/forward-word t 1)))
(global-set-key (kbd "<C-left>")  (lambda () (interactive) (my/forward-word t -1)))

(global-set-key (kbd "<C-backspace>")  'my/backward-kill-word)
(global-set-key (kbd "C-_")  'my/backward-kill-word)
(global-set-key (kbd "<C-delete>")  'my/kill-word)


(global-set-key (kbd "M-p")  (lambda nil (interactive) (scroll-up -10)))
(global-set-key (kbd "M-n")  (lambda nil (interactive) (scroll-up 10)))

; accounting for Linux' AltGr behavior
(global-set-key (kbd "þ")  (lambda nil (interactive) (scroll-up -10)))
(global-set-key (kbd "”")  (lambda nil (interactive) (scroll-up 10)))
(global-set-key (kbd "đ")  (lambda () (interactive) (my/forward-word t 1)))
(global-set-key (kbd "“")  (lambda () (interactive) (my/forward-word t -1)))



(load "~/.emacs.d/window_management")


; Spelling
(setq ispell-dictionary "en_US")
(defun look-for-aspell ()
  (unless (executable-find "aspell")
    (warn-echo-area "aspell not installed"))
)

(add-hook
 'flyspell-mode-hook
 (lambda () ; only do flyspell-buffer after file local vars are available
   (run-at-time "3 sec" nil 'flyspell-buffer))
	     )
; check whole buffer once entering flyspell mode
(add-hook 'flyspell-mode-hook (lambda () (run-at-time "3 sec" nil 'look-for-aspell)))

(defun string-list-p (l)
  (when (listp l)
    (cl-every 'stringp l)
 )
)

; ispell-buffer-session-localwords is safe for any list of strings
(put 'ispell-buffer-session-localwords 'safe-local-variable #'string-list-p)

;(setq ispell-dictionary "american")

(defun *-ispell-buffer-local-words-list ()
  (let (words)
    (or ispell-buffer-local-name
        (setq ispell-buffer-local-name (buffer-name)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward ispell-words-keyword nil t)
        (let ((end (point-at-eol))
              (ispell-casechars (ispell-get-casechars))
              string)
          (while (re-search-forward " *\\([^ ]+\\)" end t)
            (setq string (match-string-no-properties 1))
            (if (and (< 1 (length string))
                     (equal 0 (string-match ispell-casechars string)))
                (push string words))))))
    words))


(defun ispell-move-buffer-words-to-dir-locals ()
  (interactive)
  (unless (buffer-file-name)
    (user-error "buffer not attached to file"))
  (let ((words (*-ispell-buffer-local-words-list)))
    (save-excursion
      (add-dir-local-variable
       nil ; or name of major mode
       'ispell-buffer-session-localwords
       (setq ispell-buffer-session-localwords
             (cl-remove-duplicates
              (append ispell-buffer-session-localwords words)
              :test #'string=)))
      (when (y-or-n-p "Save .dir-locals.el?")
        (save-buffer))
      (bury-buffer))
    (or ispell-buffer-local-name
        (setq ispell-buffer-local-name (buffer-name)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward ispell-words-keyword nil t)
        (delete-region (point-at-bol) (1+ (point-at-eol)))))))


;; Tmux interaction
(defun get-tmux-session ()
  (when
      (getenv "TMUX")
    (replace-regexp-in-string "\n$" ""
      (shell-command-to-string "tmux display-message -p '#S'"))))



(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          ;; treemacs-file-extension-regex          treemacs-last-period-regex-value ;; yields error
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	  ;;treemacs-persist-file                  (format "%s.%s" (expand-file-name ".cache/treemacs-persist" user-emacs-directory) (or (get-tmux-session) "notmux")))
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         50)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))



(use-package highlight-thing
  :defer
  :init
    ;;(add-hook 'prog-mode-hook 'highlight-thing-mode)
    (setq highlight-thing-delay-seconds 0.25)
    (setq highlight-thing-case-sensitive-p t)
    (setq highlight-thing-exclude-thing-under-point t)
    (setq highlight-thing-prefer-active-region t))

; Highlighting:


(defface my-hi-yellow
  '(
    (t (:foreground "color-226" :background "black" :underline t)))
  "Default face for hi-lock mode."
  )

(defface my-hi-pink
  '(
    (t (:foreground "color-132" :background "black" :underline t)))
  "Default face for hi-lock mode."
  )

(defface my-hi-green
  '(
    (t (:foreground "color-45" :background "black" :underline t)))
  "Default face for hi-lock mode."
  )

(setq hi-lock-face-defaults
  '("my-hi-yellow" "my-hi-pink" "my-hi-green" ))



(setq create-lockfiles nil)




(defun update-display-from-tmux ()
  (interactive)
  (setenv "DISPLAY" (substring (shell-command-to-string "tmux show-environment DISPLAY") 8 -1))
)




;; Recent files
(use-package recentf
  :config
  (setq recentf-save-file
	(format "%s.%s" (expand-file-name ".cache/recentf" user-emacs-directory) (or (get-tmux-session) "notmux"))))


(use-package visual-regexp-steroids)



(defun flycheck-disable-error-list-update ()
  (remove-hook 'post-command-hook 'flycheck-error-list-highlight-errors 'local))

(use-package flycheck
  :defer t
  :pin MELPA ;; MELPA-STABLE version 31 is from 2017
  :config
  ;; We decide to only ever allow lsp-ui, but maybe we also want flycheck for tex and lisp?
  (setq flycheck-checkers (quote (lsp-ui)))
  ;; flycheck on save and loading of mode only ... doesn't seem to work
  ;; properly with lsp-ui though.
  (setq flycheck-check-syntax-automatically (quote (save mode-enabled)))
  (setq flycheck-highlighting-mode nil) ;; do not highlight errors in buffer itself

  ;; do not show errors in minibuffer when the cursor is on them
  (setq flycheck-display-errors-function #'ignore)
  ;; alternative: show errors in minibuffer, unless extra error list buffer is visible
  ;;  (setq flycheck-display-errors-function
  ;;    #'flycheck-display-error-messages-unless-error-list)


  ;; workaround for flickering of errors when flycheck error list enabled
  ;; seems to be solved in newer versions of flymake
  ;; (add-hook 'flycheck-mode-hook 'flycheck-disable-error-list-update)
)



; After typing prefix key show help popup after some idle time
(use-package which-key)
;; Allow C-h to trigger which-key before it is done automatically
;; (setq which-key-show-early-on-C-h t)
;; make sure which-key doesn't show normally but refreshes quickly after it is
;; triggered.
;(setq which-key-idle-delay 10000)
;(setq which-key-idle-secondary-delay 0.05)
(which-key-mode)
;(which-key-setup-minibuffer)
(which-key-setup-side-window-bottom)



; save history of commands, searches, etc
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring query-replace-history extended-command-history))
(setq savehist-file
      (format
       "%s.%s"
       (expand-file-name
	".cache/savehist"
	user-emacs-directory)
       (or (get-tmux-session) "notmux")))
(savehist-mode 1)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
