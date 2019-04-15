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

(add-to-list 'load-path "~/.emacs.d/custom-packages/")




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


(use-package cl-lib)

(use-package magit
 ;get rid of rebase mode
 :config
   (setq
     auto-mode-alist
    (cl-remove "/git-rebase-todo\\'" auto-mode-alist :test 'equal :key 'car)))



; auto completion
(use-package company
;  :hook prog-mode
  )
(add-hook 'prog-mode-hook 'global-company-mode)
;(setq company-async-timeout 5)
(setq company-auto-complete 'company-explicit-action-p)






(defun warn-echo-area (format &rest args)
  "Display lsp warn message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "Warning" 'face 'warning) (apply #'format format args)))



;lsp config
(defun lsp--original-calculate-root (session file-name)
  "Calculate project root for FILE-NAME in SESSION."
  (and
   (->> session
        (lsp-session-folders-blacklist)
        (--first (f-ancestor-of? it file-name))
        not)
   (or
    (when lsp-auto-guess-root
      (lsp--suggest-project-root))
    (lsp-find-session-folder session file-name)
    (unless lsp-auto-guess-root
(lsp--find-root-interactively session)))))

(defun lsp--noquery-calculate-root (session file-name)
  "Calculate project root for FILE-NAME in SESSION."
  (and
   (->> session
        (lsp-session-folders-blacklist)
        (--first (f-ancestor-of? it file-name))
        not)
   (or
    (when lsp-auto-guess-root
      (lsp--suggest-project-root))
    (lsp-find-session-folder session file-name)
    (unless lsp-auto-guess-root nil))))



(use-package lsp-mode
  :commands lsp
  :config
   (require 'lsp-clients)
   (fset 'lsp--calculate-root 'lsp--noquery-calculate-root)
)


;version of 'lsp that will not interactively ask to specify project root
(defun lsp-noquery ()
  (interactive)
  (progn
    (fset 'lsp--calculate-root 'lsp--noquery-calculate-root)
    (lsp)
    (fset 'lsp--calculate-root 'lsp--original-calculate-root))
)


; after loading file with active LSP, set the LSP workspace root as default-directory
(defun set-default-dir-lsp-root ()
  (setq default-directory (lsp-workspace-root)))
(add-hook 'lsp-after-open-hook 'set-default-dir-lsp-root)


(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)


(defun lsp-disable-highlighting ()
  (interactive)

   (advice-add 'lsp-document-highlight  :override
            (lambda () nil))

)



; move C-SPC to C-S-SPC so that company can use C-SPC
(global-set-key (kbd "C-S-SPC") 'set-mark-command)
(global-set-key (kbd "\200") 'set-mark-command)

(global-set-key (kbd "C-SPC") 'company-complete)
(global-set-key (kbd "C-@") 'company-complete)



(setq indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)


; on-the-fly error checking
;(use-package flycheck)
;(add-hook 'after-init-hook #'global-flycheck-mode)





;languages:
(load "~/.emacs.d/ocaml")
(load "~/.emacs.d/python")
(load "~/.emacs.d/haskell")
(load "~/.emacs.d/links")
(load "~/.emacs.d/javascript")
(require 'llvm-mode)





(setq make-backup-files nil) ; stop creating backup~ files

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))) ; store auto save files in tmp dir




(fset 'yes-or-no-p 'y-or-n-p) ; replace yes no questions by y n

;load my modified (i.e., darker) ample theme
(require 'darker-ample-theme)
(load-theme 'darker-ample t t)
(enable-theme 'darker-ample)



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

;easily step through file's history
(use-package git-timemachine)

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


(use-package helm-ag)





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


; case insensitive search by default
(setq case-fold-search t)

(setq split-height-threshold nil)
(setq split-width-threshold 200)


(use-package  multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)


;quickly jump around
(use-package avy
  :pin MELPA ; the MELPA-stable version is outdated
)
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
(set-face-attribute 'hl-line nil :inherit nil :background "gray13")


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



(defun kill-buffer-and-frame ()
  (interactive)
  (kill-buffer)
  (delete-frame)
)
(global-set-key (kbd "C-q")  'kill-buffer-and-frame)




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

(add-hook 'after-revert-hook `linum-update-current)


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
(back-button-mode 1)
(global-set-key (kbd "<M-left>") 'back-button-global-backward)
(global-set-key (kbd "<M-right>") 'back-button-global-forward)


(advice-add 'xref-push-marker-stack :after
  (lambda  (&optional m)
    (back-button-push-mark-local-and-global (or m (point-marker)))))





;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
