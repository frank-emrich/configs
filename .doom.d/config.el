;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Frank Emrich"
      user-mail-address "git@emrich.io")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;(setq doom-theme 'doom-spacegrey)
(setq doom-theme 'doom-one)
;;(setq doom-theme 'doom-one-vibrant)
(setq doom-vibrant-brighter-comments t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! windmove
  :defer
  :config
  :bind
    ("C-x <left>"  . windmove-left)
    ("C-x <right>" . windmove-right)
    ("C-x <up>"    . windmove-up)
    ("C-x <down>"  . windmove-down))

;terminal mouse integration
(unless window-system
  (xterm-mouse-mode 1))


(after! undo-tree
  (let ((map undo-tree-map))
    ;; continue providing normal undo behavior on C-x u
    (define-key map (kbd "\C-x U") 'undo-tree-visualize)
    (define-key map (kbd "\C-x u") nil)))


(after! company
  (setq company-dabbrev-downcase nil) ; make company-dabbrev case-sensitive
  (setq company-tooltip-align-annotations 't)
  (setq company-idle-delay nil)
  (setq company-minimum-prefix-length 0)
  (setq company-auto-commit nil))


; FIXME needs package entry
;quickly jump around
(use-package! avy
;  :pin MELPA ; the MELPA-stable version is outdated
  :init
     (require 'avy) ;; load immediately to prevent delay on first usages

  :config
    (setq avy-all-windows t) ;; show stuff in all windows
    (setq avy-goto-word-0-regexp
      "\\([-_a-zA-Z0-9]\\{3,1000\\}\\|[-_a-zA-Z0-9]\\{2\\}$\\|[-_a-zA-Z0-9]\\{2\\}[^-_a-zA-Z0-9]\\{2\\}\\|[-_a-zA-Z0-9]$\\|[-_a-zA-Z0-9][^-_a-zA-Z0-9]\\{3\\}\\|^[ \\t]*?[ \\t]\\{0,3\\}[;/,(){}]+$\\)")
   (setq avy-style (quote words))
   

    ;; (face-spec-set 'avy-lead-face   '((t (:background "brightwhite" :foreground "color-52"))))
    ;; (face-spec-set 'avy-lead-face-0 '((t (:background "brightwhite" :foreground "color-17"))))
    ;; (face-spec-set 'avy-lead-face-2 '((t (:background "brightwhite" :foreground "color-22"))))
)

(define-key global-map (kbd "<S-return>") 'avy-goto-word-0)
(global-set-key (kbd "C-j") 'avy-goto-word-0)
(global-set-key (kbd "<M-return>") 'avy-goto-word-0)
(global-set-key (kbd "M-RET") 'avy-goto-word-0)
(global-set-key (kbd "C-c RET") 'avy-goto-word-0)
(global-set-key (kbd "C-c j") 'avy-goto-word-0)

(after! helm-rg
  (setq helm-rg--extra-args "-P"))


;; No auto-closing " and bracktets/braces
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)



;; (after! lsp-mode
;;   (setq lsp-diagnostics-provider :flycheck)
;;  ;; (lsp-ui-doc-enable nil)
;;   )
;;
;;

(after! lsp-ui
                                        ; Don't update flycheck immediately, but according to the value of
                                        ; flycheck-check-syntax-automatically
  (setq lsp-ui-flycheck-live-reporting nil))




(after! flycheck

  ;; Only check on save, not while typing
  (setq flycheck-check-syntax-automatically (quote (save mode-enabled)))

  ;; do not highlight errors in buffer itself
  (setq flycheck-highlighting-mode nil)

  ;; Highlight errors in margisn, it messes up formatting in some modes
  ;; This seems to have messed up things in the past?
  (setq flycheck-indication-mode 'left-margin)


  ;; Like Doom's +syntax-init-popups-h, but
  ;; don't enable flycheck-popup-tip-mode
  (defun custom-syntax-init-popups-h ()
    (require 'cl-lib)
    (cl-letf (((symbol-function 'flycheck-popup-tip-mode) #'ignore))
      (+syntax-init-popups-h)))


  (remove-hook 'flycheck-mode-hook '+syntax-init-popups-h)
  (add-hook! 'flycheck-mode-hook 'custom-syntax-init-popups-h))

;; TMUX interaction

(defun get-tmux-session ()
  (when
      (getenv "TMUX")
    (replace-regexp-in-string "\n$" ""
      (shell-command-to-string "tmux display-message -p '#S'"))))

;; Helm config

(use-package! helm-projectile
  :init
  (helm-projectile-on)
)

(use-package! helm
  :init

   ;; (setq helm-advice-push-mark nil)
  :config

  ; Show helm by splitting the current window
  (setq helm-split-window-inside-p t)


  (setq helm-candidate-number-limit 100)
  (setq helm-for-files-preferred-list
	(quote
	 (helm-source-buffers-list
	  helm-source-projectile-files-list
	  helm-source-recentf
	  helm-source-files-in-current-dir)))
  (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
        helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.

	helm-M-x-always-save-history t ;; save failing commands in M-x history
        )

  ;; Do not override push-mark with helm's special version.
  ;; We want to advice it with our own my/push-mark!
  ;; cannot use setq for this, because it has a :set attribute
  (customize-set-variable 'helm-advice-push-mark nil)
  )

(define-key global-map (kbd "C-x b") 'helm-for-files)
(define-key global-map (kbd "C-x C-b") 'helm-buffers-list)



(load "~/.doom.d/latex")


;; Version control/magit


(after! transient
  :config
    ;; show the popup in the same way as the old magit-popup did
    (setq transient-display-buffer-action '(display-buffer-below-selected)))

(defun my/magit-display-buffer (buffer)
  "Display BUFFER the way this has traditionally been done."
  (display-buffer
   buffer (if (and (derived-mode-p 'magit-mode)
                   (not (memq (with-current-buffer buffer major-mode)
                              '(magit-process-mode
                                magit-revision-mode
                                magit-diff-mode
                                magit-stash-mode
                                ;; magit-status-mode
                                ))))
              '(display-buffer-same-window)
            nil)))

(after! magit
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq magit-display-buffer-function 'my/magit-display-buffer)
  (setq magit-display-file-buffer-function 'magit-display-file-buffer-other-window)
  (define-key magit-hunk-section-map (kbd "RET") 'magit-diff-visit-file-other-window)

  (let ((magit-post-hooks
        '(magit-post-commit-hook
          magit-commit-post-finish-hook
          magit-post-stage-hook
          magit-post-unstage-hook
          magit-post-refresh-hook)))
    (dolist (hook magit-post-hooks)
      (progn
        (message "adding to magit hook")
        (add-hook hook #'git-gutter:update-all-windows)))))

(defhydra smerge-hydra
    (:color pink :hint nil :exit t)
    "
^Move^       ^Keep snippet          ^Diff^                           ^Other^
^^-----------^^---------------------^^-------------------------------^^-------
_n_ext       _b_ase                 _<_: upper/base                  _C_ombine with next conflict
_p_rev       _u_pper                _=_: upper/lower                 _r_esolve magically
^^           _l_ower                _>_: base/lower                  _k_ill version under cursor
^^           _a_ll                  _R_efine (highlight differences)
^^           _RET_: under cursor    _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))

(global-set-key (kbd "C-c m") 'smerge-hydra/body)


;; Doom integrates its own jump mode, which seems to just work
;; These bindings don't fully work yet, they are sometimes overwritten again.
(defun setup-better-jumper-bindings ()
  (define-key global-map (kbd "M-<left>") 'better-jumper-jump-backward)
  (define-key global-map (kbd "M-<right>") 'better-jumper-jump-forward))
(add-hook! 'after-change-major-mode-hook #'setup-better-jumper-bindings)


(define-key global-map (kbd "C-c SPC") '+company/complete)


(defun dos2unix ()
  "Convert dos to unix file endings (save afterwards)"
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

;; ; Make sure that better jumper can go back to start of isearch
;; (defun my/isearch-done-set-better-jumper-mark ()
;;   ;; copied from isearch-done, using better-jumper-set-jump instead of push-mark
;;   (if (/= (point) isearch-opoint)
;;       (or (and transient-mark-mode mark-active)
;;           (better-jumper-set-jump isearch-opoint))))

;; (add-hook! 'isearch-mode-end-hook #'my/isearch-done-set-better-jumper-mark)


(defun disable-electric-indent-mode ()
  (electric-indent-mode -1))
(add-hook! 'after-change-major-mode-hook #'disable-electric-indent-mode)


;; Version of push-mark that also calls better-jumper-set-jump.
;; The latter calls push-mark, so we must prevent those mutually recursive calls
(defvar my/push-mark-active nil)
(defun my/push-mark (original-push-mark &optional LOCATION NOMSG ACTIVATE)
  ;;(message "my/push-mark called")
  (funcall original-push-mark LOCATION NOMSG ACTIVATE)
  (unless my/push-mark-active
    (let ((my/push-mark-active t))
      (better-jumper-set-jump LOCATION))))

(advice-add 'push-mark :around #'my/push-mark)
;;(advice-remove 'push-mark  #'my/push-mark)


;; (defun my/push-mark-to-better-jumper (&optional LOCATION NOMSG ACTIVATE)
;;   (message "advice working")
;;   (better-jumper-set-jump (or LOCATION (point)))
;; )

;; (advice-add 'push-mark :after #'my/push-mark-to-better-jumper)

;; Search

;; remove unused stuff from search map:
;; (let (bad-search-keys (list "O"))

;; doom-leade-search-map

;; OSC52 clipboard integration
(add-to-list 'load-path "~/.doom.d/custom-packages/")
(require 'osc52)
;(load "~/.emacs.d/custom-packages/ocs52.el")
(osc52-set-cut-function)


;; History saving (setup elsewhere by doom)

(after! savehist
  ;; doom's config code for savehist changes this, so
  ;; execute our logic afterwards
  (setq savehist-additional-variables
        (append savehist-additional-variables
                '(query-replace-history
	          extended-command-history
	          comint-input-ring ;; for gdb/gud
                  ))))

;; Save history per tmux-session
(setq savehist-file
      (format
       "%s.%s"
       (expand-file-name
	"savehist"
	doom-cache-dir)
       (or (get-tmux-session) "notmux")))

;; Recent files (setup elsewhere by doom)

(setq recentf-save-file
      (format
       "%s.%s"
       (expand-file-name
	"recentf"
	doom-cache-dir)
       (or (get-tmux-session) "notmux")))
