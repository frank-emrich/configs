(with-eval-after-load 'gdb-mi
  (defvar my/gdb-source-frame nil)
  (defvar my/gdb-control-frame nil)


  (defvar my/gdb-window-source nil)

  (defun my/gdb-setup-windows ()
    (interactive)

    (setq my/gdb-source-frame (make-frame))
    (setq my/gdb-control-frame (make-frame))

    (set-frame-size my/gdb-source-frame  (frame-width) (frame-height))
    (set-frame-size my/gdb-control-frame  (frame-width) (frame-height))


    ;; set up buffers
    (gdb-get-buffer-create 'gdb-locals-values-buffer)
    (gdb-get-buffer-create 'gdb-locals-buffer)
    (let
        ((buf-stack (gdb-get-buffer-create 'gdb-stack-buffer))
         (buf-break (gdb-get-buffer-create 'gdb-breakpoints-buffer))
         (buf-io (gdb-get-buffer-create 'gdb-inferior-io)))




      ;; set up windows in control frame
      (select-frame my/gdb-control-frame)
      (delete-other-windows)
      ;; top-left, top-right, bottom-left, bottom-right
      (let*
            ((two-third ( / (* (window-height) 2) 3))
             (one-third ( /  (window-height) 3))
             (win-top-row (selected-window))
             (win-bottom-row (split-window-vertically two-third win-top-row))
             (win-mid-row (split-window-vertically one-third win-top-row))
            ;; (win-bl (split-window-vertically nil win-tl))
            ;;(win-tr (split-window-horizontally nil win-tl))
            ;; (win-br (split-window-vertically nil win-tr))
            )


        (gdb-set-window-buffer (gdb-locals-buffer-name) t win-top-row)
        (gdb-set-window-buffer buf-break t win-mid-row)
        (gdb-set-window-buffer buf-stack t win-bottom-row)
        )


      ;; set up windows in source/gud frame
      (select-frame-set-input-focus my/gdb-source-frame)
      (delete-other-windows)

      (let* ((win-source (selected-window))
            (win-io (split-window-horizontally nil win-source))
            (win-gud (split-window-vertically nil win-io))
            )


        (setq gdb-source-window-list (list win-source))
        (set-window-dedicated-p win-source nil)
        (set-window-buffer win-source (or (gdb-get-source-buffer)
                                          (list-buffers-noselect)))
        (setq my/gdb-window-source win-source)

        (gdb-set-window-buffer buf-io t win-io)

        (select-window win-gud)
        (switch-to-buffer gud-comint-buffer)
        (set-window-dedicated-p win-gud t)


        ))



    )


  (defun my/gdb-display-source-buffer (buffer)
    (or my/gdb-window-source (error "gdb-source-window-list should not be empty"))
    (set-window-buffer my/gdb-window-source buffer)
    my/gdb-window-source
    )

  (advice-add 'gdb-display-source-buffer :override #'my/gdb-display-source-buffer)

  ;; Do not pop up the io buffer if it's burried
  (setq gdb-display-io-nopopup t)

  ;; Do not ask for debuginfod, just enabled it
  (setq gdb-debuginfod-enable-setting t)
  ;;(advice-add 'gdb-display-buffer :around #'my/gdb-display-buffer)

  (add-hook 'kill-buffer-hook 'comint-write-input-ring)


  (defhydra gud-hydra
      (:color pink :hint nil :exit nil
       :pre (add-hook 'doom-switch-window-hook 'disable-gud-hydra)
       :post (remove-hook 'doom-switch-window-hook 'disable-gud-hydra))
      "
  ^Execute^    ^Breakpoints^          ^Moving^                           ^Other^
  ^^-----------^^---------------------^^-------------------------------^^-------
  _n_ext       _b_reak                _<_: up (outer) frame           _p_rint
  _s_tep       _t_emp break           _<_: down (inner) frame
  _c_ont       ^^                     _l_: refresh/go to line
  _u_ntil      ^^
  _f_inish
  _r_un
  "
      ("n" gud-next)
      ("s" gud-step)
      ("c" gud-cont)
      ("u" gud-until)
      ("f" gud-finish)
      ("r" gud-run)
      ("b" gud-break)
      ("t" gud-tbreak)
      ("l" gud-refresh)
      ("<" gud-up)
      (">" gud-down)
      ("p" gud-print)
      ("q" nil "cancel" :color blue))

  (global-set-key (kbd "C-c d") 'gud-hydra/body)


  (defun disable-gud-hydra ()
    (hydra-disable))
)
