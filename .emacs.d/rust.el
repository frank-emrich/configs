
(defmacro if-present (executable f not-found-message)
 `(if (executable-find ,executable)
      ,f
    (warn-echo-area "%s: Couldn't find %s" ,not-found-message ,executable )))


(use-package rust-mode
  :defer t
  :config
    (if-present rust-rustfmt-bin (setq rust-format-on-save t) "cannot format on save"))
