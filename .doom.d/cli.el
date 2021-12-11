
;; pc-bufsw does something weird with autoloads. The following ensures that we
;; skip it when putting stuff into Doom's autloads.el
(with-eval-after-load "core/cli/autoloads.el"
  (add-to-list 'doom-autoloads-excluded-packages "pc-bufsw"))
