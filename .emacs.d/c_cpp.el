
(if (executable-find "clangd")
    (add-hook 'c++-mode-hook 'lsp-noquery))


(with-eval-after-load 'lsp-clients
  (setq lsp-clients-clangd-args (quote ("-j=6" "--background-index"))))


(use-package cc-mode
  :defer t
  :config)
