(defun lsp-if-non-magit-history ()
  ;(warn buffer-name)
  (unless (string-match "\\.\\(cc\\|h\\|tpp\\|cpp\\|c\\)\\.~[a-z0-9_-]+\\(\\(\\^\\|~\\)[0-9]*\\)?~" (buffer-name))
  (lsp-noquery)))

;; bla.cc.~master^~
;; bla.cc.~master^1~
;; bla.cc.~master~
;; "src/ic/accessor-assembler.cc.~CSA_forward~2~"

(if (executable-find "clangd")
    (add-hook 'c++-mode-hook 'lsp-if-non-magit-history))


(with-eval-after-load 'lsp-clients
  ;; "--pch-storage=disk" should reduce memory consumption
  ;; "--completion-style=detailed" should affect auto completion detail
  (setq lsp-clients-clangd-args
	(quote ("-j=6"
		"--background-index"
		"--pch-storage=disk"
		"--cross-file-rename"
		"--completion-style=detailed"
		"--header-insertion=never"))))


(use-package cc-mode
  :defer t
  :config
   (define-key c-mode-base-map (kbd "RET") 'newline-and-indent))

;"\\.\\(cc\\|h\\|tpp\\|cpp\\|c\\)\\.~[a-z0-9_-]+\\(\\^[0-9]*\\)?~"
