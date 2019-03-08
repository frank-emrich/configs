; needs Haskell IDE Engine:
; git clone https://github.com/haskell/haskell-ide-engine
;cd haskell-ide-engine
;stack install

(use-package haskell-mode)

(use-package lsp-haskell)
 ; :hook haskell-mode
;	:pin melpa
;  )


;(add-hook 'haskell-mode-hook 'lsp-noquery)


;; (add-hook 'haskell-mode-hook 'flycheck-mode)
