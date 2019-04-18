; needs Haskell IDE Engine:
; git clone https://github.com/haskell/haskell-ide-engine
; cd haskell-ide-engine
; stack install

(use-package haskell-mode
  :defer t
)

(use-package lsp-haskell
  :defer t
)

(add-hook 'haskell-mode-hook (lambda () (require 'lsp-haskell)))
