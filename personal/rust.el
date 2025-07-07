
(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save nil)
  ;;(setq rustic-analyzer-command '("~/.local/bin/rust-analyzer"))
  :custom
  (rustic-cargo-use-last-stored-arguments t)
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))

(use-package tomlparse
  :init
  (add-to-list 'treesit-language-source-alist
               '(toml "https://github.com/tree-sitter-grammars/tree-sitter-toml"))
  (unless (treesit-language-available-p 'toml)
    (treesit-install-language-grammar 'toml)))

(use-package inf-evcxr
  :requires tomlparse
  ;; :straight (:host codeberg :repo "jasalt/inf-evcxr")
  :load-path "~/dev/rust/inf-evcxr"
  :commands inf-evcxr
  :bind (:map rustic-mode-map
         ("C-c C-e" . inf-evcxr-eval-region-or-buffer)
         ("C-M-x" . inf-evcxr-eval-line)))
