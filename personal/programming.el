;; Programming related config

;; Also refer to extras/dev.el which sets some Bedrock framework defaults
;; which might be merged with this file content at some point.

;; TOC:
;; - General LSP config
;; - Language specific configs for LSP, DAP, tree-sitter, etc.


;;;;; LSP
;; Using lsp-mode instead of built-in eglot because it promises to integrate
;; more things (including debugging) out of the box.

(use-package company :ensure t)  ;; lsp-mode default completion mechanism

;;https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")  ; (few alternatives - "C-l", "C-c l")
  ;;https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq read-process-output-max (* 1024 1024))
  (setq gc-cons-threshold 100000000)
  :hook ((python-ts-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; TODO if startup is slow, defer with eg.
;; (use-package lsp-mode
;;     :hook (XXX-mode . lsp-deferred)
;;     :commands (lsp lsp-deferred))

;; Optionally
;; (use-package lsp-ui :commands lsp-ui-mode)
;; TODO Err: lsp--auto-configure: Cannot open load file: No such file or directory, lsp-ui

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)  ; TODO

;; optionally if you want to use debugger  ; TODO
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; (use-package dap-mode :ensure t
;;   :config (progn
;; 	    ;; (dap-mode 1)

;; 	    ;; ;; The modes below are optional

;; 	    ;; (dap-ui-mode 1)
;; 	    ;; ;; enables mouse hover support
;; 	    ;; (dap-tooltip-mode 1)
;; 	    ;; ;; use tooltips for mouse hover
;; 	    ;; ;; if it is not enabled `dap-mode' will use the minibuffer.
;; 	    ;; (tooltip-mode 1)
;; 	    ;; ;; displays floating panel with debug buttons
;; 	    ;; ;; requies emacs 26+
;; 	    ;; (dap-ui-controls-mode 1)
;; 	    ))


;; PYTHON

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))
					; or lsp-deferred

;;sudo npm install --global pyright  ; TODO needed at all?

;; Pyright requires config file per project.
;; Does Neovim assist pyright to check the .venv dir automatically?
;; -> When NeoVim is started in activated venv, pyright ran by it finds correct python

(defun pyrightconfig-write (virtualenv)
  (interactive "DEnv: ")

  (let* (;; file-truename and tramp-file-local-name ensure that neither `~' nor
	 ;; the Tramp prefix (e.g. "/ssh:my-host:") wind up in the final
	 ;; absolute directory path.
	 (venv-dir (tramp-file-local-name (file-truename virtualenv)))

	 ;; Given something like /path/to/.venv/, this strips off the trailing `/'.
	 (venv-file-name (directory-file-name venv-dir))

	 ;; Naming convention for venvPath matches the field for
	 ;; pyrightconfig.json.  `file-name-directory' gets us the parent path
	 ;; (one above .venv).
	 (venvPath (file-name-directory venv-file-name))

	 ;; Grabs just the `.venv' off the end of the venv-file-name.
	 (venv (file-name-base venv-file-name))

	 ;; Eglot demands that `pyrightconfig.json' is in the project root
	 ;; folder.
	 (base-dir (vc-git-root default-directory))
	 (out-file (expand-file-name "pyrightconfig.json" base-dir))

	 ;; Finally, get a string with the JSON payload.
	 (out-contents (json-encode (list :venvPath venvPath :venv venv))))

    ;; Emacs uses buffers for everything.  This creates a temp buffer, inserts
    ;; the JSON payload, then flushes that content to final `pyrightconfig.json'
    ;; location
    (with-temp-file out-file (insert out-contents))))


(use-package clojure-mode :ensure t
  :config (defun ed-clojure/eval-first-comment-sexp ()
	    (interactive)
	    (save-excursion
	      (re-search-forward "^(comment")
	      (forward-sexp)
	      (cider-eval-last-sexp)))
  )
(use-package cider :ensure t)
;; TODO :config (setq cider-enrich-classpath t)
;; https://docs.cider.mx/cider/config/basic_config.html#use-enrich-classpath


;; Jet (flexible replacement for jq)
;; Requires jet binary in path https://github.com/borkdude/jet/ and clojure-mode
(use-package jet :ensure t)  

(use-package lua-mode :ensure t) 

;; PHP

;; Follow readme and install grammar at ~/.emacs.d/tree-sitter/
;; https://github.com/emacs-php/php-ts-mode
;; TODO check that .so file exists so this won't error at start without it
(use-package php-ts-mode
  :straight (php-ts-mode :type git :host github :repo "emacs-php/php-ts-mode"))
