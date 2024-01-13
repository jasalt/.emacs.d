;; Programming related config

;; Also refer to extras/dev.el which sets some Bedrock framework defaults
;; which might be merged with this file content at some point.


(use-package clojure-mode :ensure t)
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
(use-package php-ts-mode
  :straight (php-ts-mode :type git :host github :repo "emacs-php/php-ts-mode"))

;; PYTHON

;; TODO how to activate instead of pyright ?
;; (use-package flymake-ruff
;;   :ensure t
;;   :hook (eglot-managed-mode . flymake-ruff-load))

;;sudo npm install --global pyright

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


(use-package dap-mode :ensure t
  :config (progn
	    ;; (dap-mode 1)

	    ;; ;; The modes below are optional

	    ;; (dap-ui-mode 1)
	    ;; ;; enables mouse hover support
	    ;; (dap-tooltip-mode 1)
	    ;; ;; use tooltips for mouse hover
	    ;; ;; if it is not enabled `dap-mode' will use the minibuffer.
	    ;; (tooltip-mode 1)
	    ;; ;; displays floating panel with debug buttons
	    ;; ;; requies emacs 26+
	    ;; (dap-ui-controls-mode 1)
	    ))
