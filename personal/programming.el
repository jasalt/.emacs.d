;; Programming related config

;; Also refer to extras/dev.el which sets some Bedrock framework defaults
;; which might be merged with this file content at some point.

;; TOC:
;; - General code editing UI stuff
;; - LSP-Mode general config
;; - Language specific configs for LSP, DAP, tree-sitter, etc.

(use-package ethan-wspace
  :blackout t
  :config
  (setq mode-require-final-newline nil)
  (global-ethan-wspace-mode 1)
  )

(setq-default tab-width 4)

;; Using lsp-mode instead of built-in eglot because it integrates more features.

(use-package company  ;; lsp-mode default completion backend
  :straight (company :type git :host github :repo "company-mode/company-mode"))

;; UI enhancement, not restricted to buffer area, shows help tooltip, not for TTY
(use-package company-box
  :straight (company-box :type git :host github :repo "sebastiencs/company-box")
  :hook (company-mode . company-box-mode))

(use-package yasnippet
  :blackout t
  :hook
  ((clojure-ts-mode . yas-minor-mode)
   (org-mode . yas-minor-mode)
   ))

(use-package logview)

;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setenv "LSP_USE_PLISTS" "true")

;;https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"  ; (few alternatives - "C-l", "C-c l")

	treemacs-space-between-root-nodes nil
	company-minimum-prefix-length 3
	company-idle-delay 0.5 ;php-mode recommend?
	lsp-idle-delay 0.5 ;php-mode recommend?
	lsp-file-watch-threshold 7000  ; increased from 1000, enough for WP projects

	;; https://emacs-lsp.github.io/lsp-mode/page/performance/
	read-process-output-max (* 1024 1024)
	gc-cons-threshold (* 100 1024 1024))
  :hook (
		 ;; (yaml-ts-mode . lsp-mode)
		 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; Optionally
(use-package lsp-ui :hook (lsp-mode . lsp-ui-mode)
  ;;:init (lsp-ui-sideline-toggle-symbols-info)
  ;; :config (setq lsp-ui-doc-position 'bottom)
  ;; :commands lsp-ui-mode
  )

;; TODO if startup is slow, defer with eg.
;; (use-package lsp-mode
;;     :hook (XXX-mode . lsp-deferred)
;;     :commands (lsp lsp-deferred))


(global-unset-key (kbd "<f10>")) ; unbind useless menu key, could be mode local

(use-package dap-mode
  :init
  (setq dap-auto-configure-features '(locals controls tooltip sessions expressions breakpoints)) ; repl
  :config (dap-ui-mode 1)
  (setq dap-ui-buffer-configurations
	'(("*dap-ui-locals*"
	  (side . right)
	  (slot . 1)
	  ;;(window-height . 0.6)
	  (window-width . 0.25))
	  ("*dap-ui-sessions*"
	   (side . right)
	   (slot . 3)
	   ;;(window-width . 0.2)
	   (window-height . 0.1))
	  ("*dap-ui-expressions*"
	   (side . right)  ;; default right
	   (slot . 2)
	   (window-height . 0.1)
	   )
	 ("*dap-ui-breakpoints*"
	  (side . right)  ;; defaults to left
	  (slot . 4)
	  (window-height . 0.2)
	  ;;(window-width . 26)
	  )
	 ("*debug-window*"
	  (side . bottom)
	  (slot . 2)
	  (window-width . 0.2))
	 ("*dap-ui-repl*"
	  (side . bottom)
	  (slot . 1)
	  (window-height . 0.10))))

  :bind (("<f5>" . dap-continue) ;; TODO (dap--get-sessions) (dap--cur-session-or-die)
	 ("C-<f5>" . dap-debug)
	 ("S-<f5>" . dap-debug-restart)
	 ("<f10>" . dap-next))  ; todo activate also? if php, use default profile
  :custom (dap-ui-controls-screen-position 'posframe-poshandler-frame-bottom-right-corner)
  (dap-ui-locals-expand-depth t)  ; TODO not working?
  ;; (setq dap-print-io t) ; print debug info into *Messages*
  )


;; PYTHON
;; sudo npm install --global pyright  ; TODO needed at all? Should auto-install.
;; pipx install ruff ruff-lsp

(comment
 "Making (python-shell-send-region) work with Django manage.py"

 ;; Working static setting
 (setq python-shell-interpreter "/home/user/dev/django-toybox/.venv/bin/ipython"
       python-shell-interpreter-args "--simple-prompt -i /home/user/dev/django-toybox/manage.py shell_plus")
 )

(defun set-django-root-for-python-shell ()
   "Ask user for Django project root having manage.py and .venv to use with
    (run-python). Experimental but initially working. WIP...

    Expects .venv to be in project dir, eg. with poetry requires:
    poetry config virtualenvs.in-project true"
   ;; - TODO (automate) recursively search upwards for manage.py to decide dir automatically,
   ;; - TODO (automate) check if ipython exists and use python if not
   ;; - TODO (hook) python-ts-mode :init (?)
   (interactive)
   (let ((abs-dir-name (expand-file-name (read-directory-name "Select Django root directory (with manage.py): "))))
     (message "Selected: %s" abs-dir-name)
     (setq python-shell-interpreter (concat abs-dir-name ".venv/bin/" "ipython")
	   python-shell-interpreter-args (concat "--simple-prompt -i " (concat abs-dir-name "manage.py") " shell_plus"))))

;; Fix unreadable ipython term traceback colors in global ipython profile settings
;; Run `ipython profile create', then set in : ~/.ipython/profile_default/ipython_config.py:
;; c.InteractiveShell.colors = 'nocolor'
;; -- https://jsstevenson.github.io/blog/2022/custom-ipython-colors/

(use-package emacs
  :hook (python-ts-mode . lsp-deferred))

(use-package lsp-pyright  ; https://emacs-lsp.github.io/lsp-pyright/
  ;; :init
  ;; (setq dap-python-debugger 'debugpy)  ;; TODO, no response
  :config
  (setq lsp-pyright-disable-organize-imports t)  ; ruff does this
  ;; (setq lsp-pyright-auto-import-completions nil)  ; ruff does this too?
  ;; (setq lsp-pyright-typechecking-mode "strict") ; defaults to basic
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
			 ;; (require 'dap-python)
                         (lsp)))) ; or lsp-deferred


;; NOTE if fails at start with type errors, go to ~/.emacs.d/elpa and run:
;; find . -type f -name '*.elc' -delete


;; Pyright requires config file per project.
;; Does Neovim assist pyright to check the .venv dir automatically?
;; -> When NeoVim is started in activated venv, pyright ran by it finds correct python

(defun pyrightconfig-write (virtualenv)
  "Helper function that attempts to create pyright config file.
   Copied from somewhere..."
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


;;;; Clojure

;; TODO https://clojure-lsp.io/features/#snippets
;; https://emacs-lsp.github.io/lsp-mode/tutorials/clojure-guide/#basic-configuration

(use-package clojure-mode
  :hook ((clojure-mode . lsp)
	 (clojure-script-mode . lsp)
	 (clojurec-mode . lsp)))

(use-package cider
  :config
  (defun ed-clojure/eval-first-comment-sexp ()
	(interactive)
	(save-excursion
	  (re-search-forward "^(comment")
	  (forward-sexp)
	  (cider-eval-last-sexp)))
  (cider-register-cljs-repl-type 'sci-js "(+ 1 2 3)")
  :init
  (setq
   lsp-enable-indentation nil ; use cider indentation instead of lsp, less strict is ok
   ;; lsp-enable-completion-at-point nil ; use cider completion instead of lsp
   lsp-lens-enable nil
   cider-use-tooltips nil
   cider-repl-display-help-banner nil
   cider-connection-message-fn #'cider-random-tip
   ;;cider-repl-pop-to-buffer-on-connect nil
   cider-repl-pop-to-buffer-on-connect 'display-only ; show but don't focus
   cider-repl-buffer-size-limit 100000
   cider-use-overlays 'errors-only
   cider-save-file-on-load t
   clojure-toplevel-inside-comment-form t ; eval inside comment form
   )
  :custom (cider-enrich-classpath t "Enable experimental enrich classpath")

  ;; :config (setq cider-enrich-classpath t) ; TODO
  ;; https://docs.cider.mx/cider/config/basic_config.html#use-enrich-classpath
  )

;; https://github.com/babashka/scittle/tree/main/doc/nrepl
(defun mm/cider-connected-hook ()
	(when (eq 'sci-js cider-cljs-repl-type)
      (setq-local cider-show-error-buffer nil)
      (cider-set-repl-type 'cljs)))

(add-hook 'cider-connected-hook #'mm/cider-connected-hook)

(use-package clay
  :straight (clay :type git
				  :host github
				  :repo "scicloj/clay.el"))

;; Jet (flexible replacement for jq)
;; Requires jet binary in path https://github.com/borkdude/jet/ and clojure-mode
(use-package jet)

;; TODO treemacs support
;; https://clojure-lsp.io/features/#project-tree
;; lsp-treemacs-call-hierarchy not working
;; https://clojure-lsp.io/features/#call-hierarchy

(use-package lua-mode)


;;;;; HTML editing
;; Using web-mode, and lsp-mode with html-lsp and emmet-ls

(use-package web-mode
  ;; TODO incompatibility with lsp-mode possible
  ;; lsp-format does not handle {% %} {{ }} template tags
  :init (add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
  :hook ((web-mode . yas-minor-mode) (web-mode . lsp-deferred))
  :config (add-to-list 'lsp-language-id-configuration '("\\.twig" . "html"))
  )

(comment ; disabled, indent was not compatible with template tags
 (use-package mhtml-mode
  :init (add-to-list 'auto-mode-alist '("\\.twig\\'" . mhtml-mode))
  :hook ((mhtml-mode . yas-minor-mode)(mhtml-mode . lsp-deferred))
  :config
  ;; Remove overlapping of some personal bindings
  (define-key mhtml-mode-map (kbd "C-c 1") nil)
  (define-key mhtml-mode-map (kbd "C-c 2") nil)
  (define-key mhtml-mode-map (kbd "C-c 3") nil)
  (define-key mhtml-mode-map (kbd "C-c 4") nil)
  ))


;; PHP

;; Follow readme and install grammar at ~/.emacs.d/tree-sitter/
;; https://github.com/emacs-php/php-ts-mode
;; TODO check that .so file exists so this won't error at start without it
;; TODO update
(use-package php-ts-mode
  :straight (php-ts-mode :type git :host github :repo "emacs-php/php-ts-mode")
  :config
  (setq
   lsp-intelephense-format-braces "k&r"
   ;; Intelephense WP stubs https://marioyepes.com/blog/intelephense-wordpress-acf-genesis-conf/
   lsp-intelephense-stubs ["apache" "bcmath" "bz2" "calendar" "com_dotnet" "Core" "ctype" "curl" "date" "dba" "dom" "enchant" "exif" "fileinfo" "filter" "fpm" "ftp" "gd" "hash" "iconv" "imap" "interbase" "intl" "json" "ldap" "libxml" "mbstring" "mcrypt" "meta" "mssql" "mysqli" "oci8" "odbc" "openssl" "pcntl" "pcre" "PDO" "pdo_ibm" "pdo_mysql" "pdo_pgsql" "pdo_sqlite" "pgsql" "Phar" "posix" "pspell" "readline" "recode" "Reflection" "regex" "session" "shmop" "SimpleXML" "snmp" "soap" "sockets" "sodium" "SPL" "sqlite3" "standard" "superglobals" "sybase" "sysvmsg" "sysvsem" "sysvshm" "tidy" "tokenizer" "wddx" "xml" "xmlreader" "xmlrpc" "xmlwriter" "Zend OPcache" "zip" "zlib" "wordpress"])
  :hook (php-ts-mode . (lambda () (require 'dap-php) (lsp))))

;; NOTE If getting startup error and "Cannot find module ... phpDebug.js"
;; when installing vscode ext with (dap-php-setup) in *Listen for XDebug stderr* output,
;; copy extension from another editor eg:
;; cp ~/.local/share/nvim/mason/packages/php-debug-adapter/extension ~/.emacs.d/.extension/vscode/xdebug.php-debug/


;; Javascript

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))


(use-package copilot  ; TODO move to cp.el (WIP)
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))

	   ;; :hook (prog-mode . copilot-mode)  ;; manual startup for now
  :bind (("C-c M-f" . copilot-complete)
	 :map copilot-completion-map
	 ("C-g" . 'copilot-clear-overlay)
	 ("M-p" . 'copilot-previous-completion)
	 ("M-n" . 'copilot-next-completion)
	 ("<tab>" . 'copilot-accept-completion)
	 ("M-f" . 'copilot-accept-completion-by-word)
	 ("M-<return>" . 'copilot-accept-completion-by-line))
  :config
  ; https://github.com/copilot-emacs/copilot.el/issues/249
  (add-to-list
	   'copilot-indentation-alist
	   '(emacs-lisp-mode 2)))
