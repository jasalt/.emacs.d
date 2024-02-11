;; Programming related config

;; Also refer to extras/dev.el which sets some Bedrock framework defaults
;; which might be merged with this file content at some point.

;; TOC:
;; - General LSP config
;; - Language specific configs for LSP, DAP, tree-sitter, etc.


;;;;; LSP
;; Using lsp-mode instead of built-in eglot because it promises to integrate
;; more things (including debugging) out of the box.

(use-package company :ensure t)  ;; lsp-mode default completion
(use-package yasnippet :ensure t)  ;; lsp-mode default snippets

;;https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"  ; (few alternatives - "C-l", "C-c l")
	
	treemacs-space-between-root-nodes nil
	company-minimum-prefix-length 1
	company-idle-delay 0.0 ;php-mode recommend?
	lsp-idle-delay 0.1 ;php-mode recommend?
	
	;; https://emacs-lsp.github.io/lsp-mode/page/performance/
	read-process-output-max (* 1024 1024)
	gc-cons-threshold (* 100 1024 1024)
	
	)  
  
  
  :hook ( ; Enable lsp-mode to following language modes
	 (python-ts-mode . lsp)
	 (clojure-mode . lsp)
	 (clojure-script-mode . lsp)
	 (clojurec-mode . lsp)
	 (php-ts-mode . lsp)

	 ;; Which key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-treemacs :ensure t
  ;;lsp-treemacs-errors-list
  ) ; TODO

;; TODO if startup is slow, defer with eg.
;; (use-package lsp-mode
;;     :hook (XXX-mode . lsp-deferred)
;;     :commands (lsp lsp-deferred))

;; Optionally
;; (use-package lsp-ui :commands lsp-ui-mode)
;; TODO Err: lsp--auto-configure: Cannot open load file: No such file or directory, lsp-ui



;; optionally if you want to use debugger  ; TODO
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package dap-mode :ensure t
  :init
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions controls tooltip))
  )
;; (setq dap-print-io t) ; print debug info into *Messages*

;; PYTHON


(use-package lsp-pyright
  :init
  (setq dap-python-debugger 'debugpy)  ;; TODO, no response
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
			 (require 'dap-python)
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


;;;; Clojure

;; https://emacs-lsp.github.io/lsp-mode/tutorials/clojure-guide/#basic-configuration

(use-package clojure-mode :ensure t
  :config (defun ed-clojure/eval-first-comment-sexp ()
	    (interactive)
	    (save-excursion
	      (re-search-forward "^(comment")
	      (forward-sexp)
	      (cider-eval-last-sexp)))
  )
(use-package cider :ensure t
  :init
  (setq
   lsp-enable-indentation nil ; use cider indentation instead of lsp, less strict is ok
   ;; lsp-enable-completion-at-point nil ; use cider completion instead of lsp
   )
  )

;; TODO :config (setq cider-enrich-classpath t)
;; https://docs.cider.mx/cider/config/basic_config.html#use-enrich-classpath


;; Jet (flexible replacement for jq)
;; Requires jet binary in path https://github.com/borkdude/jet/ and clojure-mode
(use-package jet :ensure t)  

;; TODO yasnippet
;; https://clojure-lsp.io/features/#snippets

;; TODO treemacs support
;; https://clojure-lsp.io/features/#project-tree
;; lsp-treemacs-call-hierarchy not working
;; https://clojure-lsp.io/features/#call-hierarchy


(use-package lua-mode :ensure t) 




;; PHP

;; Follow readme and install grammar at ~/.emacs.d/tree-sitter/
;; https://github.com/emacs-php/php-ts-mode
;; TODO check that .so file exists so this won't error at start without it
;; TODO update
(use-package php-ts-mode
  :straight (php-ts-mode :type git :host github :repo "emacs-php/php-ts-mode")
  :init
  (setq
   lsp-intelephense-format-braces "k&r"
   ;; Intelephense WP stubs https://marioyepes.com/blog/intelephense-wordpress-acf-genesis-conf/
   lsp-intelephense-stubs ["apache" "bcmath" "bz2" "calendar" "com_dotnet" "Core" "ctype" "curl" "date" "dba" "dom" "enchant" "exif" "fileinfo" "filter" "fpm" "ftp" "gd" "hash" "iconv" "imap" "interbase" "intl" "json" "ldap" "libxml" "mbstring" "mcrypt" "meta" "mssql" "mysqli" "oci8" "odbc" "openssl" "pcntl" "pcre" "PDO" "pdo_ibm" "pdo_mysql" "pdo_pgsql" "pdo_sqlite" "pgsql" "Phar" "posix" "pspell" "readline" "recode" "Reflection" "regex" "session" "shmop" "SimpleXML" "snmp" "soap" "sockets" "sodium" "SPL" "sqlite3" "standard" "superglobals" "sybase" "sysvmsg" "sysvsem" "sysvshm" "tidy" "tokenizer" "wddx" "xml" "xmlreader" "xmlrpc" "xmlwriter" "Zend OPcache" "zip" "zlib" "wordpress"]
   )
  )

;; (with-eval-after-load 'lsp-mode
;;   (require 'dap-php)
;;   (yas-global-mode))
