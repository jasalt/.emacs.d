;; PHP programming config

;; TODO

;; Refer to lsp-mode setting in programming.el

;; (add-hook 'php-mode-hook 'tree-sitter-mode)
;; (add-hook 'php-mode-hook 'web-mode)

;; (setq require-final-newline nil)

;; Follow readme and install grammar at ~/.emacs.d/tree-sitter/
;; https://github.com/emacs-php/php-ts-mode
;; TODO check that .so file exists so this won't error at start without it
;; TODO update

(setq
 lsp-intelephense-format-braces "k&r"
 ;; Intelephense WP stubs https://marioyepes.com/blog/intelephense-wordpress-acf-genesis-conf/
 lsp-intelephense-stubs ["apache" "bcmath" "bz2" "calendar" "com_dotnet" "Core" "ctype" "curl" "date" "dba" "dom" "enchant" "exif" "fileinfo" "filter" "fpm" "ftp" "gd" "hash" "iconv" "imap" "interbase" "intl" "json" "ldap" "libxml" "mbstring" "mcrypt" "meta" "mssql" "mysqli" "oci8" "odbc" "openssl" "pcntl" "pcre" "PDO" "pdo_ibm" "pdo_mysql" "pdo_pgsql" "pdo_sqlite" "pgsql" "Phar" "posix" "pspell" "readline" "recode" "Reflection" "regex" "session" "shmop" "SimpleXML" "snmp" "soap" "sockets" "sodium" "SPL" "sqlite3" "standard" "superglobals" "sybase" "sysvmsg" "sysvsem" "sysvshm" "tidy" "tokenizer" "wddx" "xml" "xmlreader" "xmlrpc" "xmlwriter" "Zend OPcache" "zip" "zlib" "wordpress"])

(comment
 ;; Broken https://github.com/emacs-php/php-ts-mode/issues/72
 (use-package php-ts-mode
   :straight (php-ts-mode :type git :host github :repo "emacs-php/php-ts-mode")
   :hook (php-ts-mode . (lambda () (require 'dap-php) (lsp)))
   :bind
   (:map php-ts-mode-map
		 ;; "M-." from lsp-mode

		 ;; TODO REPL startup command
		 ;; ("C-c M-j" . php-repl)

		 ;; REPL evaluation commands similar to Emacs Lisp
         ;; ("C-M-x"   . phel-send-sexp-to-process) ; TODO
         ;; ("C-x C-e" . phel-send-sexp-to-process) ; TODO

		 ;; ("C-c C-e" . php-send-region-or-buffer-to-process) ;; from php-repl.el

		 ;; Custom binding for triggering functions while developing them in REPL
         ;; ("C-c C-c" . phel-send-first-comment-sexp-to-process) ; TODO

		 ;; Unit test runner with flexible development container setup
         ;; ("C-c C-t" . phel-run-tests) ; TODO

		 ;; Online documentation shortcuts TODO
         ;; ("C-c C-d C-p" . phel-phpdoc)
         ;; ("C-c C-d C-w" . phel-wpdoc)
         ;; ("C-c C-d C-d" . phel-doc)
		 ))
)

(use-package php-mode
  :pin melpa
  :hook
  ((php-mode . my-php-mode-init)
   (php-mode . (lambda () (require 'dap-php) (lsp))))
  :custom
  (php-mode-coding-style 'wordpress)
  (php-mode-template-compatibility nil)
  (php-imenu-generic-expression php-imenu-generic-expression-simple)
  :config
  (defun my-php-mode-init ()
    (setq-local ac-disable-faces '(font-lock-comment-face font-lock-string-face))
	(setq-local php-mode-template-compatibility t)
    ;;(add-hook 'hack-local-variables-hook 'php-ide-turn-on nil t)
	)

  ;; (when (require 'flycheck nil)
  ;;   (add-to-list 'flycheck-disabled-checkers 'php-phpmd)
  ;;   (add-to-list 'flycheck-disabled-checkers 'php-phpcs))

  )
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;; (setq-local show-trailing-whitespace t) ; superceded by ethan-wspace?

;; NOTE If getting startup error and "Cannot find module ... phpDebug.js"
;; when installing vscode ext with (dap-php-setup) in *Listen for XDebug stderr* output,
;; copy extension from another editor eg:
;; cp ~/.local/share/nvim/mason/packages/php-debug-adapter/extension \
;;    ~/.emacs.d/.extension/vscode/xdebug.php-debug/
