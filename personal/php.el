;; PHP programming config

;; Rever to lsp-mode setting in programming.el


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
  :hook (php-ts-mode . (lambda () (require 'dap-php) (lsp)))
  :bind
  (:map php-ts-mode-map
		;; "M-." from lsp-mode

		;; TODO REPL startup command
		;; ("C-c M-j" . php-repl)

		;; REPL evaluation commands similar to Emacs Lisp
        ;; ("C-M-x"   . phel-send-sexp-to-process) ; TODO
        ;; ("C-x C-e" . phel-send-sexp-to-process) ; TODO
        ("C-c C-e" . php-send-region-or-buffer-to-process)

		;; Custom binding for triggering functions while developing them in REPL
        ;; ("C-c C-c" . phel-send-first-comment-sexp-to-process) ; TODO

		;; Unit test runner with flexible development container setup
        ;; ("C-c C-t" . phel-run-tests) ; TODO

		;; Online documentation shortcuts TODO
        ;; ("C-c C-d C-p" . phel-phpdoc)
        ;; ("C-c C-d C-w" . phel-wpdoc)
        ;; ("C-c C-d C-d" . phel-doc)
		))

;; NOTE If getting startup error and "Cannot find module ... phpDebug.js"
;; when installing vscode ext with (dap-php-setup) in *Listen for XDebug stderr* output,
;; copy extension from another editor eg:
;; cp ~/.local/share/nvim/mason/packages/php-debug-adapter/extension \
;;    ~/.emacs.d/.extension/vscode/xdebug.php-debug/

;; Hacky REPL setup copied from phel.el

;; NOTE Function definitions cannot be changed by default, triggering
;; PHP Fatal error:  Cannot redeclare asdf() (previously declared in php shell code...

;; So REPL process needs a restart or outdated runkit7 PHP mod needs to be used
;; for redefining functions:
;; https://www.php.net/manual/en/function.runkit7-function-redefine.php
;; Docs: https://www.php.net/runkit7 / https://github.com/runkit7/runkit7

;; Failed install attempt:
;; pecl channel-update pecl.php.net
;; sudo pecl install runkit7-alpha
;; echo "extension=runkit7.so" >> /etc/php/8.2/cli/php.ini

;; Should return the type but does ot find declaration yet:
;; php -a
;; echo gettype(runkit7_function_redefine);


(defun php-get-or-set-process-target (arg)
  "Get the current process target or set a new one if needed.
   TODO duplicate of phel-get-or-set-process-target"
  (if (or arg
          (not (boundp 'process-target))
          (not (process-live-p (get-buffer-process process-target))))
      (setq process-target
            (completing-read
             "Process: "
             (seq-map (lambda (el) (buffer-name (process-buffer el)))
                      (process-list))))
    process-target))

(defun php-send-text-to-process (text)
  "Send the given text to the process buffer. Phel code being sent to REPL
  should be processed beforehand to avoid some quirks."
  (php-get-or-set-process-target nil)
  (process-send-string process-target text)

  (let ((buf-name (car (last (split-string process-target " " t)))))
    (when (string= buf-name "*mistty*")
      (with-current-buffer buf-name
        (call-interactively 'mistty-send-command)))))

(defun php-send-region-or-buffer-to-process (arg &optional beg end)
  "Send the current buffer or region to a process buffer. The first time it's
  called, will prompt for the buffer to send to. Subsequent calls send to the
  same buffer, unless a prefix argument is used (C-u), or the buffer no longer
  has an active process."
  (interactive "P\nr")
  (php-get-or-set-process-target arg)

  (let ((text (if (use-region-p)
				  (buffer-substring-no-properties beg end)
                (buffer-substring-no-properties (point-min) (point-max)))))
    (php-send-text-to-process (php-process-source text))))

(defun php-process-source (code)
  "Prepare PHP source code to be evaluated in PHP REPL process
   E.g. 'php -a' or 'wp shell' in comint (mistty) process."
  (with-temp-buffer
    (insert code)
    (print-buffer-to-messages "at input")

    ;; Delete comments (everything on each line after //)
    (goto-char (point-min))
    (while (re-search-forward "//.*$" nil t)
      (replace-match ""))

    ;; Replace each newline character '\n' with space ' '
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match " "))

	;; Replace tab characters triggering shell auto-complete
	(goto-char (point-min))
    (while (search-forward "\t" nil t)
      (replace-match " "))

    ;; Delete all empty lines
    (goto-char (point-min))
    (flush-lines "^\\s-*$")

    (print-buffer-to-messages "after processing")

    (buffer-string)))
