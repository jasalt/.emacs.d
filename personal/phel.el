;; Phel programming config

(use-package phel-mode  ; derived from clojure-mode
  :mode "\\.phel\\'"
  ;; workaround for lsp-warning coming from lsp hooked to clojure-mode
  :config (setq lsp-warn-no-matched-clients nil)

  :bind (("C-M-x" . phel-send-sexp-to-process)
		 ("C-x C-e" . phel-send-sexp-to-process)
		 ("C-c C-e" . phel-send-region-or-buffer-to-process)
		 ("C-c C-c" . phel-send-first-comment-sexp-to-process)
		 ("C-c C-t" . phel-run-tests)
		 ("C-c C-d C-p" . phel-phpdoc)
		 ("C-c C-d C-w" . phel-wpdoc)
		 ("C-c C-d C-d" . phel-doc)
		 ("M-." . phel-xref-find-definitions)
		 ))

(use-package mistty
  :bind (("C-c C-s" . mistty)))

(defun print-buffer-to-messages (&optional prefix)
  "Print the current buffer's contents to the *Messages* buffer for debugging.
  If PREFIX is provided, it is inserted at the specified location in the message."
  (interactive)
  (let* ((buffer-contents (buffer-substring-no-properties (point-min) (point-max)))
         (message-template "### Buffer contents ({prefix}):\n%s")
         (message-text (if prefix
                           (replace-regexp-in-string "{prefix}" prefix message-template)
                         (replace-regexp-in-string " ({prefix})" "" message-template))))
    (message message-text buffer-contents)))

(defun phel-process-source (code)
  "Prepare Phel source code to be evaluated by Phel REPL
   Workaround https://github.com/phel-lang/phel-lang/issues/766"
  (with-temp-buffer
    (insert code)

	(print-buffer-to-messages "at input")
    ;; Remove (ns ...) form around require-statements
	;; Workaround for https://github.com/phel-lang/phel-lang/issues/766
	(goto-char (point-min))

    (when (re-search-forward "^(ns\\s-+" nil t)
	  (let ((start (match-beginning 0)))
		;; Erase ending parenthesis
		(goto-char start)
		(forward-sexp)
		(backward-char)
		(delete-char 1)

		;; Erase the ns-form line
		(goto-char start)
		(beginning-of-line)
		(kill-line)))

	;; (print-buffer-to-messages "after ns removal")

	;; Remove comment forms
	(goto-char (point-min))
    (while (re-search-forward "(comment\\s-*\n" nil t)
	  (let ((start (match-beginning 0)))
        (goto-char start)
        (forward-sexp)
        (delete-region start (point))))

	;; Delete comments (everything on each line after # character)
	(goto-char (point-min))
    (while (re-search-forward "#.*$" nil t)
      (replace-match ""))

	;; Convert :require-file to php/require_once
    (goto-char (point-min))
    (while (search-forward "(:require-file " nil t)
      (replace-match "(php/require_once "))

    ;; Convert :require to require
    (goto-char (point-min))
    (while (search-forward "(:require " nil t)
      (replace-match "(require "))

	;; (print-buffer-to-messages "before removing whitespace")

	;; Delete all empty lines
	(goto-char (point-min))
    (flush-lines "^\\s-*$")

    (buffer-string)))

(defun phel-send-region-or-buffer-to-process (arg &optional beg end)
  "Send the current buffer or region to a process buffer. The first time it's
  called, will prompt for the buffer to send to. Subsequent calls send to the
  same buffer, unless a prefix argument is used (C-u), or the buffer no longer
  has an active process. Ref:
  - https://emacs.stackexchange.com/a/37889/42614
  - https://stackoverflow.com/a/7053298"
  (interactive "P\nr")
  (if (or arg ;; user asks for selection
          (not (boundp 'process-target)) ;; target not set
          ;; or target is not set to an active process:
          (not (process-live-p (get-buffer-process
                                process-target))))
      (setq process-target
            (completing-read
             "Process: "
             (seq-map (lambda (el) (buffer-name (process-buffer el)))
                      (process-list)))))

  ;; (process-send-region process-target beg end)  ; This was v1

  ;; Process Phel source-code to be REPL friendly before sending it to process
  ;; TODO modify 'let' to set 'beg' and 'end' to be (point-min) (point-max) if they are not set
  (let ((modified-region
		 (process-phel-source
		  (if (and beg end)
			  (buffer-substring-no-properties beg end)
			(buffer-substring-no-properties (point-min) (point-max))))))
    (process-send-string process-target modified-region))

  ;; If target buffer is *mistty*, also evaluate sent region
  ;; by calling missty-send-command
  (let ((buf-name (let ((str process-target))
					(setq parts (split-string str " " t))
					(car (last parts)))))  ; Extract "actual" buffer name

	(if (string= buf-name "*mistty*")
		(with-current-buffer buf-name
		  (call-interactively 'mistty-send-command)))))

(defun phel-send-sexp-to-process ()
  "Send the current Phel sexp to the process buffer."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (let ((start (point)))
        (send-phel-region-or-buffer-to-process nil start end)))))

(defun phel-send-first-comment-sexp-to-process ()
  "Evaluates first s-exp inside comment form e.g. for evaluating defn being written
   with pre-set args. Idea from ed at Clojurians Slack."
  (interactive)
  (save-excursion
	(re-search-forward "^(comment")
	(forward-sexp)
	(send-phel-defn-to-process)))


(defun phel-read-test-command ()
  "Obtains test runner command by traversing up in filesystem from buffer
  file path and reading it from first docker-compose.yml containing it as
  x-custom-data directive as following:

   services:
     ...
   volumes:
     ...
   x-custom-data:
     test-command: <the test runner command>

  Command is returned with 'cd' to the abspath of the docker-compose.yml:
  cd /home/user/sites/my-project/ && <the test runner command>"
  (interactive)
  (let ((file-path (buffer-file-name))
		(root-dir "/")
		(command nil))
	(while (and file-path (not (string= file-path root-dir)) (not command))
      (let ((docker-compose-path (expand-file-name "docker-compose.yml" file-path)))
		(when (file-exists-p docker-compose-path)
          (let* ((yaml-data (yaml-parse-string
							 (with-temp-buffer
                               (insert-file-contents docker-compose-path)
                               (buffer-string))))
				 (custom-data (gethash 'x-custom-data yaml-data)))
			(when custom-data
              (setq command (gethash 'test-command custom-data))
              (when command
				(setq command (concat "cd " (file-name-directory docker-compose-path) " && " command))))))
		(setq file-path (file-name-directory (directory-file-name file-path)))))
	(message "Test command:")
	(message command)
	command))


;; - obtain relative path to current buffer file from project root path (containing docker-compose.yml)
;; Could run quiet and show test result only if there's error

(defun phel-run-tests (&optional run-all)
  "Run tests for file or project, printing results in messages buffer.
  Expects default Phel project structure and config in docker-compose.yml.
  By default runs tests for current file. If passed universal argument, runs all
  tests for project. Opens results in new window."
  (interactive "P")
  (let* ((command (phel-read-test-command))
         (file (when (not run-all) (buffer-file-name)))
         (docker-compose-dir (file-name-directory
							  (locate-dominating-file (buffer-file-name) "docker-compose.yml")))
         (relative-file (when file
                          (replace-regexp-in-string
                           "src/"
                           "tests/"
                           (file-relative-name file docker-compose-dir))))
         (full-command (if relative-file
                           (concat command " " relative-file)
                         command)))
    (message "Running tests with command:")
	(message full-command)
    (let ((output (shell-command-to-string full-command)))
      (with-current-buffer (get-buffer-create "*Phel Test Results*")
        (erase-buffer)
        (insert output)
        (make-frame '((buffer-predicate . (lambda (buf) (eq buf (current-buffer)))))))
      (message "Tests completed. Results in *Phel Test Results* buffer.")))
  )

;; Simplified go to definition

(defvar phel-definition-regex
  "(\\(defn\\(-\\)?\\|def\\|defmacro\\)\\s-?%s\\b"
  "Regex template for finding Phel definitions. %s is replaced with the symbol name.")

(defun phel-xref-find-definitions (&optional arg)
  "Search for definition of symbol at point and navigate to it.
When given universal argument, run rgrep for the definition instead.
Uses xref for navigation and docker-compose.yml to determine project root."
  (interactive "P")
  (let* ((symbol (thing-at-point 'symbol t))
         (project-root (locate-dominating-file default-directory "docker-compose.yml"))
         (defn-regex (format phel-definition-regex (regexp-quote symbol))))
    (if arg
        (phel-xref-find-definitions-with-consult-ripgrep symbol project-root)
      (phel-xref-find-definitions-in-current-file symbol defn-regex))))

(defun phel-extract-symbol-name (symbol)
  "Extract SYMBOL name without namespace."
  (if (string-match-p "/" symbol)
      (car (last (split-string symbol "/")))
    symbol))

(defun phel-xref-find-definitions-with-consult-ripgrep (symbol project-root)
  "Run consult-ripgrep to find definition of SYMBOL in PROJECT-ROOT."
  (if project-root
      (let* ((default-directory project-root)
             (function-name (phel-extract-symbol-name symbol))
             (search-pattern (format phel-definition-regex (regexp-quote function-name)))
             (consult-ripgrep-args (concat consult-ripgrep-args " --no-ignore-vcs")))
        (xref-push-marker-stack)
        (consult-ripgrep default-directory search-pattern))
    (message "Project root not found. Cannot perform ripgrep search.")))

(defun phel-xref-find-definitions-in-current-file (symbol defn-regex)
  "Find definition of SYMBOL in current file using DEFN-REGEX."
  (let ((definition-point
         (save-excursion
           (goto-char (point-min))
           (when (re-search-forward defn-regex nil t)
             (match-beginning 0)))))
    (if definition-point
        (progn
          (xref-push-marker-stack)
          (goto-char definition-point)
          (recenter))
      (message "Definition not found in current file."))))

;; Documentation

(defun phel-open-doc-url (url-format)
  "Open documentation URL for the symbol at point."
  (let* ((symbol (thing-at-point 'symbol t))
         (function-name (phel-extract-symbol-name symbol))
         (url (format url-format function-name)))
    (browse-url url)))

(defun phel-doc ()
  "Navigate to PHP documentation for the symbol at point."
  (interactive)
  (phel-open-doc-url "https://phel-lang.org/documentation/api/#%s"))

(defun phel-phpdoc ()
  "Navigate to PHP documentation for the symbol at point."
  (interactive)
  (phel-open-doc-url "https://www.php.net/manual/en/function.%s.php"))

(defun phel-wpdoc ()
  "Navigate to WordPress documentation for the symbol at point."
  (interactive)
  (phel-open-doc-url "https://developer.wordpress.org/reference/functions/%s/"))
