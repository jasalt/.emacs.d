;; "Phel Cider Light" Phel programming config

;; TODO local vs container setup in progress, test / repl commands broken

;; Written during two days during winter cold largely with Claude Sonnet 3.5 LLM
;; and gptel.el. While surprisingly good experience for a quick proof of concept,
;; may come with hidden defects that need to be smoothed out by hand during use.

;; Attempts to emulate some useful Clojure and Elisp editing functionalities
;; for a more frugal yet still very usable Lisp environment hosted on PHP.

;; Differs in not being based on Nrepl but on basic stdio communication (comint)
;; because Nrepl does not exist yet for Phel.

;; Mistty is used for terminal hosting the REPL session which sometimes breaks
;; a bit and needs to be restarted but mostly works. It is "reserved" for Phel
;; usage for now so there may be some quirks when using it for different tasks.

(use-package mistty) ;; https://github.com/szermatt/mistty

;; Includes familiar 'clojure-mode' based major-mode and keybindings:

(use-package phel-mode
  :mode "\\.phel\\'"
  :bind
  (:map phel-mode-map
		;; 'xref-find-definitions' style in-buffer or project source navigation
		;; using regex search in-buffer and ripgrep for project and vendor libs
		("M-." . phel-xref-find-definitions)

		;; REPL startup command
		("C-c M-j" . phel-repl)

		;; REPL evaluation commands similar to Emacs Lisp
        ("C-M-x"   . phel-send-sexp-to-process)
        ("C-x C-e" . phel-send-sexp-to-process)
        ("C-c C-e" . phel-send-region-or-buffer-to-process)

		;; Custom binding for triggering functions while developing them in REPL
        ("C-c C-c" . phel-send-first-comment-sexp-to-process)

		;; Unit test runner with flexible development container setup
        ("C-c C-t" . phel-run-tests)

		;; Online documentation shortcuts
        ("C-c C-d C-p" . phel-phpdoc)
        ("C-c C-d C-w" . phel-wpdoc)
        ("C-c C-d C-d" . phel-doc))

  ;; Workaround for lsp-warning coming from LSP hooked via clojure-mode
  :config (setq lsp-warn-no-matched-clients nil))


;; Test runner 'phel-run-tests', REPL startup command 'phel-repl' and project
;; root selection for search depend on 'phel-config.php' at project dir or
;; it's parent dir


;; Interactive REPL evaluation setup inspired from:
;; - https://emacs.stackexchange.com/a/37889/42614
;; - https://stackoverflow.com/a/7053298

(defun phel-get-or-set-process-target (arg)
  "Get the current process target or set a new one if needed."
  (if (or arg
          (not (boundp 'process-target))
          (not (process-live-p (get-buffer-process process-target))))
      (setq process-target
            (completing-read
             "Process: "
             (seq-map (lambda (el) (buffer-name (process-buffer el)))
                      (process-list))))
    process-target))

(defun phel-send-text-to-process (text)
  "Send the given text to the process buffer. Phel code being sent to REPL
  should be processed beforehand to avoid some quirks."
  (phel-get-or-set-process-target nil)
  (process-send-string process-target text)

  (let ((buf-name (car (last (split-string process-target " " t)))))
    (when (string= buf-name "*mistty*")
      (with-current-buffer buf-name
        (call-interactively 'mistty-send-command)))))

(defun phel-process-source (code)
  "Prepare Phel source code to be evaluated in Phel REPL. Fixes some quirks and
  cleans up comments."
  (with-temp-buffer
    (insert code)
	;;(print-buffer-to-messages "at input")

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

	;; Remove comment forms, TODO does not take into account comment lines not
	;; having newline right after comment symbol
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

	;; Convert :require-file to php/require_once (related to issue 766)
    (goto-char (point-min))
    (while (search-forward "(:require-file " nil t)
      (replace-match "(php/require_once "))

    ;; Convert :require to require (related to issue 766)
    (goto-char (point-min))
    (while (search-forward "(:require " nil t)
      (replace-match "(require "))

	;; .. same for :use
    (goto-char (point-min))
    (while (search-forward "(:use " nil t)
      (replace-match "(use "))

	;; (print-buffer-to-messages "before removing whitespace")

	;; Delete all empty lines
	(goto-char (point-min))
    (flush-lines "^\\s-*$")

	;; (print-buffer-to-messages "after processing")

    (buffer-string)))

(defun phel-send-region-or-buffer-to-process (arg &optional beg end)
  "Send the current buffer or region to a process buffer. The first time it's
  called, will prompt for the buffer to send to. Subsequent calls send to the
  same buffer, unless a prefix argument is used (C-u), or the buffer no longer
  has an active process."
  (interactive "P\nr")
  (phel-get-or-set-process-target arg)

  (let ((text (if (use-region-p)
				  (buffer-substring-no-properties beg end)
                (buffer-substring-no-properties (point-min) (point-max)))))
    (phel-send-text-to-process (phel-process-source text))))

(defun phel-send-sexp-to-process ()
  "Send the Phel sexp at point to the process buffer."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (let ((start (point)))
        (phel-send-text-to-process
		 (phel-process-source (buffer-substring-no-properties start end)))))))

(defun phel-send-first-comment-sexp-to-process ()
  "Evaluates first s-exp inside comment form e.g. for evaluating defn being
  written with pre-set args. Idea from ed at Clojurians Slack. Requires form
  to be placed on newline after comment symbol."
  (interactive)
  (save-excursion
	(re-search-forward "(comment\\s-*\n")  ; TODO allow comment without newline
	(forward-sexp)
	(phel-send-sexp-to-process)))

;; Test runner and REPL startup command setup

(defun phel-find-project-root ()
  "Find the root directory of the Phel project."
  (locate-dominating-file (buffer-file-name) "phel-config.php"))

(defun phel-read-repl-command ()
  "Get the REPL command for the current Phel project."
  (let ((root (phel-find-project-root)))
    (when root
      (concat "cd " root " && ./vendor/bin/phel repl"))))

(defun phel-read-test-command ()
  "Get the test runner command for the current Phel project."
  (let ((root (phel-find-project-root)))
    (when root
      (concat "cd " root " && ./vendor/bin/phel test"))))

(defun phel-repl ()
  "Starts or opens existing Phel REPL process mistty buffer in current window.
  Expects buffer name to be '*mistty*'"
  (interactive)
  (if (and (boundp 'mistty-buffer-name)
           (get-buffer "*mistty*"))
      (progn
		;; (message "phel-repl: reusing existing *mistty* buffer")
		(switch-to-buffer "*mistty*"))
    (progn
      (mistty)
      (setq process-target (buffer-name (current-buffer)))
      ;; (setq mistty-repl-command (phel-read-repl-command))
      ;; (message mistty-repl-command)
      (phel-send-text-to-process (phel-read-repl-command)))))

;; TODO does not work locally (only with container)
(defun phel-run-tests (&optional run-all)
  "Run tests for file or project, printing results in messages buffer.
  Expects default Phel project structure having 'phel-config.php'.
  By default runs tests for current file. If passed universal argument, runs all
  tests for project. Opens results in new window for now, room for improvement."
  (interactive "P")
  (let* ((command (phel-read-test-command))
         (file (when (not run-all) (buffer-file-name)))
         (docker-compose-dir (file-name-directory
							  (locate-dominating-file
							   (buffer-file-name) "docker-compose.yml"))) ;; TODO change to phel-config.php
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
        (make-frame
		 '((buffer-predicate . (lambda (buf) (eq buf (current-buffer)))))))
      (message "Tests completed. Results in *Phel Test Results* buffer."))))

;; Simplified go-to definition

(defvar phel-definition-regex
  "(\\(defn\\(-\\)?\\|def\\|defmacro\\)\\s-?%s\\b"
  "Format string regex template for some Phel functions/macros creating top
  level bindings. '%s' is replaced with the symbol name.")

(defun phel-xref-find-definitions (&optional arg)
  "Search for definition of symbol at point and navigate to it.
  When given universal argument, run 'ripgrep' for the definition instead.
  Uses xref for navigation and 'docker-compose.yml' to determine project root."
  (interactive "P")
  (let* ((symbol (thing-at-point 'symbol t))
         (project-root (locate-dominating-file
						default-directory "docker-compose.yml"))
         (defn-regex (format phel-definition-regex (regexp-quote symbol))))
    (if arg
        (phel-xref-find-definitions-with-consult-ripgrep symbol project-root)
      (phel-xref-find-definitions-in-current-file symbol defn-regex))))

(defun phel-extract-symbol-name (symbol)
  "Extract 'symbol' name without namespace."
  (if (string-match-p "/" symbol)
      (car (last (split-string symbol "/")))
    symbol))

(defun phel-xref-find-definitions-with-consult-ripgrep (symbol project-root)
  "Run consult-ripgrep to find definition of 'symbol' in 'project-root'"
  (if project-root
      (let* ((default-directory project-root)
             (function-name (phel-extract-symbol-name symbol))
             (search-pattern (format phel-definition-regex
									 (regexp-quote function-name)))
             (consult-ripgrep-args (concat consult-ripgrep-args
										   " --no-ignore-vcs")))
        (xref-push-marker-stack)
        (consult-ripgrep default-directory search-pattern))
    (message "Project root not found. Cannot perform ripgrep search.")))

(defun phel-xref-find-definitions-in-current-file (symbol defn-regex)
  "Find definition of 'symbol' in current file using 'defn-regex'"
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

;; Misc

(defun print-buffer-to-messages (&optional prefix)
  "Print the current buffer's contents to the *Messages* buffer for debugging.
  If 'prefix' is provided, it is inserted at the specified location in the
  message."
  (interactive)
  (let* ((buffer-contents (buffer-substring-no-properties
						   (point-min) (point-max)))
         (message-template "### Buffer contents ({prefix}):\n%s")
         (message-text
		  (if prefix
              (replace-regexp-in-string "{prefix}" prefix message-template)
            (replace-regexp-in-string " ({prefix})" "" message-template))))
    (message message-text buffer-contents)))
