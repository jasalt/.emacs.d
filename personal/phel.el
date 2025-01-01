;; Phel programming config

(use-package phel-mode  ; derived from clojure-mode
  :mode "\\.phel\\'"
  ;; workaround for lsp-warning coming from lsp hooked to clojure-mode
  :config (setq lsp-warn-no-matched-clients nil)

  :bind (("C-M-x" . phel-send-sexp-to-process)
		 ("C-x C-e" . phel-send-sexp-to-process)
		 ("C-c C-e" . phel-send-region-or-buffer-to-process)
		 ("C-c C-c" . phel-send-first-comment-sexp-to-process)
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
