(defun php-get-or-set-process-target (arg)
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
  "Prepare PHP source code to be evaluated in PHP REPL process (wp shell in comint process)."
  (with-temp-buffer
    (insert code)
	;;(print-buffer-to-messages "at input")

	;; Delete comments (everything on each line after //)
	(goto-char (point-min))
    (while (re-search-forward "//.*$" nil t)
      (replace-match ""))

	;; TODO Replace each newline character '\n' with space ' '

	;; (print-buffer-to-messages "before removing whitespace")

	;; Delete all empty lines
	(goto-char (point-min))
    (flush-lines "^\\s-*$")

	(print-buffer-to-messages "after processing")

    (buffer-string)))
