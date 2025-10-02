;; Work in progress Python REPL config for use with iPython & `uv run manage.py shell'
;; working on top of `python.el' (python-ts-mode) and leveraging Tree sitter where
;; applicable. Takes inspiration from phel.el using MisTTY for subprocess communication.

(define-key python-ts-mode-map (kbd "C-c C-e") 'python-send-region-or-buffer-to-process)

(comment
 (define-key python-ts-mode-map (kbd "C-c M-j") 'python-repl)

 (define-key python-ts-mode-map (kbd "C-M-x") 'python-send-sexp-to-process)
 (define-key python-ts-mode-map (kbd "C-x C-e") 'python-send-sexp-to-process)

 (define-key python-ts-mode-map (kbd "C-c C-c") 'python-send-first-comment-sexp-to-process)

 (define-key python-ts-mode-map (kbd "C-c C-t") 'python-run-tests)
 (define-key python-ts-mode-map (kbd "C-c M-t") 'python-switch-test-ns)

 )

(defun python-get-or-set-process-target (arg)
  "Get the current process target or set a new one if needed.
   TODO duplicate of python-get-or-set-process-target"
  (if (or arg
          (not (boundp 'process-target))
          (not (process-live-p (get-buffer-process process-target))))
      (setq process-target
            (completing-read
             "Process: "
             (seq-map (lambda (el) (buffer-name (process-buffer el)))
                      (process-list))))
    process-target))

(defun python-send-text-to-process (text)
  "Send the given text to the process buffer. Phel code being sent to REPL
  should be processed beforehand to avoid some quirks."
  (python-get-or-set-process-target nil)
  (process-send-string process-target text)

  (let ((buf-name (car (last (split-string process-target " " t)))))
    (when (string= buf-name "*mistty*")
      (with-current-buffer buf-name
        (call-interactively 'mistty-send-command)))))

(defun python-send-region-or-buffer-to-process (arg &optional beg end)
  "Send the current buffer or region to a process buffer. The first time it's
  called, will prompt for the buffer to send to. Subsequent calls send to the
  same buffer, unless a prefix argument is used (C-u), or the buffer no longer
  has an active process."
  (interactive "P\nr")
  (python-get-or-set-process-target arg)

  (let ((text (if (use-region-p)
				  (buffer-substring-no-properties beg end)
                (buffer-substring-no-properties (point-min) (point-max)))))
    (python-send-text-to-process text
	 ;; (python-process-source text)
	 )))

(defun python-process-source (code)
  "Prepare PHP source code to be evaluated in PHP REPL process
   E.g. 'python -a' or 'wp shell' in comint (mistty) process."
  (with-temp-buffer
    (insert code)
    (print-buffer-to-messages "at input")

    ;; Delete comments (everything on each line after #)
    ;; (goto-char (point-min))
    ;; (while (re-search-forward "//.*$" nil t)
    ;;   (replace-match ""))

    ;; Replace each newline character '\n' with space ' '
    ;; (goto-char (point-min))
    ;; (while (search-forward "\n" nil t)
    ;;   (replace-match " "))

	;; Replace tab characters triggering shell auto-complete with 4 spaces (depends on tab-space value)
	;; (goto-char (point-min))
    ;; (while (search-forward "\t" nil t)
    ;;   (replace-match "    "))

    ;; Delete all empty lines
    ;; (goto-char (point-min))
    ;; (flush-lines "^\\s-*$")

    ;; (print-buffer-to-messages "after processing")

    (buffer-string)))
