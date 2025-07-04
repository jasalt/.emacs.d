(use-package rustic)

(use-package tomlparse
  :init
  (add-to-list 'treesit-language-source-alist
               '(toml "https://github.com/tree-sitter-grammars/tree-sitter-toml"))
  (unless (treesit-language-available-p 'toml)
    (treesit-install-language-grammar 'toml)))

(use-package inf-evcxr
  :requires tomlparse
  ;; :straight (:host codeberg :repo "jasalt/inf-evcxr")
  :load-path "~/dev/rust/inf-evcxr"
  :commands inf-evcxr
  :bind (:map rustic-mode-map
         ("C-c C-e" . inf-evcxr-eval-region-or-buffer)
         ("C-M-x" . inf-evcxr-eval-line)))


(defvar mistty-buffer-name "*mistty*" "Name of the mistty buffer for evcxr")

(defun inf-evcxr-blink-region (start end)
  "Make the text between START and END blink."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'success)
    (run-at-time 0.1 nil 'delete-overlay overlay)))

(defun inf-evcxr-get-or-set-process-target (arg)
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

(defun inf-evcxr-send-text-to-process (text)
  "Send the given text to the process buffer."
  (inf-evcxr-get-or-set-process-target nil)
  (message "sending")
  (message text)
  (process-send-string process-target text)

  (let ((buf-name (car (last (split-string process-target " " t)))))
    (when (string= buf-name "*mistty*")
      (with-current-buffer buf-name
        (call-interactively 'mistty-send-command)))))

(defun inf-evcxr-repl ()
  "Starts or opens existing REPL process mistty buffer in current window.
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
      ;; (message mistty-repl-command)
      (inf-evcxr-send-text-to-process "evcxr"))))

(defun inf-evcxr-eval-line ()
  "Send the line at point to the process buffer."
  (interactive)
  (save-excursion
	(inf-evcxr-blink-region (pos-bol) (pos-eol))
	  (inf-evcxr-send-text-to-process
	   (buffer-substring-no-properties (pos-bol) (pos-eol)))
	))


(defun inf-evcxr-eval-region ()
  (interactive)
  (save-excursion
	(inf-evcxr-blink-region (region-beginning) (region-end))
	(inf-evcxr-send-text-to-process
	   (buffer-substring-no-properties (region-beginning) (region-end)))
	;; (message
	;;    (buffer-substring-no-properties (region-beginning) (region-end)))
    ))
