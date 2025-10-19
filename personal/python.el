;; Python specific config

;; https://emacs-lsp.github.io/lsp-pyright/
;; https://docs.basedpyright.com/latest/installation/command-line-and-language-server/
(use-package lsp-pyright  ;; uv tool install basedpyright
  ;; :init
  ;; (setq dap-python-debugger 'debugpy)  ;; TODO, no response
  :custom (lsp-pyright-langserver-command "basedpyright")
  :config
  (setq lsp-pyright-disable-organize-imports t)	; ruff does this
  :hook
  (python-ts-mode . (lambda ()
					  (require 'lsp-pyright)
					  (lsp-deferred))))

(use-package flycheck
  :diminish flycheck-mode
  :init
  (setq flycheck-check-syntax-automatically '(save new-line)
        flycheck-idle-change-delay 5.0
        flycheck-display-errors-delay 0.9
        flycheck-highlighting-mode 'symbols
        flycheck-indication-mode 'left-fringe
        flycheck-standard-error-navigation t
        flycheck-deferred-syntax-check nil))

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; (require 'dap-python)

;; NOTE: if fails at start with type errors, go to ~/.emacs.d/elpa and run:
;; find . -type f -name '*.elc' -delete


;; Work in progress Python REPL config for use with iPython & `uv run manage.py shell'
;; working on top of `python.el' (python-ts-mode) and leveraging Tree sitter where
;; applicable. Takes inspiration from phel.el using MisTTY for subprocess communication.

;; NOTE: See ~/.ipython/profile_default/ipython_config.py for configuration
;; options such as removing color with c.InteractiveShell.colors = 'nocolor'
;; if required. Initialize with `ipython profile create' if missing.

;; Uses functions from python.el:
;; - python-info-looking-at-beginning-of-defun
;; - python-info-looking-at-beginning-of-block

(defun my-python-ts-mode-setup ()
  "Custom setup for python-ts-mode."
  (define-key python-ts-mode-map (kbd "C-c C-e") 'python-send-region-or-buffer-to-process))

(add-hook 'python-ts-mode-hook 'my-python-ts-mode-setup)

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
  "Send TEXT to the REPL process. Detect IPython in MisTTY and use terminal bracketed paste to send the entire block at once; otherwise send plain text with a terminating blank line (two newlines) to end blocks."
  (python-get-or-set-process-target nil)
  (let* ((buf-name (car (last (split-string process-target " " t))))
         (is-mistty (and buf-name (string-prefix-p "*mistty*" buf-name)))
         (clean text)
         (use-bracketed-paste
          (and is-mistty
               (with-current-buffer buf-name
                 (save-excursion
                   (goto-char (point-max))
                   (re-search-backward "^In \\[[0-9]+\\]:" (max (point-min) (- (point-max) 2000)) t))))))
    ;; Ensure exactly two trailing newlines so compound statements are executed
    (while (string-match-p "\n\\'" clean)
      (setq clean (substring clean 0 -1)))
    (setq clean (concat clean "\n\n"))
    (let ((payload (if use-bracketed-paste
                       (concat "\e[200~" clean "\e[201~")
                     clean)))
      (process-send-string process-target payload)
	  (when is-mistty
		(with-current-buffer buf-name
		  (call-interactively 'mistty-send-command))))))

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
  "Prepare source code before evaluating."
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
