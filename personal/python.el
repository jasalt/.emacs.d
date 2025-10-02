;; Python specific config

(comment
 "Making (python-shell-send-region) work with Django manage.py"

 ;; Working static setting
 (setq python-shell-interpreter "/home/user/dev/django-toybox/.venv/bin/ipython"
       python-shell-interpreter-args "--simple-prompt -i /home/user/dev/django-toybox/manage.py shell_plus")
 )

(defun set-django-root-for-python-shell ()
   "Ask user for Django project root having manage.py and .venv to use with
    (run-python). Experimental but initially working. WIP...

    Expects .venv to be in project dir, eg. with poetry requires:
    poetry config virtualenvs.in-project true"
   ;; - TODO (automate) recursively search upwards for manage.py to decide dir automatically,
   ;; - TODO (automate) check if ipython exists and use python if not
   ;; - TODO (hook) python-ts-mode :init (?)
   (interactive)
   (let ((abs-dir-name (expand-file-name (read-directory-name "Select Django root directory (with manage.py): "))))
     (message "Selected: %s" abs-dir-name)
     (setq python-shell-interpreter (concat abs-dir-name ".venv/bin/" "ipython")
	   python-shell-interpreter-args (concat "--simple-prompt -i " (concat abs-dir-name "manage.py") " shell_plus"))))

;; Fix unreadable ipython term traceback colors in global ipython profile settings
;; Run `ipython profile create', then set in : ~/.ipython/profile_default/ipython_config.py:
;; c.InteractiveShell.colors = 'nocolor'
;; -- https://jsstevenson.github.io/blog/2022/custom-ipython-colors/

(use-package emacs
  :hook (python-ts-mode . lsp-deferred))

;; https://emacs-lsp.github.io/lsp-pyright/#usage-notes
;; https://docs.basedpyright.com/latest/installation/command-line-and-language-server/
(use-package lsp-pyright	; https://emacs-lsp.github.io/lsp-pyright/
  ;; :init
  ;; (setq dap-python-debugger 'debugpy)  ;; TODO, no response
  :custom (lsp-pyright-langserver-command "basedpyright")
  :config
  (setq lsp-pyright-disable-organize-imports t)	; ruff does this
  ;; (setq lsp-pyright-auto-import-completions nil)  ; ruff does this too?
  ;; (setq lsp-pyright-typechecking-mode "strict") ; defaults to basic
  ;; :hook (python-mode . (lambda ()
  ;;                        (require 'lsp-pyright)
  ;;                        (lsp)))
  )

;; or lsp-deferred
;; (require 'dap-python)


;; NOTE if fails at start with type errors, go to ~/.emacs.d/elpa and run:
;; find . -type f -name '*.elc' -delete


;; Pyright requires config file per project.
;; Does Neovim assist pyright to check the .venv dir automatically?
;; -> When NeoVim is started in activated venv, pyright ran by it finds correct python

(defun pyrightconfig-write (virtualenv)
  "Helper function that attempts to create pyright config file.
   Copied from somewhere..."
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



;; Work in progress Python REPL config for use with iPython & `uv run manage.py shell'
;; working on top of `python.el' (python-ts-mode) and leveraging Tree sitter where
;; applicable. Takes inspiration from phel.el using MisTTY for subprocess communication.

(setq python-flymake-command '("ruff" "check" "--stdin-filename" "stdin.py" "-"))

;; Uses functions from python.el:
;; - python-info-looking-at-beginning-of-defun
;; - python-info-looking-at-beginning-of-block

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
  "Send TEXT to the REPL process. Detect IPython in MisTTY and use terminal bracketed paste there; otherwise send plain text with a terminating blank line to end blocks."
  (python-get-or-set-process-target nil)
  (let* ((buf-name (car (last (split-string process-target " " t))))
         (is-mistty (string= buf-name "*mistty*"))
         (clean text)
         (use-bracketed-paste
          (and is-mistty
               (with-current-buffer buf-name
                 (save-excursion
                   (goto-char (point-max))
                   (let ((line (buffer-substring-no-properties
                                (line-beginning-position) (line-end-position))))
                     (string-match-p "^In \\[[0-9]+\\]:" line))))))
         payload)
    ;; Ensure trailing newline; in non-bracketed mode add a blank line to terminate blocks.
    (unless (string-match-p "\n\\'" clean)
      (setq clean (concat clean "\n")))
    (if use-bracketed-paste
        ;; IPython: bracketed paste prevents auto-indentation; send an extra newline inside paste.
        (setq payload (concat "\e[200~" clean "\n\e[201~"))
      ;; Regular Python (>>>): do not use bracketed paste; send an extra newline to close blocks.
      (setq payload (concat clean "\n")))
    (process-send-string process-target payload)
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
