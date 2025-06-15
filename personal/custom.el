;; General editor settings.

;; Good pieces from Emacs Prelude bolted on top of Emacs Bedrock
;; plus personal additions scrapped from around the web.

;; New functionality is added to beginning of file so structure tends to get
;; out of hand every few months, refactoring once in a while..

;; TODO split Prelude stuff to it's own file

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(defmacro comment (&rest body)
  "Clojure-like comment function.
   Source: https://robjohnson.dev/posts/elisp-cheat-sheet-for-clojure-devs/."
  nil)

(defmacro when-linux (&rest body)
  "Evaluate BODY only when the system type is GNU/Linux."
  `(when (string-equal system-type "gnu/linux")
     ,@body))

(defmacro when-mac (&rest body)
  "Evaluate BODY only when the system type is GNU/Linux."
  `(when (string-equal system-type "darwing")
     ,@body))

(defmacro when-*nix (&rest body)
  "Evaluate BODY only when the system type is GNU/Linux."
  `(when (or (string-equal system-type "gnu/linux")
			 (string-equal system-type "darwin"))
     ,@body))

(defmacro when-windows (&rest body)
  "Evaluate BODY only when the system type is GNU/Linux."
  `(when (string-equal system-type "winnt")
     ,@body))


;; UPDATE CONFIG
(comment
 (use-package auto-package-update
   :config ;; TODO Automatic updating would be nice
   (setq auto-package-update-delete-old-versions t)
   (setq auto-package-update-hide-results t)
   (auto-package-update-maybe))

 ;; Meanwhile occasionally run:
 (package-upgrade-all)
 (straight-pull-all)
 )

; default to :ensure t for every package declaration
(setq use-package-always-ensure t)

(defun transparency (value)
  "Set the transparency of the frame window.  VALUE = 0-100."
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(when-mac
 (setq ns-use-native-fullscreen nil)
 (setq mac-option-modifier 'nil
       mac-command-modifier 'meta
       mac-function-modifier 'hyper)
 (set-variable 'magit-emacsclient-executable
               "/usr/local/bin/emacsclient")
 (set-face-attribute 'default nil :font "Hack-14")  ; "Inconsolata-16"
 ;; Cmd+Space Spotlight search binding workaround
 (global-unset-key (kbd " "))
 (global-set-key (kbd " ") 'just-one-space)
 )

(when-linux

 (custom-set-faces
  '(default ((t (:height 105 :family "Hack")))))

 ;; Save lock files to /var/tmp (fix problem with Python Werkzeug auto reload)
 (setq lock-file-name-transforms
       '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))

 )

(comment
 (use-package guess-language         ; WIP Automatically detect language for Flyspell
   ;; TODO replace with https://github.com/minad/jinx
   :ensure t
   :defer t
   :init (add-hook 'text-mode-hook #'guess-language-mode)
   :config
   (setq guess-language-langcodes '((en . ("en_GB" "English"))
                                    (fi . ("fi_FI" "Finnish")))
         guess-language-languages '(en it)
         guess-language-min-paragraph-length 45)
   :diminish guess-language-mode)

 ;; To download dictionary, http://aspell.net/aspell-0.60.html
 ;; ftp://ftp.gnu.org/gnu/aspell/dict/
 (defvar mu-languages-ring nil "Languages ring for Ispell")

 (let ((languages '("en_GB" "fi_FI")))
   (setq mu-languages-ring (make-ring (length languages)))
   (dolist (elem languages) (ring-insert mu-languages-ring elem)))

 (defun mu-cycle-ispell-languages ()
   (interactive)
   (let ((language (ring-ref mu-languages-ring -1)))
     (ring-insert mu-languages-ring language)
     (ispell-change-dictionary language)))
 )


(comment
 "forgot what this was about"
 (magit-add-section-hook 'magit-status-sections-hook
			 'magit-insert-modules
			 'magit-insert-unpulled-from-pushremote)
 )

;;;;; General
(when-linux
 (defun paste-from-selection ()
   "Paste from X11 primary selection as if pressing middle mouse button.
    TODO for Wayland maybe use (insert (string-trim (shell-command-to-string \"wl-paste\")))"
   (interactive)
   (insert (x-get-selection 'PRIMARY)))

(global-set-key (kbd "C-M-y") 'paste-from-selection))


(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(defun increase-text-and-pane ()
  "Increase text size and adjust window width proportionally."
  (interactive)
  (let* ((orig-scale (or (car (get 'text-scale-mode-amount 'customized-value))
                        text-scale-mode-amount))
         (new-scale (+ orig-scale 1))
         (scale-factor (/ (float (expt text-scale-mode-step new-scale))
                          (float (expt text-scale-mode-step orig-scale)))))
    (text-scale-increase 1)
    (enlarge-window-horizontally (round (* (window-width) (- scale-factor 1))))))

(global-set-key (kbd "C-M-+") 'increase-text-and-pane)

(defun decrease-text-and-pane ()
  "Decrease text size and adjust window width proportionally."
  (interactive)
  (let* ((orig-scale (or (car (get 'text-scale-mode-amount 'customized-value))
                        text-scale-mode-amount))
         (new-scale (- orig-scale 1))
         (scale-factor (/ (float (expt text-scale-mode-step new-scale))
                          (float (expt text-scale-mode-step orig-scale)))))
    (text-scale-decrease 1)
    (shrink-window-horizontally (round (* (window-width) (- 1 scale-factor))))))

(global-set-key (kbd "C-M-_") 'decrease-text-and-pane)


(global-set-key (kbd "C-x F") 'project-find-file)

;; re-mapped to consult-line in extras/base.el
(global-set-key (kbd "C-S-s") 'isearch-forward)

(menu-bar-mode -1)
(global-set-key (kbd "<f12>") 'menu-bar-mode)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Scrolling.html
(setq scroll-conservatively 101)  ; avoid recentering when moving cursor
(setq scroll-margin 0)  ; TODO would be nice to set as a mode specific setting

(size-indication-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode 1)  ; yanking replaces region content

;; C-S-v for pasting in terminal as in terminal emulator
(add-hook 'shell-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-S-v"))
            (local-set-key (kbd "C-S-v") 'yank)))

(use-package blackout)  ; helps keeps modeline clean
(use-package diminish)  ; helps keeps modeline clean (with use-package)
(blackout 'eldoc-mode)
(blackout 'which-key-mode)

;; Dired customization
(setq dired-listing-switches "-alh")  ;; default -al
;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))


;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" "~/.emacs.d/.cache/"))
(savehist-mode +1)

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" "~/.emacs.d/.cache/")
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 300) ; 'never
(defun prelude-recentf-exclude-p (file)
  "A predicate to decide whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file))))
    (cl-some (lambda (dir)
               (string-prefix-p dir file-dir))
             (mapcar 'file-truename (list "~/.emacs.d/" package-user-dir)))))
(recentf-mode +1)

(use-package super-save
  :init (super-save-mode +1)
  :config (progn
	    (add-to-list 'super-save-triggers 'ace-window)
	    (diminish 'super-save-mode)
	    ))
;;(require 'super-save)
;; add integration with ace-window

;; (defun prelude-cleanup-maybe ()
;;   "Invoke `whitespace-cleanup' if `prelude-clean-whitespace-on-save' is not nil."
;;   (when prelude-clean-whitespace-on-save
;;     (whitespace-cleanup)))

;; (defun prelude-enable-whitespace ()
;;   "Enable `whitespace-mode' if `prelude-whitespace' is not nil."
;;   (when prelude-whitespace
;;     ;; keep the whitespace decent all the time (in this buffer)
;;     (add-hook 'before-save-hook 'prelude-cleanup-maybe nil t)
;;     (whitespace-mode +1)))

;; (add-hook 'text-mode-hook 'prelude-enable-whitespace)


(load "server")
(setq server-name "jarkon-emacs")
(setq server-socket-dir "~/.emacs.d/.cache/server")
(unless (server-running-p) (server-start))

(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
  "Open file with emacsclient with cursors positioned on requested line.
Most of console-based utilities prints filename in format
'filename:linenumber'.  So you may wish to open filename in that format.
Just call:

  emacsclient filename:linenumber

and file 'filename' will be opened and cursor set on line 'linenumber'"
  (ad-set-arg 0
              (mapcar (lambda (fn)
                        (let ((name (car fn)))
                          (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                              (cons
                               (match-string 1 name)
                               (cons (string-to-number (match-string 2 name))
                                     (string-to-number (or (match-string 3 name) ""))))
                            fn)))
		      files)))

;; Make scratch-buffer more convenient
(setq initial-scratch-message "")
(setq initial-major-mode 'org-mode)

(defun show-or-create-scratch ()
  "Shows a scratch buffer or creates another one if visiting one."
  (interactive)
  (if (string-match "*scratch*" (buffer-name))
      (progn
	(switch-to-buffer (get-buffer-create (concat (buffer-name) "*")))
	(princ "Created a new scratch buffer."))
    (progn (switch-to-buffer
	    (get-buffer-create "*scratch*")))))

(global-set-key (kbd "C-c <escape>") 'show-or-create-scratch)
(global-set-key (kbd "C-c `") 'show-or-create-scratch)  ;; terminal workaround

(global-set-key [(shift mouse-3)] 'acme-search-forward)
(global-set-key [(mouse-8)] 'acme-search-forward)
(global-set-key [(shift mouse-8)] 'acme-search-backward)

;; Pop marks faster by repeated spacing
;; Eg. C-u <space> <space> <space>
(setq set-mark-command-repeat-pop 't)

(use-package key-chord)

;; Allow definions like (key-chord-define-global "jf" 'avy-goto-word-1)
;; with :chords key
(use-package use-package-chords
  :config (key-chord-mode 1))

(use-package multiple-cursors
  :chords
  ("jn" . mc/mark-more-like-this-extended)
  ("jp" . mc/mark-previous-like-this)
  ("jm" . mc/mark-all-like-this)
  ("jt" . mc/mark-sgml-tag-pair)
  ("kd" . mc/edit-lines)
  ("jf" . avy-goto-word-1)
  ("jl" . avy-goto-line))

(use-package mc-extras)

(use-package phi-search  ; MC compatible i-search
  :config
  (global-set-key (kbd "C-s") 'phi-search)
  (global-set-key (kbd "C-r") 'phi-search-backward))

;; BROKEN MC support :<
;; (use-package iy-go-to-char
;;   :straight (iy-go-to-char :type git :host github
;; 			   :repo "doitian/iy-go-to-char")
;;   :config
;;   (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)
;;   (key-chord-define-global "jj" 'iy-go-up-to-char)
;;   (key-chord-define-global "JJ" 'iy-go-up-to-char-backward)
;;   )

;; TODO NOT multi-cursor compatible, jumps every cursor to same location
;; Eg. https://github.com/doitian/iy-go-to-char/commit/c1b5d5317b85a6eadd75ba70062fe364e3356efc
;; (use-package jump-char  ; iy-go-to-char successor
;;   :init
;;   (key-chord-define-global "jj" 'jump-char-forward)
;;   (key-chord-define-global "JJ" 'jump-char-backward)
;;   )

;;  Not fitting well with multiple cursors
;; (use-package zop-to-char
;;   :bind (("M-z" . zop-up-to-char)
;; 	 ("M-Z" . zop-to-char)
;; 	 ))

(use-package undo-tree
  :init (progn
	  (setq undo-tree-history-directory-alist
		`((".*" . ,temporary-file-directory)))
	  (global-undo-tree-mode))
  :config (diminish 'undo-tree-mode)
  :bind ("C-x u" . undo-tree-visualize)
  )

(winner-mode +1)

(use-package smartrep)
(use-package operate-on-number)

(smartrep-define-key global-map "C-c ."
  '(("+" . apply-operation-to-number-at-point)
    ("-" . apply-operation-to-number-at-point)
    ("*" . apply-operation-to-number-at-point)
    ("/" . apply-operation-to-number-at-point)
    ("\\" . apply-operation-to-number-at-point)
    ("^" . apply-operation-to-number-at-point)
    ("<" . apply-operation-to-number-at-point)
    (">" . apply-operation-to-number-at-point)
    ("#" . apply-operation-to-number-at-point)
    ("%" . apply-operation-to-number-at-point)
    ("'" . operate-on-number-at-point)))

(use-package editorconfig
  :init (progn
	  (editorconfig-mode 1)
	  (diminish 'editorconfig-mode)
	  )
  )

;; Window splitting keys, same as Terminator / Konsole / iTerm
(global-set-key (kbd "C-S-o") (lambda () (interactive) (split-window-vertically nil)))
(global-set-key (kbd "C-M-o") '(lambda () (interactive) (split-window-horizontally nil)))
;; (global-set-key (kbd "C-M-w") '(lambda () (interactive) (delete-window)))
(global-set-key (kbd "C-S-w")
               (lambda () (interactive)
                  (if (= (length (window-list)) 1)
                      (delete-frame)
                    (delete-window))))

;; TODO has problems with treemacs mode with
;; (treemacs--select-visible-window) failing
(global-set-key (kbd "M-0") '(lambda () (interactive) (treemacs-select-window)))

(use-package winum
  :config (winum-mode)
  (setq winum-scope 'frame-local)
  ;; https://github.com/deb0ch/emacs-winum
  :bind (
	 ;; Prefer OS Window Manager binding
	 ;; ("M-`" . winum-select-window-by-number)
	 ;; Prefer treemacs default map
	 ;; ("M-0" . winum-select-window-0-or-10)
	 ;; ("M-0" . treemacs-select-window)
	 ("M-1" . winum-select-window-1)
	 ("M-2" . winum-select-window-2)
	 ("M-3" . winum-select-window-3)
	 ("M-4" . winum-select-window-4)
	 ("M-5" . winum-select-window-5)
	 ("M-6" . winum-select-window-6)
	 ("M-7" . winum-select-window-7)
	 ("M-8" . winum-select-window-8)
	 ("M-9" . winum-select-window-9)))


(use-package move-text
  :bind (
	 ("C-S-<up>" . move-text-up)
	 ("C-S-<down>" . move-text-down))
  )

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package crux
  ;; Prelude niceties from https://github.com/bbatsov/crux
  :config
  (global-set-key [remap kill-whole-line] 'crux-kill-whole-line)
  (crux-with-region-or-line kill-region)  ; C-w kills row, not random region TODO
  (unbind-key "C-S-<return>" org-mode-map)
  :bind (
	 ("C-a" . crux-move-beginning-of-line)
	 ("C-k" . crux-smart-kill-line)

	 ;; vscode style
	 ("C-<return>" . crux-smart-open-line)
	 ("C-S-<return>" . crux-smart-open-line-above)

	 ;; vim style
	 ("M-o" . crux-smart-open-line)
	 ("M-S-o" . crux-smart-open-line-above)

	 ("C-<backspace>" . crux-kill-line-backwards)

	 ("C-c d" . crux-duplicate-current-line-or-region)
	 ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)

	 ("C-c o" . crux-open-with)
	 ("C-c f" . crux-recentf-find-file)
	 ("C-c F" . crux-recentf-find-directory)

	 ("C-c u" . crux-view-url)
	 ("C-c e" . crux-eval-and-replace)
	 ("C-c D" . crux-delete-file-and-buffer)
	 ("C-c r" . crux-rename-file-and-buffer)
	 ("C-c t" . crux-visit-term-buffer)
	 ("C-c k" . crux-kill-other-buffers)
	 ;;("C-M z" . crux-indent-defun) ; err
	 ("C-c I" . crux-find-user-init-file)
	 ("C-c S" . crux-find-shell-init-file)

	 ("C-c s" . crux-swap-windows)
	 )
  )


(global-set-key (kbd "C-c C-SPC") 'comment-or-uncomment-region)


;; meaningful names for buffers with the same name
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" "~/.emacs.d/.cache/"))
;; activate it for all buffers
(save-place-mode 1)


;; Minimal writeroom-mode alternative
(use-package olivetti
  :config
  (custom-set-faces
  '(olivetti-fringe ((t (:background "gray" :foreground "white")))))
  :custom
  (olivetti-style 'fancy)
  (olivetti-margin-width 8)
  (olivetti-body-width 80)
  )


(use-package tramp)  ; depends on
;; give /path/to/.venv

;; Fork https://github.com/jasalt/docker-nrepl.el
(use-package docker-nrepl
  :straight (:host github :repo "jasalt/docker-nrepl.el")
  :after cider
  :config
  (docker-nrepl-setup))

;; To set Mac Env vars that GUI Emacs (d12frosted/homebrew-emacs-plus) reads:
;; add to ~/Library/LaunchAgents/com.example.set-env-vars.plist
;; <?xml version="1.0" encoding="UTF-8"?>
;; <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
;; <plist version="1.0">
;; <dict>
;;   <key>Label</key>
;;   <string>setenv.MY_VARS</string>
;;   <key>ProgramArguments</key>
;;   <array>
;;     <string>sh</string>
;;     <string>-c</string>
;;     <string>launchctl setenv MY_VAR my_value</string>
;;   </array>
;;   <key>RunAtLoad</key>
;;   <true/>
;;   <key>KeepAlive</key>
;;   <true/>
;; </dict>
;; </plist>

(use-package org :pin gnu)  ; should get latest org-mode over built-in

;; Read environment values from shell environment
(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns x))
			(dolist (var '("ANONYMIZE_KEY" "GROQ_API_KEY" "CLAUDE_API_KEY" "OPENROUTER_API_KEY"))
			  (add-to-list 'exec-path-from-shell-variables var))
			(exec-path-from-shell-initialize)))


(use-package gptel ; https://github.com/karthink/gptel
  :straight t
  :bind ("C-x G" . gptel)
  :config

  (setq gptel-track-media t) ;; TODO works?
  ;; Local models

  (gptel-make-ollama "localhost"
    :stream t
    :models '(qwen3:4b gemma3:4b-it-qat))

  (gptel-make-ollama "mbp14"
    :host "js-mbp14:11434"
    :stream t
    :models '(qwen3:14b gemma3:12b-it-qat)
	)

  ;; Default / Openrouter
  (setq gptel-model 'deepseek/deepseek-chat-v3-0324:free
		gptel-backend
		(gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key (getenv "OPENROUTER_API_KEY")
          :models '(deepseek/deepseek-chat-v3-0324:free
					deepseek/deepseek-r1-0528:free
					openrouter/google/gemini-2.5-pro-preview)))

  ;; Claude
  (gptel-make-anthropic "Anthropic"
  	:stream t
  	:key (getenv "CLAUDE_API_KEY")
  	:models '("claude-4-sonnet-20250514" "claude-3-7-sonnet-20250219"))

  (gptel-make-gemini "Gemini" :key (getenv "GEMINI_API_KEY") :stream t))


;; TODO
(defun my-gptel-deepseek-remove-think-block (beg end)
  "Delete an HTML block tag named 'think' and its content."
  (unless gptel-mode
    (save-excursion
      (goto-char beg)
      ;; Find the opening <think> tag
      (when (re-search-forward "^<think>" end t)
        (let* ((start (line-beginning-position))
               (end-pos (progn (forward-line 1) (search-forward "</think>") (point))))
          ;; Delete from start of opening tag to end of closing tag
          (delete-region start end-pos))))
    (save-excursion
      (goto-char beg)
      (delete-region
       (point) (progn (skip-syntax-backward " ")
                      (point))))))
(add-hook 'gptel-post-response-functions #'my-gptel-deepseek-remove-think-block)

(use-package llm
  :straight (:host github :repo "ahyatt/llm"))

(use-package magit-gptcommit
  :straight t
  :demand t
  :after magit
  :init (require 'llm-openai)
  :config (setq llm-warn-on-nonfree nil)
  :custom
  (magit-gptcommit-llm-provider
   (make-llm-openai-compatible
    :key (getenv "OPENROUTER_API_KEY")
    :url "https://openrouter.ai/api/v1/"
    :chat-model "deepseek/deepseek-chat-v3-0324:free")))

(use-package aider
  :straight (:host github :repo "tninja/aider.el")
  :config
  ;; For latest claude sonnet model
  (setq aider-args '("--model" "openrouter/deepseek/deepseek-chat-v3-0324:free" "--no-auto-accept-architect"))
  ;; (setenv "ANTHROPIC_API_KEY" anthropic-api-key) ;; requ?
  ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
  ;; ;;
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu) ;; for wider screen
  ;; or use aider-transient-menu-2cols / aider-transient-menu-1col, for narrow screen
  (aider-magit-setup-transients)) ;; add aider magit function to magit menu

(use-package minuet
  :straight (:host github :repo "milanglacier/minuet-ai.el" :type git)
  :config

  (setq minuet-provider 'openai-fim-compatible)
  (setq minuet-n-completions 1) ; recommended for Local LLM for resource saving
  ;; I recommend beginning with a small context window size and incrementally
  ;; expanding it, depending on your local computing power. A context window
  ;; of 512, serves as an good starting point to estimate your computing
  ;; power. Once you have a reliable estimate of your local computing power,
  ;; you should adjust the context window to a larger value.
  (setq minuet-context-window 512)
  (plist-put minuet-openai-fim-compatible-options :end-point "http://js-mbp14:11434/v1/completions")
  ;; an arbitrary non-null environment variable as placeholder.
  ;; For Windows users, TERM may not be present in environment variables.
  ;; Consider using APPDATA instead.
  (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
  (plist-put minuet-openai-fim-compatible-options :model "qwen2.5-coder:3b") ;; 14b


  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 56)

  ;; Deepseek / Openrouter
  (comment
    (setq minuet-provider 'openai-compatible)
    (setq minuet-request-timeout 2.5)
    (setq minuet-auto-suggestion-throttle-delay 1.5) ;; Increase to reduce costs and avoid rate limits
    (setq minuet-auto-suggestion-debounce-delay 0.6) ;; Increase to reduce costs and avoid rate limits

    (plist-put minuet-openai-compatible-options :end-point "https://openrouter.ai/api/v1/chat/completions")
    (plist-put minuet-openai-compatible-options :api-key "OPENROUTER_API_KEY")
    (plist-put minuet-openai-compatible-options :model "deepseek/deepseek-chat-v3-0324:free")


    ;; Prioritize throughput for faster completion
    (minuet-set-optional-options minuet-openai-compatible-options :provider '(:sort "throughput"))
    (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 56)
    (minuet-set-optional-options minuet-openai-compatible-options :top_p 0.9)
	)

	:bind
	(("M-i" . #'minuet-show-suggestion)
	 :map minuet-active-mode-map
	 ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
	 ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
	 ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
	 ("M-A" . #'minuet-accept-suggestion) ;; accept whole completion
	 ;; Accept the first line of completion, or N lines with a numeric-prefix:
	 ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
	 ("M-a" . #'minuet-accept-suggestion-line)
	 ("M-e" . #'minuet-dismiss-suggestion)

	 )
	)


;;;; MISC UI STUFF

(use-package git-gutter  ; https://github.com/emacsorphanage/git-gutter
  :config (progn
	    ;; (git-gutter:linum-setup) ; not for line-number-mode?
	    (setq git-gutter:ask-p nil)
	    (diminish 'git-gutter-mode)

	    (setq git-gutter:update-interval 1)

	    (set-face-background 'git-gutter:modified "#e8edcc")
	    (set-face-foreground 'git-gutter:modified "#e8edcc")
	    (set-face-background 'git-gutter:added "#cceecc")
	    (set-face-foreground 'git-gutter:added "white")
	    (set-face-background 'git-gutter:deleted "#eecccc")
	    (set-face-foreground 'git-gutter:deleted "#aa2222")
	    )
  :hook ((prog-mode markdown-mode toml-ts-mode) . git-gutter-mode)
  :bind (
	 ;;("C-x C-g" . git-gutter)
	 ("C-M-g C-M-g" . git-gutter:popup-hunk)

	 ;; Jump to next/previous hunk
	 ("C-x p" . git-gutter:previous-hunk)
	 ("C-M-g p" . git-gutter:previous-hunk)
	 ("C-x n" . git-gutter:next-hunk)
	 ("C-M-g n" . git-gutter:next-hunk)

	 ;; Stage current hunk
	 ;;("C-x v s" . git-gutter:stage-hunk)
	 ("C-M-g s" . git-gutter:stage-hunk)

	 ;;("C-x v a" . git-gutter:stage-hunk)
	 ("C-M-g a" . git-gutter:stage-hunk)

	 ;; Revert current hunk
	 ("C-x v r" . git-gutter:revert-hunk)
	 ("C-M-g r" . git-gutter:revert-hunk)

	 ;; Mark current hunk
	 ("C-x v SPC" . git-gutter:mark-hunk)
	 ("C-M-g SPC" . git-gutter:mark-hunk)
	 )
  )

;; TODO startup dir is stubbornly ~

(use-package treemacs
  ;;  :defer t
  :init
  (progn
    ;; (with-eval-after-load 'winum
    ;;   (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))

    (with-eval-after-load 'treemacs
      (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)) ;; TODO move to bind section
    )
  :custom (treemacs-no-delete-other-windows nil) ;; TODO LSP-mode breaks (delete-other-windows) https://github.com/emacs-lsp/lsp-treemacs/issues/122
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
	  treemacs-deferred-git-apply-delay        0.5
	  treemacs-directory-name-transformer      #'identity
	  treemacs-display-in-side-window          t
	  treemacs-eldoc-display                   'simple
	  treemacs-file-event-delay                2000
	  treemacs-file-extension-regex            treemacs-last-period-regex-value
	  treemacs-file-follow-delay               0.2
	  treemacs-file-name-transformer           #'identity
	  treemacs-follow-after-init               nil ;; t
	  treemacs-expand-after-init               t
	  treemacs-find-workspace-method           'find-for-file-or-pick-first
	  treemacs-git-command-pipe                ""
	  treemacs-goto-tag-strategy               'refetch-index
	  treemacs-header-scroll-indicators        '(nil . "^^^^^^")
	  treemacs-hide-dot-git-directory          t
	  treemacs-indentation                     1
	  treemacs-indentation-string              " "
	  treemacs-is-never-other-window           nil
	  treemacs-max-git-entries                 5000
	  treemacs-missing-project-action          'ask
	  treemacs-move-forward-on-expand          nil
	  treemacs-no-png-images                   nil
	  treemacs-no-delete-other-windows         t
	  treemacs-project-follow-cleanup          nil
	  treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	  treemacs-position                        'left
	  treemacs-read-string-input               'from-child-frame
	  treemacs-recenter-distance               0.1
	  treemacs-recenter-after-file-follow      nil
	  treemacs-recenter-after-tag-follow       nil
	  treemacs-recenter-after-project-jump     'always
	  treemacs-recenter-after-project-expand   'on-distance
	  treemacs-litter-directories
	  '("/node_modules" "/.venv" "/.cask" "/.clj-kondo" "/.cpcache" ".lsp")
	  treemacs-project-follow-into-home        nil
	  treemacs-show-cursor                     t
	  treemacs-show-hidden-files               t
	  treemacs-silent-filewatch                nil
	  treemacs-silent-refresh                  nil
	  treemacs-sorting                         'alphabetic-asc
	  treemacs-select-when-already-in-treemacs 'move-back
	  treemacs-space-between-root-nodes        t
	  treemacs-tag-follow-cleanup              t
	  treemacs-tag-follow-delay                1.5
	  treemacs-text-scale                      -0.5
	  treemacs-user-mode-line-format           nil
	  treemacs-user-header-line-format         nil
	  treemacs-wide-toggle-width               70
	  treemacs-width                           35
	  treemacs-width-increment                 1
	  treemacs-width-is-initially-locked       t
	  treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 16)

    ;;(treemacs-follow-mode t)  ; FIXME Error running timer ‘treemacs--follow’: (wrong-type-argument arrayp nil) [6 times]
					; treemacs-find-file-node: Wrong type argument: arrayp, nil

    (treemacs-follow-mode 0)
    ;;(treemacs-tag-follow-mode t) ; FIXME Encountered error while following tag at point: (wrong-type-argument arrayp nil)
    ;;(treemacs-tag-follow-mode nil)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 0)

    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
		 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (
   ;; ("M-0"       . treemacs-select-window)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t t"   . treemacs)
   ("C-x D"     . treemacs)
   ("C-x t d"   . treemacs-select-directory)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag)
   )
  )

(comment

 (use-package treemacs-evil
   :after (treemacs evil))

 (use-package treemacs-projectile
   :after (treemacs projectile))

 (use-package treemacs-magit
   :after (treemacs magit))

 (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
   :config (treemacs-set-scope-type 'Perspectives))

 (use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
   :after (treemacs)
   :config (treemacs-set-scope-type 'Tabs))

 (use-package lsp-treemacs
   ;;lsp-treemacs-errors-list
   :config (lsp-treemacs-sync-mode 1)
   ) ; TODO
 )

(add-hook 'org-mode-hook (lambda () (setq tab-width 8)))
