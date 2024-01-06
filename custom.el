;; Platform specific stuff

(defmacro progn-on (platform &rest body)
  "Evaluate given expressions if running on given platform."
  (if (string-equal system-type platform)
      (cons 'progn body)))

(progn-on
 "darwin"
 (setq ns-use-native-fullscreen nil)
 (setq mac-option-modifier 'nil
       mac-command-modifier 'meta
       mac-function-modifier 'hyper)
 (set-variable 'magit-emacsclient-executable
               "/usr/local/bin/emacsclient")
 ;;(set-face-attribute 'default nil :font "Inconsolata-16")
 ;; Mac spotlight search workaround
 (global-unset-key (kbd " "))
 (global-set-key (kbd " ") 'just-one-space)
 )

(progn-on
 "gnu/linux"
 (custom-set-faces
  '(default ((t (:height 105 :family "Hack")))))
 )

;;;;; General

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-S-s") 'isearch-forward)  ; re-mapped to consult-line in extras/base.el

(menu-bar-mode -1)
(global-set-key (kbd "<f12>") 'menu-bar-mode)

(size-indication-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode 1)  ; yanking replaces region content

(use-package diminish :ensure t)
(diminish 'which-key-mode)
(diminish 'eldoc-mode)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

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
      savehist-file (expand-file-name "savehist" "~/.emacs.d/"))
(savehist-mode +1)

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" "~/.emacs.d/")
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)
(defun prelude-recentf-exclude-p (file)
  "A predicate to decide whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file))))
    (cl-some (lambda (dir)
               (string-prefix-p dir file-dir))
             (mapcar 'file-truename (list "~/.emacs.d/" package-user-dir)))))
(recentf-mode +1)

(use-package super-save :ensure t
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
(setq server-socket-dir "~/.emacs.d/server")
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

(use-package key-chord
  :ensure t
  :init (progn
	  (key-chord-define-global "jj" 'avy-goto-word-1)
	  (key-chord-define-global "jl" 'avy-goto-line)
	  (key-chord-define-global "jf" 'avy-goto-char)
	  (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
	  (key-chord-define-global "uu" 'undo-tree-visualize)

	  (key-chord-mode +1)
	  )
  )

(use-package multiple-cursors :ensure t
  :init (progn
	  (key-chord-define-global "jn" 'mc/mark-more-like-this-extended)
	  (key-chord-define-global "jp" 'mc/mark-previous-like-this)
	  (key-chord-define-global "jm" 'mc/mark-all-like-this)
	  (key-chord-define-global "jt" 'mc/mark-sgml-tag-pair)
	  (key-chord-define-global "kd" 'mc/edit-lines)
	  ))

(use-package undo-tree :ensure t
  :init (progn
	  (setq undo-tree-history-directory-alist
		`((".*" . ,temporary-file-directory)))
	  (global-undo-tree-mode))
  :config (diminish 'undo-tree-mode)
  :bind ("C-x u" . undo-tree-visualize)
  )

(winner-mode +1)

(use-package smartrep :ensure t)
(use-package operate-on-number :ensure t)
  
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

(use-package editorconfig :ensure t
  :init (progn
	  (editorconfig-mode 1)
	  (diminish 'editorconfig-mode)
	  )
  )

;; Window splitting keys, same as Terminator / Konsole / iTerm
(global-set-key (kbd "C-S-o") '(lambda () (interactive) (split-window-vertically nil)))
(global-set-key (kbd "C-M-o") '(lambda () (interactive) (split-window-horizontally nil)))
(global-set-key (kbd "C-M-w") '(lambda () (interactive) (delete-window)))


;; TODO has problems with treemacs mode with
;; (treemacs--select-visible-window) failing
(global-set-key (kbd "M-0") '(lambda () (interactive) (treemacs-select-window)))

(use-package winum :ensure t :config (winum-mode)
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


(use-package move-text :ensure t
  :bind (
	 ("C-S-<up>" . move-text-up)
	 ("C-S-<down>" . move-text-down))
  )

(use-package zop-to-char :ensure t
  :bind (("M-z" . zop-up-to-char)
	 ("M-Z" . zop-to-char)
	 ))

(use-package expand-region :ensure t
  :bind ("C-=" . er/expand-region))

(use-package crux :ensure t
  ;; Prelude niceties from https://github.com/bbatsov/crux
  :config (progn
	    (global-set-key [remap kill-whole-line] 'crux-kill-whole-line)
	    (crux-with-region-or-line kill-region)  ; C-w kills row, not random region
	    )
  :bind (
	 ("C-a" . crux-move-beginning-of-line)
	 ("C-k" . crux-smart-kill-line)

	 ("C-M-<return>" . crux-smart-open-line-above)
	 ("C-S-<return>" . crux-smart-open-line-above)
	 ("C-<return>" . crux-smart-open-line)
	 ("M-<return>" . crux-smart-open-line)

	 ("M-o" . crux-smart-open-line)
	 ; ("M-S-o" . crux-smart-open-line-above) ; vim style idea, overlaps window split binds

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
(setq save-place-file (expand-file-name "saveplace" "~/.emacs.d/"))
;; activate it for all buffers
(save-place-mode 1)

;; If having problems with bedrock default method, install tree-sitter language grammars following:

;; https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter

;; Use build script
;; git clone https://github.com/casouri/tree-sitter-module.git
;; Move them to directory ~/.emacs.d/tree-sitter/




;; This function creates that assuming virtualenv is at .venv path.
;; Source: https://robbmann.io/posts/emacs-eglot-pyrightconfig/

(use-package tramp :ensure t)  ; depends on
;; give /path/to/.venv



;; OPENAI LLM CONFIG STUFF

;; TODO include these in chatgpt declaration
(defun get-openai-api-key ()
  "Return the value of the OPENAI_API_KEY environment variable."
  (getenv "OPENAI_API_KEY"))
(use-package spinner :ensure t)
(use-package openai
  :config (progn
	    (setq openai-key #'get-openai-api-key)
	    )
  :straight (openai :type git :host github :repo "emacs-openai/openai"))
(use-package chatgpt
  :straight (chatgpt :type git :host github :repo "emacs-openai/chatgpt"))


;; https://github.com/zerolfx/copilot.el
;; Mapping examples https://github.com/zerolfx/copilot.el/issues/103
;; TODO has server connection issues
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
;;  :hook (prog-mode . copilot-mode)  ;; TODO hook to specific languages elisp-mode etc
  :bind (("C-c M-f" . copilot-complete)
	 :map copilot-completion-map
	 ("C-g" . 'copilot-clear-overlay)
	 ("M-p" . 'copilot-previous-completion)
	 ("M-n" . 'copilot-next-completion)
	 ("<tab>" . 'copilot-accept-completion)
	 ("M-f" . 'copilot-accept-completion-by-word)
	 ("M-<return>" . 'copilot-accept-completion-by-line)))


;;;; MISC UI STUFF

(use-package git-gutter :ensure t  ; https://github.com/emacsorphanage/git-gutter
  :config (progn
	    ;; (git-gutter:linum-setup) ; not for line-number-mode?
	    (setq git-gutter:ask-p nil)
	    (diminish 'git-gutter-mode)
	    
	    (setq git-gutter:update-interval 3)

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
(use-package treemacs
  :ensure t
;;  :defer t
  :init
  (progn
    ;; (with-eval-after-load 'winum
    ;;   (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
    
    (with-eval-after-load 'treemacs
      (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)) ;; TODO move to bind section
    )
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
	  treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
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
	  treemacs-text-scale                      -1
	  treemacs-user-mode-line-format           nil
	  treemacs-user-header-line-format         nil
	  treemacs-wide-toggle-width               70
	  treemacs-width                           26
	  treemacs-width-increment                 1
	  treemacs-width-is-initially-locked       t
	  treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 16)

    ;(treemacs-follow-mode t)  ; gives timer error
    (treemacs-follow-mode 0)
    ;(treemacs-tag-follow-mode t)
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
	))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))
