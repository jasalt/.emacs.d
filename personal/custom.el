;; General editor behavior related settings, partially inherited from Prelude

;; Comment function like in Clojure
;; https://robjohnson.dev/posts/elisp-cheat-sheet-for-clojure-devs/
(defmacro comment (&rest body)
  nil)

(setq use-package-always-ensure t)  ; don't require :ensure t for every package

;;TODO update https://github.com/rranelli/auto-package-update.el
;; (use-package auto-package-update
;;   :config
;;   (setq auto-package-update-delete-old-versions t)
;;   (setq auto-package-update-hide-results t)
;;   (auto-package-update-maybe))
;; TODO update straight separately also?


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
 (set-face-attribute 'default nil :font "Hack-14")  ; "Inconsolata-16"
 ;; Cmd+Space Spotlight search binding workaround
 (global-unset-key (kbd " "))
 (global-set-key (kbd " ") 'just-one-space)
 )

(progn-on
 "gnu/linux"
 (custom-set-faces
  '(default ((t (:height 105 :family "Hack")))))
 (pixel-scroll-precision-mode 0)
 (pixel-scroll-mode 0)

 ;; Save lock files to /var/tmp (fix problem with Python Werkzeug auto reload)
 (setq lock-file-name-transforms
       '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))

 )

;; (magit-add-section-hook 'magit-status-sections-hook
;;                         'magit-insert-modules
;;                         'magit-insert-unpulled-from-pushremote)

;;;;; General

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-S-s") 'isearch-forward)  ; re-mapped to consult-line in extras/base.el

(menu-bar-mode -1)
(global-set-key (kbd "<f12>") 'menu-bar-mode)

(size-indication-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode 1)  ; yanking replaces region content

(use-package blackout)  ; helps keeps modeline clean
(blackout 'eldoc-mode)
(blackout 'which-key-mode)

;; Dired customization
(setq dired-listing-switches "-lhX")  ;; TODO -X does not seem to have effect
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

(use-package key-chord
  :init (progn
	  (key-chord-define-global "jj" 'avy-goto-word-1)
	  (key-chord-define-global "jl" 'avy-goto-line)
	  (key-chord-define-global "jf" 'avy-goto-char)
	  (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
	  ;;(key-chord-define-global "uu" 'undo-tree-visualize)

	  (key-chord-mode +1)
	  )
  )

(use-package multiple-cursors
  :init (progn
	  (key-chord-define-global "jn" 'mc/mark-more-like-this-extended)
	  (key-chord-define-global "jp" 'mc/mark-previous-like-this)
	  (key-chord-define-global "jm" 'mc/mark-all-like-this)
	  (key-chord-define-global "jt" 'mc/mark-sgml-tag-pair)
	  (key-chord-define-global "kd" 'mc/edit-lines)
	  ))

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
(global-set-key (kbd "C-S-o") '(lambda () (interactive) (split-window-vertically nil)))
(global-set-key (kbd "C-M-o") '(lambda () (interactive) (split-window-horizontally nil)))
(global-set-key (kbd "C-M-w") '(lambda () (interactive) (delete-window)))


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

(use-package zop-to-char
  :bind (("M-z" . zop-up-to-char)
	 ("M-Z" . zop-to-char)
	 ))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package crux
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


;; LLM CONFIG STUFF

;; TODO include these in chatgpt declaration
(defun get-openai-api-key ()
  "Return the value of the OPENAI_API_KEY environment variable.
   It needs to be set in .profile and maybe in .zshrc"
  (getenv "OPENAI_API_KEY"))

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


(use-package openai
  :config 
  (setq openai-key #'get-openai-api-key)
	  
  :straight (openai :type git :host github :repo "emacs-openai/openai"))

;; Wrong type argument: listp error means that (getenv "OPENAI_API_KEY")
;; probably returns ""
(use-package spinner)
(use-package chatgpt
  :straight (chatgpt :type git :host github :repo "emacs-openai/chatgpt"))

(use-package gpt  ; https://github.com/stuhlmueller/gpt.el
  :config (setq gpt-openai-key (get-openai-api-key)
		gpt-openai-engine "gpt-4-turbo-preview"))


;;;; MISC UI STUFF

(use-package git-gutter  ; https://github.com/emacsorphanage/git-gutter
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

