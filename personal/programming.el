;; Programming related config


(load-file (expand-file-name "personal/phel-mode/phel.el" user-emacs-directory)) ;; jasalt/phel-mode

(load-file (expand-file-name "personal/php.el" user-emacs-directory))
(load-file (expand-file-name "personal/python.el" user-emacs-directory))
(load-file (expand-file-name "personal/rust.el" user-emacs-directory))


;; Flashing evaluated region (elisp)
;; Partially generated code warning

(defun flash-region (start end)
  "Make the text between START and END blink."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'success)
    (run-at-time 0.1 nil 'delete-overlay overlay)))

(defun beginning-of-top-level-form-p ()
  "Return t if point is at the beginning of a top-level form."
  (and (bolp)                        ; beginning of line
	   (null (nth 8 (syntax-ppss)))  ; is inside form
	   (not (looking-at-p "\\s-")))) ; not in whitespace in middle of form

(defun eval-defun-advice (orig-fun &rest args)
  "Advice to blink region after eval-defun."
  (let* ((current-prefix-arg (car args))
		 (start
		  (save-excursion
            (when  ; avoid jumping to previous sexp
				(not (beginning-of-top-level-form-p)) (beginning-of-defun))
			(point)))
		 (end (save-excursion
				(end-of-defun)
				(point)))
		 (eval-defun-result (apply orig-fun args)))
	(flash-region start end)
	eval-defun-result))

(defun elisp-eval-region-or-buffer-advice (orig-fun &rest args)
  "Advice to blink region after elisp-eval-region-or-buffer."
  (let* ((start (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (eval-result (apply orig-fun args)))
	(flash-region start end)
    eval-result))

(advice-add 'eval-defun :around #'eval-defun-advice)
(advice-add 'elisp-eval-region-or-buffer :around #'elisp-eval-region-or-buffer-advice)


(use-package paredit)


;; Also refer to extras/dev.el which sets some Bedrock framework defaults
;; which might be merged with this file content at some point.

;; Also see phel.el

;; TOC:
;; - General code editing UI stuff
;; - LSP-Mode general config
;; - Language specific configs for LSP, DAP, tree-sitter, etc.

(use-package ethan-wspace
  :blackout t
  :config
  (setq mode-require-final-newline nil)
  (global-ethan-wspace-mode 1)
  )

(setq-default tab-width 4)

;; Using lsp-mode instead of built-in eglot because it integrates more features.

(use-package company  ;; lsp-mode default completion backend
  :straight (company :type git :host github :repo "company-mode/company-mode"))

;; UI enhancement, not restricted to buffer area, shows help tooltip, not for TTY
(use-package company-box
  :straight (company-box :type git :host github :repo "sebastiencs/company-box")
  :hook (company-mode . company-box-mode))

(use-package yasnippet
  :blackout t
  :hook
  ((clojure-ts-mode . yas-minor-mode)
   (org-mode . yas-minor-mode)
   ))

(use-package logview)

;; https://github.com/prettier/prettier-emacs
(use-package prettier-js)

;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setenv "LSP_USE_PLISTS" "true")

;;https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"  ; (few alternatives - "C-l", "C-c l")

	treemacs-space-between-root-nodes nil
	company-minimum-prefix-length 3
	company-idle-delay 0.5 ;php-mode recommend?
	lsp-idle-delay 0.5 ;php-mode recommend?
	lsp-file-watch-threshold 7000  ; increased from 1000, enough for WP projects

	;; https://emacs-lsp.github.io/lsp-mode/page/performance/
	read-process-output-max (* 1024 1024)
	gc-cons-threshold (* 100 1024 1024))
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :custom
  ;; (lsp-headerline-breadcrumb-enable-diagnostics t)
  ;; (lsp-diagnostics-provider :capf)
  (lsp-headerline-breadcrumb-icons-enable t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(file symbols))
  (lsp-lens-enable nil)
  (lsp-disabled-clients '((python-mode . pyls)))
  :commands lsp)

;; Optionally
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :after lsp-mode
  :custom
  (lsp-ui-doc-show-with-cursor nil)
  :config
  (setq lsp-ui-doc-position 'bottom)
  )

;; TODO if startup is slow, defer with eg.
;; (use-package lsp-mode
;;     :hook (XXX-mode . lsp-deferred)
;;     :commands (lsp lsp-deferred))


(global-unset-key (kbd "<f10>")) ; unbind useless menu key, could be mode local

(comment
 (use-package dap-mode
  :init
  (setq dap-auto-configure-features '(locals controls tooltip sessions expressions breakpoints)) ; repl
  :config (dap-ui-mode 1)
  (setq dap-ui-buffer-configurations
		'(("*dap-ui-locals*"
		   (side . right)
		   (slot . 1)
		   ;;(window-height . 0.6)
		   (window-width . 0.25))
		  ("*dap-ui-sessions*"
		   (side . right)
		   (slot . 3)
		   ;;(window-width . 0.2)
		   (window-height . 0.1))
		  ("*dap-ui-expressions*"
		   (side . right)  ;; default right
		   (slot . 2)
		   (window-height . 0.1)
		   )
		  ("*dap-ui-breakpoints*"
		   (side . right)  ;; defaults to left
		   (slot . 4)
		   (window-height . 0.2)
		   ;;(window-width . 26)
		   )
		  ("*debug-window*"
		   (side . bottom)
		   (slot . 2)
		   (window-width . 0.2))
		  ("*dap-ui-repl*"
		   (side . bottom)
		   (slot . 1)
		   (window-height . 0.10))))

  ;; TODO does this actually make sense
  ;; :bind
  ;; (:map
  ;;  dap-mode-map
  ;;  ("<f5>" . dap-continue) ;; TODO (dap--get-sessions) (dap--cur-session-or-die)
  ;;  ("C-<f5>" . dap-debug)
  ;;  ("S-<f5>" . dap-debug-restart)
  ;;  ("<f10>" . dap-next)
  ;;  ("C-M-n" . dap-next)      ; cider
  ;;  ("C-M-c" . dap-continue)  ; cider
  ;;  ("C-c C-e" . dap-eval-thing-at-point)
  ;;  ("C-M-x" . dap-eval-region)
  ;;  )  ; todo activate also? if php, use default profile

  :custom (dap-ui-controls-screen-position 'posframe-poshandler-frame-bottom-right-corner)
  (dap-ui-locals-expand-depth t)  ; TODO not working?
  ;; (setq dap-print-io t) ; print debug info into *Messages*
  ))



(use-package iedit)
(use-package zoutline)
(use-package lispy
  :requires (iedit zoutline)
  ;; Disable conflicting keymaps
  :bind (:map lispy-mode-map
			  ;; TODO setting nil does not disable mode bindings (?)
			  ("[" . 'self-insert-command)  ; lispy-backward
			  ("]" . 'self-insert-command)  ; lispy-forward
			  ("DEL" . 'delete-backward-char)
			  ("M-d" . 'kill-word)  ; lispy-kill-word
			  ))

;;(define-key lispy-mode-map (kbd "[") 'self-insert-command)
;; (define-key lispy-mode-map (kbd "]") 'self-insert-command)

;;;; Clojure

;; TODO https://clojure-lsp.io/features/#snippets
;; https://emacs-lsp.github.io/lsp-mode/tutorials/clojure-guide/#basic-configuration

(use-package clojure-mode
  :hook ((clojure-mode . lsp)
		 (clojure-script-mode . lsp)
		 (clojurec-mode . lsp))
  :config (add-hook 'clojure-mode-hook 'lsp) ;; HACK cause phel requires removing it
  )


(use-package cider
  :config
  (defun ed-clojure/eval-first-comment-sexp ()
	(interactive)
	(save-excursion
	  (re-search-forward "^(comment")
	  (forward-sexp)
	  (cider-eval-last-sexp)))
  (cider-register-cljs-repl-type 'sci-js "(+ 1 2 3)")
  :bind (("C-c C-c" . ed-clojure/eval-first-comment-sexp))
  :init
  (setq
   lsp-enable-indentation nil ; use cider indentation instead of lsp, less strict is ok
   ;; lsp-enable-completion-at-point nil ; use cider completion instead of lsp
   lsp-lens-enable nil
   cider-use-tooltips nil
   cider-repl-display-help-banner nil
   cider-connection-message-fn #'cider-random-tip
   ;;cider-repl-pop-to-buffer-on-connect nil
   cider-repl-pop-to-buffer-on-connect 'display-only ; show but don't focus
   cider-repl-buffer-size-limit 100000
   cider-use-overlays 'errors-only
   cider-save-file-on-load t
   clojure-toplevel-inside-comment-form t ; eval inside comment form
   )
  :custom (cider-enrich-classpath t "Enable experimental enrich classpath")

  ;; :config (setq cider-enrich-classpath t) ; TODO
  ;; https://docs.cider.mx/cider/config/basic_config.html#use-enrich-classpath
  )

;; https://github.com/babashka/scittle/tree/main/doc/nrepl
(defun mm/cider-connected-hook ()
	(when (eq 'sci-js cider-cljs-repl-type)
      (setq-local cider-show-error-buffer nil)
      (cider-set-repl-type 'cljs)))

(add-hook 'cider-connected-hook #'mm/cider-connected-hook)

(use-package clay
  :straight (clay :type git
				  :host github
				  :repo "scicloj/clay.el"))

;; Jet (flexible replacement for jq)
;; Requires jet binary in path https://github.com/borkdude/jet/ and clojure-mode
(use-package jet)

;; TODO treemacs support
;; https://clojure-lsp.io/features/#project-tree
;; lsp-treemacs-call-hierarchy not working
;; https://clojure-lsp.io/features/#call-hierarchy

(use-package lua-mode)


;;;;; HTML editing
;; https://web-mode.org/
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-html/

(use-package web-mode
  :mode
  (("\\.html\\'" . web-mode)
   ("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.astro\\'" . web-mode)  ; npm install -g @astrojs/language-server
   ("\\.djhtml\\'" . web-mode))

  :custom
  (web-mode-engines-alist
	'(("django"    . "\\.djhtml\\'")
      ("twig"  . "\\.twig\\.")))

  (web-mode-enable-front-matter-block t)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 4)
  (web-mode-code-indent-offset 4)
  (web-mode-enable-auto-pairing t)  ;; NOTE: electric-pair-mode conflicts

  :config
  (require 'lsp-mode)  ;; Set LSP language IDs before LSP starts
  (add-to-list 'lsp-language-id-configuration '("\\.djhtml\\'" . "html"))
  (add-to-list 'lsp-language-id-configuration '("\\.twig\\'" . "html"))

  (require 'treemacs-icons-dired)  ;; Add treemacs icon for .djhtml files
  (treemacs-modify-theme "Default"
	:config
	(progn
      (treemacs-create-icon :file "vsc/django.png" :extensions ("djt" "django-html" "django-txt" "djhtml"))))

  (treemacs-define-custom-icon "👨‍🚀 " "astro")  ;; Add treemacs icon for .astro
  (treemacs-define-custom-icon "🌱‍ " "twig")  ;; Add treemacs icon for .astro

  (setq lsp-html-hover-documentation nil)
  (setq lsp-html-hover-references nil)

  ;; Make sure to use web-mode features for these
  (setq lsp-html-auto-closing-tags nil)
  (setq lsp-html-format-enable nil)

  :hook
  (web-mode . (lambda () (electric-pair-mode -1)))
  (web-mode . lsp-deferred))

;; Javascript

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))

(comment
 ;; Subscription on hold
 (use-package copilot  ; TODO move to cp.el (WIP)
   :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))

   ;; :hook (prog-mode . copilot-mode)  ;; manual startup for now
   :bind (("C-c M-f" . copilot-complete)
		  :map copilot-completion-map
		  ("C-g" . 'copilot-clear-overlay)
		  ("M-p" . 'copilot-previous-completion)
		  ("M-n" . 'copilot-next-completion)
		  ("<tab>" . 'copilot-accept-completion)
		  ("M-f" . 'copilot-accept-completion-by-word)
		  ("M-<return>" . 'copilot-accept-completion-by-line))
   :config
										; https://github.com/copilot-emacs/copilot.el/issues/249
   (add-to-list
	'copilot-indentation-alist
	'(emacs-lisp-mode 2)))
 )


(comment
 (use-package tabnine
  :commands (tabnine-start-process)
  ;; :hook (prog-mode . tabnine-mode)
  :straight t
  ;; :diminish "⌬"
  :custom
  (tabnine-wait 1)
  (tabnine-minimum-prefix-length 2)
  :hook (kill-emacs . tabnine-kill-process)
  :config
  (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
  (tabnine-start-process)
  :bind
  (:map  tabnine-completion-map
	 ("<tab>" . tabnine-accept-completion)
	 ("TAB" . tabnine-accept-completion)
	 ("M-f" . tabnine-accept-completion-by-word)
	 ("M-<return>" . tabnine-accept-completion-by-line)
	 ("C-g" . tabnine-clear-overlay)
	 ("M-[" . tabnine-previous-completion)
	 ("M-]" . tabnine-next-completion))))
