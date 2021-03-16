;; package management
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; keep packages updates
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; fade mode names in modeline
(use-package diminish)

;; recent files
(use-package recentf
  :init (recentf-mode 1))

;; command completion
(use-package ivy
  :diminish
  :after counsel
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line))
  :init
  (ivy-mode 1)

  :config
  (setq ivy-wrap t)
  (add-to-list 'ivy-sort-functions-alist
	       '(counsel-projectile-find-file . file-newer-than-file-p)
	       '(counsel-buffer-or-recentf . file-newer-than-file-p)))

;; ivy enhancements
(use-package ivy-rich
  :init (ivy-rich-mode 1)
  :after counsel)

;; custom menus
(use-package hydra
  :defer t)

;; search
(use-package swiper
  :diminish
  :bind (("C-s" . 'swiper)))

;; undo for vim
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; vim
(use-package evil
  :ensure t
  :after undo-tree
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)

  :preface (defun cdaddr/evil-hook ()
	     (dolist (mode '(term-mode
			     eshell-mode))
	       (add-to-list 'evil-emacs-state-modes mode)))

  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; (define-key evil-normal-state-map (kbd "/") 'swiper)
  (define-key evil-normal-state-map (kbd "M-f") 'swiper)
  (evil-set-undo-system 'undo-tree)
  (add-hook 'evil-mode-hook 'cdaddr/evil-hook))

;; more vim
(use-package evil-collection
  :after evil
  :config (evil-collection-init))

;; modeline
;;   be sure to run M-x all-the-icons-install-fonts
(use-package all-the-icons)
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; themes
(use-package doom-themes
  :init (load-theme 'doom-one-light t))

;; rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; command preview
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.1))

;; ivy-enhanced common commands
(use-package counsel
  :diminish
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history))
  :config (setq ivy-initial-inputs-alist nil))

;; ivy menu descriptions
(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1))

;; (use-package ivy-prescient
;;   :after counsel
;;   :custom
;;   (ivy-prescient-enable-filtering nil)
;;   :config
;;   (ivy-prescient-mode 1))

;; emacs help
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; org
(use-package org
  :ensure org-plus-contrib
  :pin org)

;; syntax checking
(use-package flycheck
  :defer t
  :diminish
  :init (global-flycheck-mode 1))

;; key binds - leader
(use-package general
 :config
 (general-evil-setup t)
 (general-create-definer cdaddr/leader-keys
   :keymaps '(normal insert visual emacs)
   :prefix "SPC"
   :global-prefix "C-SPC"))

 (cdaddr/leader-keys
   "t" '(:ignore t :which-key "toggles")
   "x" '(counsel-M-x :which-key "M-x command"))

;; lsp
(use-package lsp-mode
  :commands lsp
  :config
  (lsp-enable-which-key-integration 1))

(use-package lsp-ivy
  :after lsp
  :commands lsp-ivy-workspace-symbol)

;; text completion
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map ("<tab>" . company-complete-selection))
  (:map lsp-mode-map ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; projects
(use-package projectile
  :demand t
  :diminish projectile-mode
  :config (projectile-mode +1)
  :bind-keymap ("M-P" . projectile-command-map)
  :init
  (setq projectile-sort-order 'recently-active)
  (setq projectile-project-search-path '("~/Local"))
  ;; :init
  ;; (let ((cdaddr/project-path "~/Local"))
  ;;   (when (file-directory-p cdaddr/project-path)
  ;;     (setq projectile-project-search-path cdaddr/project-path)))
  ;; (setq projectile-switch-project-action #'projectile-dired)
  )

(cdaddr/leader-keys
  "pf" 'counsel-projectile-find-file
  "ps" 'counsel-projectile-switch-project
  "pg" 'counsel-projectile-rg
  "pr" 'counsel-buffer-or-recentf)

(use-package counsel-projectile
  :after projectile
  :init
  :config (counsel-projectile-mode 1))

;; color preview
(use-package rainbow-mode
  :config (rainbow-mode 1))

;; make UI tolerable
(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)

(setq ring-bell-function 'ignore)

(defun my-terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq visible-bell       t
      ring-bell-function #'my-terminal-visible-bell)

;; builtins
(setq show-trailing-whitespace t)

;; prog-mode
(defun cdaddr/prog-hook ()
  "Some prog mode hooks."
  (display-line-numbers-mode 1)
  (line-number-mode 1)
  (column-number-mode 1))
(add-hook 'prog-mode-hook 'cdaddr/prog-hook)

;; font
(set-face-attribute 'default nil :font "JetBrainsMono NF" :height 148)

;; more key bindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; XDG
(defconst cdaddr/cache-path (expand-file-name "emacs" (or (getenv "XDG_CACHE_HOME") "~/.cache")))
(make-directory cdaddr/cache-path t)

(defconst cdaddr/backup-path (expand-file-name "backup" cdaddr/cache-path))
(setq backup-by-copying-when-linked t)
(make-directory cdaddr/backup-path t)
(setq backup-directory-alist `((".*" . ,cdaddr/backup-path)))

(setq cdaddr/autosave-path (expand-file-name "save" cdaddr/cache-path))
(setq auto-save-list-file-prefix cdaddr/autosave-path)
(setq auto-save-file-name-transforms `((".*" ,cdaddr/autosave-path t)))
(make-directory cdaddr/autosave-path t)

(setq cdaddr/undo-path (expand-file-name "undo" cdaddr/cache-path))
(setq undo-tree-history-directory-alist `(("." . ,cdaddr/undo-path)))
(setq undo-tree-auto-save-history t)
(make-directory cdaddr/undo-path t)

(setq default-directory "~/")
(add-to-list 'exec-path "/usr/local/bin")	       '(counsel-projectile-switch-project . file-newer-than-file-p)


;; macos-specific
(when (string-equal system-type "darwin")
  (setq mac-command-key-is-meta t)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (global-set-key (kbd "<home>") 'beginning-of-line)
  (global-set-key (kbd "<end>") 'end-of-line)
  (add-to-list 'default-frame-alist '(top . 0))
  (add-to-list 'default-frame-alist '(left . 0))
  (add-to-list 'default-frame-alist '(height . 95))
  (add-to-list 'default-frame-alist '(width . 184)))

(defun cdaddr/elisp-hook ()
  "Emacs Lisp mode hook."
  (local-set-key (kbd "C-c C-k") 'eval-buffer))
(add-hook 'emacs-lisp-mode-hook 'cdaddr/elisp-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(counsel-projectile org-plus-contrib borg ivy-prescient no-littering auto-package-update flycheck company lsp-ivy lsp-mode rainbow-mode projectile evil-collection which-key use-package rainbow-delimiters ivy-rich helpful github-theme general evil doom-themes doom-modeline diminish counsel command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
