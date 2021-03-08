;; package loading
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages")
			 ("org" . "https://orgmode.org/elpa")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; use-package
(require 'use-package)
(setq use-package-always-ensure t)

;; diminish (fade mode names in modeline)
(use-package diminish)

;; ivy (find file, switch buffer)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line))
  :config (ivy-mode 1))


;; swiper (search)
(use-package swiper
  :diminish)

;; evil mode (vim)
(use-package undo-tree
  :config
  (global-undo-tree-mode))

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
  ;;:hook (evil-mode . cdaddr/evil-hook)

  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (evil-set-undo-system 'undo-tree)
  
  (add-hook 'evil-mode-hook 'cdaddr/evil-hook))

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

;; which-key (show commands for prefix)
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.1))

(use-package counsel
  :diminish
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history))
  :config (setq ivy-initial-inputs-alist nil))

;; show descriptions for ivy menus
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; helpful (emacs help)
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; general (key bindings)
;; FIXME
(use-package general
 :config
 (general-create-definer cdaddr/leader-keys
   :keymaps '(normal insert visual emacs)
   :prefix "SPC"
   :global-prefix "C-SPC")

 (cdaddr/leader-keys
   "t" '(:ignore t :which-key "toggles")
   "x" '(counsel-M-x :which-key "M-x command")))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap ("M-P" . projectile-command-map)
  :init
  (let ((cdaddr/project-path "~/Local"))
    (when (file-directory-p cdaddr/project-path)
      (setq projectile-project-search-path cdaddr/project-path)))
  (setq projectile-switch-project-action #'projectile-dired))

;; make UI tolerable
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq ring-bell-function 'ignore)

 (defun my-terminal-visible-bell ()
   "A friendlier visual bell effect."
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line))
 
 (setq visible-bell       nil
       ring-bell-function #'my-terminal-visible-bell)

;; line numbers and column numbers
(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; font
(set-face-attribute 'default nil :font "Fira Code" :height 148)

;; key bindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


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

;; macos-specific
(when (string-equal system-type "darwin")
  (setq mac-command-key-is-meta t)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (global-set-key (kbd "<home>") 'beginning-of-line)
  (global-set-key (kbd "<end>") 'end-of-line)
  (add-to-list 'default-frame-alist '(top . 0))
  (add-to-list 'default-frame-alist '(left . 0))
  (add-to-list 'default-frame-alist '(height . 70))
  (add-to-list 'default-frame-alist '(width . 164))
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(projectile evil-collection which-key use-package rainbow-delimiters ivy-rich helpful github-theme general evil doom-themes doom-modeline diminish counsel command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
