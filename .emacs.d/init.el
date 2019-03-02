;; Remove gui elements early to avoid flashing
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t)

(modify-all-frames-parameters '((internal-border-width . 10)
                                (left-fringe . 0)
                                (right-fringe . 0)))

(setq-default line-spacing .1)
(cond
 ((eq window-system 'x)
  (set-frame-font (font-spec :family "Hack" :size 10) nil t))
 ((eq window-system 'w32)
  (set-frame-font (font-spec :family "Consolas" :size 12) nil t)
  (setq visible-bell t)))

;; Helper function to load files in the .emacs.d directory
(defun brady/config-file-string (name)
  (concat (directory-file-name user-emacs-directory) "/" name))

;; Setup line numbers only for text files
(require 'display-line-numbers)
(setq display-line-numbers-width-start 3)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

;; Show column number in the status bar
(column-number-mode 1)

;; Show blank scratch on startup
(setq inhibit-startup-message 't
      initial-scratch-message "")

;; Smoother scrolling
(setq scroll-conservatively 1000
			scroll-preserve-screen-position nil
      mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control . nil)))
      mouse-wheel-progressive-speed nil
      scroll-margin 2)

;; Unbind annoying mouse paste commands
(global-unset-key (kbd "<mouse-2>"))

;; Make newline on scroll down if at end of buffer
(setq next-line-add-newlines t)

;; Clear the minor mode display alist
(defun brady/wipe-minor-modes ()
	(interactive)
  (setq minor-mode-alist nil))
(add-hook 'after-init-hook 'brady/wipe-minor-modes)

;; Don't make unneeded files
(setq auto-save-default nil
      make-backup-files nil)

;; Enforce styling on save (trim trailing spaces and ff=unix)
(defun brady/enforce-styling ()
  (delete-trailing-whitespace 0)
  (set-buffer-file-coding-system 'unix))
(add-hook 'before-save-hook 'brady/enforce-styling)
(setq require-final-newline 1)
(setq sentence-end-double-space nil)

;; Don't use tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Put the custom elements in their own files
(let ((custom (brady/config-file-string "custom.el")))
  (setq custom-file custom)
  (if (file-exists-p custom)
      (load-file custom)))

;; Match parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)
(electric-pair-mode)

;; Setup the package manager
(require 'package)
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)

(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t))

(use-package company
  :config (global-company-mode 1))

(use-package ivy
  :config
  (setq ivy-use-selectable-prompt t
				ivy-use-virtual-buffers t)
  (ivy-mode 1))

(use-package counsel
  :bind
  (("C-s" . swiper))
  :config (counsel-mode 1))

(use-package magit
	:bind ("C-x g" . magit-status))

(use-package neotree
  :bind ("<f8>" . neotree-toggle))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :init (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind
  (("M-S-<down>" . mc/mark-next-lines)
   ("M-S-<up>" . mc/mark-previous-lines)
   ("M-<mouse-1>" . mc/add-cursor-on-click))
  :config
  (multiple-cursors-mode 1))

(use-package move-text
	:config (move-text-default-bindings))

(use-package god-mode)

(use-package modalka)

(use-package go-mode
  :mode "\\.go\\'"
  :config
  (if (eq window-system 'x) (setenv "GOPATH" "/home/brady/.go")))

(use-package tuareg)

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package haskell-mode
  :mode "\\.hs\\'")

(use-package antlr-mode
  :ensure nil
  :mode "\\.g4?\\'")

(use-package elm-mode
  :mode "\\.elm\\'")

(use-package js2-mode
  :mode "\\.jsx?\\'")
(setq-default js-indent-level 2)

;; Bind some more keybindings

(global-set-key (kbd "<C-tab>") 'other-window)

;; Setup the emacs server for emacs clients
(require 'server)
(unless (server-running-p)
  (server-start))
