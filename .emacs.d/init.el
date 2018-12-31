;; Remove gui elements early to avoid flashing
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(modify-all-frames-parameters '((internal-border-width . 10)
                                (width . 80)
                                (background-color . "honeydew2")
                                (left-fringe . 0)
                                (right-fringe . 0)))

(setq-default line-spacing .15)
(cond
 ((eq window-system 'x)
  (set-frame-font (font-spec :family "Hack" :size 10) nil t))
 ((eq window-system 'w32)
  (set-frame-font (font-spec :family "Consolas" :size 12) nil t)
  (setq visible-bell t)))

;; Setup line numbers only for text files
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
      mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control . nil)))
      mouse-wheel-progressive-speed nil
      scroll-margin 2)

;; Unbind annoying mouse paste commands
(global-unset-key (kbd "<mouse-2>"))

;; Clear the minor mode display alist
(defun brady/wipe-minor-modes ()
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

;; Don't use tabs
(setq indent-tabs-mode nil)

;; Helper function to load files in the .emacs.d directory
(defun brady/config-file-string (name)
  (concat (directory-file-name user-emacs-directory) "/" name))

;; Put the custom elements in their own files
(let ((custom (brady/config-file-string "custom.el")))
  (setq custom-file custom)
  (if (file-exists-p custom)
      (load-file custom)))

;; Match parentheses
(electric-pair-mode 1)

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

(setq use-package-always-ensure t)

(use-package which-key
  :config (which-key-mode))

(use-package company
  :config (global-company-mode 1))

(use-package ivy
  :config
  (setq ivy-use-selectable-prompt t)
  (ivy-mode 1))

(use-package counsel
  :bind ("C-s" . swiper)
  :config (counsel-mode 1))

(use-package magit)

(use-package neotree
  :bind ("<f8>" . neotree-toggle))

(use-package go-mode
  :mode "\\*.go\\'")

;; Setup the emacs server for emacs clients
(require 'server)
(unless (server-running-p)
  (server-start))
