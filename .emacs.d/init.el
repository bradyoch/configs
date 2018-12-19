;; New and unimproved configs because comments work fine

(menu-bar-mode -1)
(unless (not window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(column-number-mode 1)

(modify-all-frames-parameters (list (cons 'internal-border-width 10)
                                    (cons 'cursor-type 'bar)))

(add-hook 'after-init-hook
	        (lambda ()
	          (setq minor-mode-alist nil)))

;; Turn off autosaving and backup

(setq make-backup-files nil
      auto-save-default nil)

;; Font options

(setq-default line-spacing .15)
(cond
 ((eq window-system 'x)
  (set-frame-font (font-spec :family "Hack" :size 10) nil t))
 ((eq window-system 'w32)
  (set-frame-font (font-spec :family "Consolas" :size 10) nil t)))

;; Move custom info to its own file

(let ((custom (concat (directory-file-name user-emacs-directory)
		                  "/custom.el")))
  (setq custom-file custom)
  (if (file-exists-p custom)
      (load-file custom)))

;; Better startup buffers

(setq inhibit-startup-message t
      initial-scratch-message "")

;; Better scrolling

(setq scroll-conservatively 1000
      mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control . nil)))
      mouse-wheel-progressive-speed nil)

;; Consistent saving

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook '(lambda ()
			                         (set-buffer-file-coding-system
				                        'unix)))

;; No tabs

(setq-default indent-tabs-mode nil
	            tab-width 2)

;; Also make things auto close

(electric-pair-mode 1)

;; Better dired mode

(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")
      dired-listing-switches "-alh")

;; Setup melpa

(require 'package)
(add-to-list 'package-archives
	           '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq package-enable-at-startup nil)

;; Bootstrap use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Setup all the packages we want

(setq use-package-always-ensure t)

(use-package gruvbox-theme
  :config (load-theme 'gruvbox t))

(use-package company
  :config
  (global-company-mode 1))

(use-package yasnippet
  :init (yas-global-mode 1))

(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package which-key
  :config (which-key-mode 1))

(use-package go-mode
  :mode "\\*.go\\'")

;; Lastly, start emacs in server mode

(require 'server)
(unless (server-running-p)
  (server-start))

;; Some misc keybindings

(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-k k") 'kill-buffer)

(global-unset-key (kbd "<mouse-2>"))
