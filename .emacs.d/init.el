;; remove unneeded gui elements

(menu-bar-mode -1)
(if window-system
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))

;; if linux set font

(if (eq window-system 'x)
    (set-frame-font "xos4 Terminus 12"))

;; show the column number as well as row

(column-number-mode 1)

;; start the server if it doesn't exist already

(require 'server)
(unless (server-running-p)
    (server-start))

;; get full path string for relative names

(defun get-fullpath (@file-relative-name)
  (concat (file-name-directory
           (or load-file-name buffer-file-name))
          @file-relative-name))

;; put custom.el settings in own file

(setq custom-file (get-fullpath "customization.el"))

;; don't make backup files

(setq make-backup-files nil)
(setq auto-save-default nil)

;; turn off startup messages

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; don't use tabs at all

(setq-default indent-tabs-mode nil)

;; smooth scrolling

(setq scroll-conservatively 1000)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control . nil))))
(setq mouse-wheel-progressive-speed nil)

;; set EOL to LF and remove trailing whitespace

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook '(lambda ()
                               (set-buffer-file-coding-system
                                'unix)))

;; Use better dired mode

(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(setq dired-listing-switches "-alh")

;; Package info and settings

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(package-initialize)

;; Use Package declarations
;; Check if use-package is installed

(if (not (require 'use-package nil 't))
    (progn
      (package-install 'use-package)
      (require 'use-package)))

(setq use-package-always-ensure t)

;; packages to load

(use-package ido
  :init
  (setq ido-everywhere t)
  :config
  (ido-mode t))

(use-package rich-minority
  :init (rich-minority-mode 1)
  :config (setq rm-blacklist ""))

(use-package gruvbox-theme
  :if window-system
  :config (load-theme 'gruvbox t))

(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq-default
   flycheck-disabled-checkers
   '(emacs-lisp-checkdoc)))

(use-package magit)

;; evil mode for when I want vim
;; (use-package evil
;;   :config (evil-mode 1))

(use-package god-mode
  :bind (([escape] . god-local-mode)
         :map god-local-mode-map
         ("." . repeat)
         ("i" . god-local-mode)))

(use-package multiple-cursors
  :bind (("C-c s a" . mc/mark-all-like-this)
         ("C-c s n" . mc/mark-next-like-this)
         ("M-n" . mc/mark-next-lines)
         ("M-p" . mc/mark-previous-lines)))

(use-package org
  :bind ("C-c a" . org-agenda)
  :init
  (add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
  (add-hook 'org-mode-hook 'turn-on-auto-fill))

(use-package haskell-mode
  :mode "\\.hs\\'")

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package lua-mode
  :mode "\\.lua\\'")

(setq js-indent-level 2)

;; Custom keybindings

(global-set-key (kbd "C-x C-k k") 'kill-buffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Local changes to mess around with

(let ((local (get-fullpath "local.el")))
  (unless (file-exists-p local)
    (with-temp-buffer
      (insert ";; This file is for local changes")
      (write-file local)))
  (load local))
