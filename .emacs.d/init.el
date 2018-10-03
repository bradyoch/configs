;; visual stuff

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(if (eq window-system 'x)
    (set-frame-font "Inconsolata 12"))

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

;; Use visual lines

(global-visual-line-mode t)

;; set EOL to LF and remove trailing whitespace

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook '(lambda ()
                               (set-buffer-file-coding-system
                                'unix)))

;; Use better dired mode
(require 'dired-x)

;; Package info and settings

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(package-initialize)

;; Use Package declarations

(require 'use-package)
(setq use-package-always-ensure t)

(use-package ido
  :init
  (setq ido-everywhere t)
  :config
  (ido-mode t))

(use-package rich-minority
  :init (rich-minority-mode 1)
  :config (setq rm-blacklist ""))

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t))

(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq-default
   flycheck-disabled-checkers
   '(emacs-lisp-checkdoc)))

(use-package magit)

(use-package org
  :bind ("C-c a" . org-agenda))

(use-package haskell-mode
  :mode "\\.hs\\'")

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package crystal-mode
  :mode "\\.cr\\'")

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package zig-mode
  :mode "\\.zig\\'")

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
