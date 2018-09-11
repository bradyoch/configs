;; visual stuff

(tool-bar-mode -1)
(scroll-bar-mode -1)

(if (eq window-system 'x)
    (set-frame-font "Inconsolata 13"))

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

;; Use visual lines

(global-visual-line-mode t)

;; set EOL to LF and remove trailing whitespace

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook '(lambda ()
                               (set-buffer-file-coding-system
                                'unix)))

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

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-light-medium t))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package magit)

(use-package org
  :bind ("C-c a" . org-agenda))

(use-package haskell-mode
  :mode "\\.hs\\'")

(use-package rust-mode
  :mode "\\.rs\\'")

(setq js-indent-level 2)

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
