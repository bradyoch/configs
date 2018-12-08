;; Setup MELPA and use-package.el

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defun brady/load-config ()
  (interactive)
  (org-babel-load-file
   (concat
    (directory-file-name user-emacs-directory) "/config.org")))

(brady/load-config)
