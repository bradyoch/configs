(require 'package)

(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))

(defun brady/package-init ()
  "Bootstrap use-package and setup package manager"
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(defun brady/config-file-string (name)
  (concat (directory-file-name user-emacs-directory) "/" name))

(defun brady/load-config ()
  "Load configuration file using babel"
  (interactive)
  (let ((config (brady/config-file-string "config.org")))
    (if (file-exists-p config)
	(org-babel-load-file config)
      (warn (concat config " not found - not loading")))))

(brady/package-init)
(brady/load-config)
