(require 'multiple-cursors)

(defgroup kakoune-mode nil
	"Lightweight kakoune editing mode"
	:group 'editing
	:tag "Kakoune"
	:prefix "kakoune-")

(defun kak/pause-mode ()
  (interactive)
  (cond
   (kak-global-mode (kak-global-mode -1))
   (kak-mode (kak-mode -1))))

(defvar-local kak/func-map 'nil)
(setq kak/func-map
      '(("w" . save-buffer)))

(defun kak/command-mode (&optional command)
  (interactive "M:")
  (let ((f (alist-get command kak/func-map 'nil 'nil 'equal)))
    (funcall f)))

(defun kak/open-line ()
  "'o' command in kakoune"
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (kak/pause-mode))

;; Thanks https://emacs.stackexchange.com/questions/22162/how-to-set-mark-in-elisp-and-have-shift-selection/22166
(defun kak/move-mark (fun &optional arg)
  (let ((oldval (or (cdr-safe transient-mark-mode) transient-mark-mode))
        (beg (and mark-active (mark-marker))))
    (unless beg
      (setq beg (point-marker)))
    (funcall fun arg)
    (unless mark-active
      (push-mark beg nil t))
    (setq transient-mark-mode (cons 'only oldval))))

(defmacro kak/move-mark-apply (fun)
  `(lambda (&optional args)
     (interactive "p")
     (kak/move-mark ',fun args)))

(defun kak/kill-line (&optional args)
  (interactive "p")
  (beginning-of-line)
  (kill-line args))

(defvar-local kak/default-keybinds 'nil)
(setq kak/default-keybinds
	`(("h" . backward-char)
    ("j" . next-line)
    ("k" . previous-line)
    ("l" . forward-char)
    ("w" . forward-word)
    ("b" . backward-word)
    ("H" . ,(kak/move-mark-apply backward-char))
    ("J" . ,(kak/move-mark-apply next-line))
    ("K" . ,(kak/move-mark-apply previous-line))
    ("L" . ,(kak/move-mark-apply forward-char))
    ("W" . ,(kak/move-mark-apply forward-word))
    ("B" . ,(kak/move-mark-apply backward-word))
    ("M-h" . beginning-of-line-text)
    ("M-l" . end-of-line)
    ("M-H" . ,(kak/move-mark-apply beginning-of-line-text))
    ("M-L" . ,(kak/move-mark-apply end-of-line))
    ("N" . mc/mark-next-like-this-word)
    ("d d" . kak/kill-line)
    ("g k" . beginning-of-buffer)
    ("g j" . end-of-buffer)
    ("G k" ,(kak/move-mark-apply beginning-of-buffer))
    ("G j" ,(kak/move-mark-apply end-of-buffer))
    ("o" . kak/open-line)
    ("u" . undo)
    ("." . repeat)
    ("i" . kak/pause-mode)
    (":" . kak/command-mode)
    ("<escape>" . keyboard-quit)))

(defvar kak/default-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    map))

(defun kak/reload-keymap ()
  (interactive)
  (dolist (e kak/default-keybinds)
    (define-key kak/default-keymap (kbd (car e)) (cdr e))))
(kak/reload-keymap)

(define-minor-mode kak-mode
	"Kakoune Emulation for Emacs"
	:lighter "ϰ"
	:keymap kak/default-keymap)

(define-globalized-minor-mode kak-global-mode kak-mode
	(lambda ()
		(unless (minibufferp)
			(kak-mode 1))))

(global-set-key (kbd "<escape>") 'kak-global-mode)

(provide 'kak-mode)
