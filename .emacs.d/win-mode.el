;;; package --- Summary:
;;; Commentary:

;; Define better keybindings for all things Emacs

;;; Code:

(defun end-of-line-and-newline ()
  "Goto end of line and add a newline."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defvar win-move-mode-map
  (let ((map (make-sparse-keymap)))
    ;; movement keys
    (define-key map (kbd "h") 'left-char)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "l") 'right-char)

    (define-key map (kbd "H") 'left-word)
    (define-key map (kbd "J") 'end-of-line)
    (define-key map (kbd "K") 'beginning-of-line)
    (define-key map (kbd "L") 'right-word)
    (define-key map (kbd "o") 'end-of-line-and-newline)

    (define-key map (kbd "q") 'win-move-minor-mode)

    (define-key map (kbd "u") 'undo)
    (define-key map (kbd "p") 'yank)

    ;; search keys
    (define-key map (kbd "/") 'isearch-forward)
    map)
  "Movement mode for win-mode.")

(defvar win-normal-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Better movement keys
    (define-key map (kbd "M-SPC i") 'win-move-minor-mode)

    ;; Beginning of useful commands
    (define-key map (kbd "M-SPC w v") 'split-window-right)
    (define-key map (kbd "M-SPC w b") 'split-window-below)
    (define-key map (kbd "M-SPC w d") 'delete-window)
    (define-key map (kbd "M-SPC w x") 'delete-other-windows)
    (define-key map (kbd "M-SPC w o") 'other-window)
    (define-key map (kbd "M-SPC w h") 'windmove-left)
    (define-key map (kbd "M-SPC w j") 'windmove-down)
    (define-key map (kbd "M-SPC w k") 'windmove-up)
    (define-key map (kbd "M-SPC w l") 'windmove-right)

    (define-key map (kbd "M-SPC f o") 'find-file)
    (define-key map (kbd "M-SPC f s") 'save-buffer)

    (define-key map (kbd "M-SPC b o") 'switch-to-buffer)
    (define-key map (kbd "M-SPC b d") 'kill-buffer)

    (define-key map (kbd "M-SPC q q") 'save-buffers-kill-terminal)

    (define-key map (kbd "M-SPC M-SPC") 'execute-extended-command)
    map)
  "Normal mode for Win Minor mode.")

(define-minor-mode win-minor-mode
  "Win minor mode for better keybindings."
  :init-value t
  :lighter " Win"
  :keymap win-normal-mode-map)

(define-minor-mode win-move-minor-mode
  "Movement mode for win-minor mode."
  :init-value nil
  :lighter ""
  :keymap win-move-mode-map)

(provide 'win-mode)

;;; win-mode.el ends here
