;;; package --- Summary:
;;; Commentary:

;; Define better keybindings for all things Emacs

;;; Code:

(defun end-of-line-and-newline ()
  "Goto end of line and add a newline."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun kill-line-and-save ()
  "Copy the link instead of killing it."
  (interactive)
  (save-excursion
    (let ((start (progn
                   (beginning-of-line)
                   (point)))
          (end (progn
                 (end-of-line)
                 (point))))
      (kill-ring-save start end))))

(defun win-mode-reload-keymap ()
  "Reload the keymap for win mode."
  (interactive)
  (unload-feature 'win-mode)
  (load-file (expand-file-name "~/.emacs.d/win-mode.el")))

(defvar win-move-map
  (let ((map (make-sparse-keymap)))
    ;; unbind all input keys
    (suppress-keymap map nil)

    ;; movement keys
    (define-key map (kbd "h") 'left-char)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "l") 'right-char)

    (define-key map (kbd "a") 'beginning-of-visual-line)
    (define-key map (kbd "e") 'end-of-visual-line)

    (define-key map (kbd "H") 'left-word)
    (define-key map (kbd "J") 'scroll-up-command)
    (define-key map (kbd "K") 'scroll-down-command)
    (define-key map (kbd "L") 'right-word)
    (define-key map (kbd "o") 'end-of-line-and-newline)

    (define-key map (kbd "s d") 'kill-whole-line)
    (define-key map (kbd "s c") 'kill-line-and-save)
    (define-key map (kbd "r d") 'kill-region)

    (define-key map (kbd "u") 'undo)
    (define-key map (kbd "p") 'yank)

    (define-key map (kbd "/") 'isearch-forward)

    (define-key map (kbd "q") (lambda ()
                                (interactive)
                                (win-move-minor-mode 0)))
    map)
  "Movement mode for win-mode.")

(defvar win-normal-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Better movement keys
    (define-key map (kbd "M-SPC i") (lambda ()
                                      (interactive)
                                      (win-move-minor-mode 1)))

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
    (define-key map (kbd "M-SPC b d") 'kill-this-buffer)
    (define-key map (kbd "M-SPC b b d") 'kill-buffer)

    (define-key map (kbd "M-SPC m s") 'start-kbd-macro)
    (define-key map (kbd "M-SPC m d") 'end-kbd-macro)
    (define-key map (kbd "M-SPC m e") 'call-last-kbd-macro)

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
  "Win minor mode movement keybindings"
  :lighter ""
  :keymap win-move-map)

(provide 'win-mode)

;;; win-mode.el ends here
