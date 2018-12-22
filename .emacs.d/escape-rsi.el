(defvar esc-rsi-map (make-sparse-keymap))
(defvar prefix-key "<escape>")

(define-prefix-command 'esc-rsi-prefix nil "esc-key")

(define-key esc-rsi-prefix (kbd "s") 'swiper)
(define-key esc-rsi-prefix (kbd "f s") 'save-buffer)
(define-key esc-rsi-prefix (kbd "f o") 'find-file)
(define-key esc-rsi-prefix (kbd "b k") 'kill-this-buffer)
(define-key esc-rsi-prefix (kbd "b b k") 'kill-buffer)
(define-key esc-rsi-prefix (kbd "a") 'execute-extended-command)
(define-key esc-rsi-prefix (kbd "g") 'keyboard-escape-quit)
(define-key esc-rsi-prefix (kbd "x o") 'other-window)
(define-key esc-rsi-prefix (kbd "x 0") 'delete-window)
(define-key esc-rsi-prefix (kbd "x 1") 'delete-other-windows)

(define-key esc-rsi-map (kbd prefix-key) 'esc-rsi-prefix)

(define-minor-mode esc-rsi
  "Toggle escape rsi mode.
Implements a leader key in emacs. By default uses the <escape> key.
Use (setq prefix-key ...) to redefine the key before calling esc-rsi-mode"
  nil " Esc-RSI")

(define-global-minor-mode global-esc-rsi-mode esc-rsi esc-rsi)
