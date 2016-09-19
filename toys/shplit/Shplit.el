(defun shplit-buffer ()
  "split pattern variables"
  (interactive)
  (setq shplit-point (string-to-number (substring (what-line) 5 nil)))
  (shell-command-on-region (point-min) (point-max) "shplit" t t)
  (goto-line shplit-point)
)

(defun shplit-this ()
  "split a Haskell pattern variable"
  (interactive)
  (when (> (following-char) 48) (forward-word))
  (insert "{-?-}")
  (shplit-buffer)
)

(add-hook 'haskell-mode-hook
          '(lambda ()
                  (define-key haskell-mode-map "\C-c\C-c"
                              'shplit-this)
           ))


