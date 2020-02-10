(require 'fira-code-mode)

(defconst timeless-mode--regexs
  '((?∘ " . "      "\\( \\. \\)")
    (?∩ "<>"       "\\(<>\\)")
    (?∪ "><"       "\\(><\\)")
    (?∧ "&&"       "\\(&&\\)")
    (?∨ "||"       "\\(||\\)")
    (?¬ "!"        "\\(!\\)")
    (?∈ "@"        "\\(@\\)")
    (?∉ "!@"       "\\(!@\\)")
    (?⊂ "<<"       "\\(<<\\)")
    (?⊃ ">>"       "\\(>>\\)")
    (?∞ "infinity" "\\(infinity\\)")))

(defvar timeless-mode--old-font-lock-keywords)

(defun timeless-mode--enable ()
  (when fira-code-mode
    (fira-code-mode -1))
  (fira-code-mode 1)
  (setq-local timeless-mode--old-font-lock-keywords font-lock-keywords)
  (setq-local font-lock-keywords
              (append font-lock-keywords
                      (fira-code-mode--build-keywords timeless-mode--regexs))))

(defun timeless-mode--disable ()
  (fira-code-mode -1))

(define-minor-mode timeless-mode
  "Timeless minor mode"
  :lighter " Timeless"
  (if timeless-mode
      (timeless-mode--enable)
    (timeless-mode--disable)))

(provide 'timeless-mode)

;; Timeless files
(add-to-list 'auto-mode-alist '("\\.tl\\'" . timeless-mode))

;; Timeless S-expression files
(add-to-list 'auto-mode-alist '("\\.tls\\'" . lisp-mode))
