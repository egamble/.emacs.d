(defconst timeless-mode--ligatures
  '((" . " . ?∘)
    ("<>" . ?∩)
    ("><" . ?∪)
    ("&&" . ?∧)
    ("||" . ?∨)
    ("!" . ?¬)
    ("@" . ?∈)
    ("!@". ?∉)
    ("<<" . ?⊂)
    (">>" . ?⊃)
    ("infinity" . ?∞)))

(defun timeless-mode--enable ()
  (fira-code-mode 1)
  (setq-local prettify-symbols-alist (append timeless-mode--ligatures prettify-symbols-alist))
  (prettify-symbols-mode -1)
  (prettify-symbols-mode 1))

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
