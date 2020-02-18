(require 'fira-code-mode)

(defconst timeless-mode--ligatures
  '((?∘ " . " (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Br) ?∘))
    (?∩ "<>")
    (?∪ "><")
    (?∧ "&&")
    (?∨ "||")
    (?¬ "!")
    (?∈ "@")
    (?∉ "!@")
    (?⊂ "<<")
    (?⊃ ">>")
    (?∞ "infinity")))

(defvar timeless-mode--old-prettify-symbols-alist)

(defun timeless-mode--enable ()
  (prettify-symbols-mode -1)
  (setq-local timeless-mode--old-prettify-symbols-alist
              prettify-symbols-alist)
  (setq-local prettify-symbols-alist
              (append (fira-code-mode--make-alist timeless-mode--ligatures) prettify-symbols-alist))
  (prettify-symbols-mode 1))

(defun timeless-mode--disable ()
  (prettify-symbols-mode -1)
  (setq-local prettify-symbols-alist timeless-mode--old-prettify-symbols-alist)
  (prettify-symbols-mode 1))

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
