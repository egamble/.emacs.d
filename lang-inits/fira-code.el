(defun fira-code-mode--make-alist (strings except-strings)
  "Generate prettify-symbols alist from STRINGS."
  (let ((idx -1))
    (mapcan
     (lambda (s)
       (setq idx (1+ idx))
       (unless (member s except-strings)
         (let* ((code (+ #Xe100 idx))
                (width (string-width s))
                (prefix ())
                (suffix '(?\s (Br . Br)))
                (n 1))
           (while (< n width)
             (setq prefix (append prefix '(?\s (Br . Bl))))
             (setq n (1+ n)))
           (list
            (cons s (append prefix suffix (list (decode-char 'ucs code))))))))
     strings)))

(defconst fira-code-mode--ligatures
  '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
    "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
    "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
    "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
    ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
    "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
    "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
    "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
    ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
    "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
    "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
    "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
    "x" ":" "+" "unused" "*"))

(defvar fira-code-mode--old-prettify-symbols-alist)
(defvar fira-code-mode--old-prettify-symbols-compose-predicate)

(defun fira-code-mode--enable ()
  (prettify-symbols-mode -1)
  (setq-local fira-code-mode--old-prettify-symbols-alist
              prettify-symbols-alist)
  (setq-local prettify-symbols-alist
              (append (fira-code-mode--make-alist fira-code-mode--ligatures '("x" "unused"))
                      fira-code-mode--old-prettify-symbols-alist))
  (setq-local fira-code-mode--old-prettify-symbols-compose-predicate
              prettify-symbols-compose-predicate)
  (setq-local prettify-symbols-compose-predicate
              (lambda (start end match) t))
  (prettify-symbols-mode 1))

(defun fira-code-mode--disable ()
  (prettify-symbols-mode -1)
  (setq-local prettify-symbols-alist fira-code-mode--old-prettify-symbols-alist)
  (setq-local prettify-symbols-compose-predicate fira-code-mode--old-prettify-symbols-compose-predicate)
  (prettify-symbols-mode 1))

(define-minor-mode fira-code-mode
  "Fira Code ligatures minor mode"
  :lighter nil
  (setq-local prettify-symbols-unprettify-at-point 'right-edge)
  (if fira-code-mode
      (fira-code-mode--enable)
    (fira-code-mode--disable)))

(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")

(provide 'fira-code-mode)

(define-globalized-minor-mode global-fira-code-mode fira-code-mode
  (lambda () (fira-code-mode 1)))
