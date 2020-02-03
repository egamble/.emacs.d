(require 'cider)
(require 'clojure-mode)

(setq cider-prompt-for-symbol nil)


;; supports fold-dwim-org
;; add separately from other lispish mode hooks because it messes up the nrepl buffer
(add-hook 'clojure-mode-hook 'hs-minor-mode)


;; better indent for let? and condf
(define-clojure-indent
  (let? 1)
  (condf 1))


(require 'paredit)

(defun forward-select-sexp ()
  "Select sexp after point."
  (interactive)
  ;; skip comments
  (paredit-forward)
  (paredit-backward)
  (set-mark (point))
  (paredit-forward))

(defun backward-select-sexp ()
  "Select sexp before point."
  (interactive)
  ;; skip comments
  (paredit-backward)
  (paredit-forward)
  (set-mark (point))
  (paredit-backward))

;; In addition to the following commands, remember these useful paredit commands:
;; paredit-forward-slurp-sexp <C-right>, C-)
;; paredit-forward-barf-sexp  <C-left>, C-}
;; paredit-raise-sexp         M-r
;; paredit-wrap-round         M-(
;; paredit-join-sexps         M-J

;; ...and this:
;; transpose-sexps            C-M-t

;; ...and this useful clojure-mode command:
;; clojure-toggle-keyword-string C-:

(dolist (mode '(clojure cider cider-repl emacs-lisp lisp scheme lisp-interaction))
  (add-hook (first (read-from-string (concat (symbol-name mode) "-mode-hook")))
            (lambda ()
              (highlight-parentheses-mode 1)
              (paredit-mode 1)
              (local-set-key (kbd "<M-s-right>")   'forward-select-sexp)
              (local-set-key (kbd "<C-M-s-right>") 'forward-select-sexp)
              (local-set-key (kbd "<M-s-left>")    'backward-select-sexp)
              (local-set-key (kbd "<C-M-s-left>")  'backward-select-sexp)
              (when auto-fira-code-mode
                (fira-code-mode 1)) ; Fira Code Symbol ligatures
)))

;; rainbow parentheses
(require 'highlight-parentheses)
(add-hook 'clojure-mode-hook '(lambda () (highlight-parentheses-mode 1)))
(setq hl-paren-colors
      '("orange1" "yellow1" "greenyellow" "green1"
        "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))


(defmacro defclojureface (name color desc &optional others)
  `(defface ,name '((((class color)) (:foreground ,color ,@others))) ,desc :group 'faces))

; Dim parens - http://briancarper.net/blog/emacs-clojure-colors
(defclojureface clojure-parens       "DimGrey"   "Clojure parens")
(defclojureface clojure-braces       "#49b2c7"   "Clojure braces")
(defclojureface clojure-brackets     "SteelBlue" "Clojure brackets")
(defclojureface clojure-keyword      "khaki"     "Clojure keywords")
(defclojureface clojure-special      "#b8bb00"   "Clojure special")

(defun tweak-clojure-coloring ()
  (mapcar (lambda (x) (font-lock-add-keywords nil x))
          '((("#?['`]*(\\|)"       . 'clojure-parens))
            (("#?\\^?{\\|}"        . 'clojure-brackets))
            (("\\[\\|\\]"          . 'clojure-braces))
            ((":\\w+"              . 'clojure-keyword))
            (("nil\\|true\\|false\\|%[1-9]?" . 'clojure-special)))))

(add-hook 'clojure-mode-hook 'tweak-clojure-coloring)


;; display pretty lambdas
(font-lock-add-keywords 'emacs-lisp-mode
    '(("(\\(lambda\\)\\>" (0 (prog1 ()
                               (compose-region (match-beginning 1)
                                               (match-end 1)
                                               ?λ))))))

;; display clojure 'fn' as a pretty lambda
(defun clj-pretty-fn ()
  (font-lock-add-keywords nil `(("(\\(\\<fn\\>\\)"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1)
                                                           ?λ
                                                           'decompose-region)))))))
(add-hook 'clojure-mode-hook 'clj-pretty-fn)
(add-hook 'cider-repl-mode-hook 'clj-pretty-fn)


(defvar repl-ns nil)

(defun cider-save-load-switch-to-repl-set-ns ()
  "Save the buffer, load the Clojure code, switch to the REPL, set the namespace."
  (interactive)
  (save-buffer)
  (cider-load-buffer)
  (let* ((ns (cider-current-ns)))
    (cider-switch-to-repl-buffer)
    (when (not (equal repl-ns ns))
      (setq repl-ns ns)
      (cider-repl-set-ns ns))))

(defun cider-save-eval-last-sexp ()
  "Save the buffer, eval the sexp before point."
  (interactive)
  (save-buffer)
  (cider-eval-last-sexp))

(defun save-insert-last-sexp-in-repl ()
  "Save the buffer, insert the sexp before point into the REPL buffer and switch to it."
  (interactive)
  (save-buffer)
  (let ((s (cider-last-sexp)))
    (cider-switch-to-repl-buffer)
    (insert s)))


(defun cider-test-save-load-run-tests ()
  "Save the buffer, load the code, run all tests."
  (interactive)
  (save-buffer)
  (cider-load-buffer)
  (cider-test-run-tests nil))

(defun cider-test-save-load-rerun-tests ()
  "Save the buffer, load the code, rerun failed tests."
  (interactive)
  (save-buffer)
  (cider-load-buffer)
  (cider-test-rerun-tests))

(defun cider-test-save-load-run-test ()
  "Save the buffer, load the code, run the test at point."
  (interactive)
  (save-buffer)
  (cider-load-buffer)
  (cider-test-run-test))


(defvar main-clj-buffer nil
  "Main buffer for Clojure source.")

(defun start-cider ()
  (interactive)

  (if (let ((bn (buffer-name (current-buffer))))
        (and (string-match ".clj" bn)
             (not (string-match "project.clj" bn))))
      (progn
        (setq main-clj-buffer (current-buffer))

        (delete-other-windows)

        ;; Unlock the Clojure window.
        (set-window-dedicated-p (get-buffer-window main-clj-buffer) nil)
        
        (dolist (connection (cider-connections))
          (when connection
            (cider--close-connection connection)))
        (cider-close-ancillary-buffers)
        (setq repl-ns nil)
        (cider-jack-in))

    (message "Buffer %s is not a Clojure source file" (current-buffer))))


;; main Clojure window is on left and the REPL on the right if true
(setq main-clj-window-on-left nil)

;; 1. Put the Clojure window on one side, and three smaller windows on the other.
;; 2. Put the REPL in the lowest of the three small windows and lock it.
;; 2. Put the nrepl-server buffer in the window above the REPL and lock it.
;; 3. Load the Clojure buffer and set the namespace in the REPL.
;; 4. Go back to the Clojure window.
(defun after-start-cider ()
  (interactive)

  (let ((repl-buffer (current-buffer)))
    (delete-other-windows)

    (let ((w (get-buffer-window repl-buffer)))
      ;; Unlock the REPL window.
      (set-window-dedicated-p w nil)
      ;; Put another buffer in it prior to splitting.
      (set-window-buffer w "*Messages*"))

    (split-window-horizontally)
    (if main-clj-window-on-left
        (other-window 1))

    (split-window-vertically)
    (other-window 1)
    (split-window-vertically)

    ;; Put the server buffer in the middle window, just above the REPL.
    (switch-to-buffer
     (car (remove-if-not (lambda (buffer)
                           (string-match "nrepl-server" (buffer-name buffer)))
                         (buffer-list))))

    ;; Lock the server window to the server buffer.
    (set-window-dedicated-p (get-buffer-window (current-buffer)) t)

    ;; Shrink the server window to window-min-height.
    (shrink-window 100)

    (other-window 1)
    (switch-to-buffer repl-buffer)

    ;; Lock the REPL window to the REPL buffer.
    (set-window-dedicated-p (get-buffer-window repl-buffer) t)

    (other-window 1)
    (switch-to-buffer main-clj-buffer)
    
    ;; Load the Clojure code (which goes back to the REPL window and sets the NS).
    (cider-save-load-switch-to-repl-set-ns)

    ;; (use 'clojure.repl) to enable doc, source, apropos, etc.
    (cider-nrepl-sync-request:eval "(clojure.core/use 'clojure.repl)")

    ;; Go back to the Clojure window.
    (other-window 1)))


(defun start-cider-or-after-start ()
  "If the current buffer is not a cider REPL, run start-cider.
Otherwise run after-start-cider, which sets up the server window, loads the code from the
starting buffer, sets the namespace in the REPL, and returns to the starting buffer."
  (interactive)
  (if (string-match "cider-repl" (buffer-name (current-buffer)))
      (after-start-cider)
    (start-cider)))

(global-set-key (kbd "s-=") 'start-cider-or-after-start)


(defun create-clj-tags (&optional arg)
 "Create tags for def* and namespaces for all *.clj files starting
at the level of project.clj for whatever the current source file is.
Writes to the TAGS file at ~/.emacs.d/TAGS.
If the argument is 1 (the default), appends to the TAGS file, otherwise overwrites."
 (interactive "p")
 (let ((append-option (if (eq 1 arg) "-a" ""))
       (tags-table "~/.emacs.d/TAGS"))
   (shell-command
    (format "find %s \! -name '.*' -name '*.clj' | xargs etags %s -o %s --regex='/[ \t\\(]*def[a-z]* \\([a-z->!?]+\\)/\\1/' --regex='/[ \t\\(]*ns \\([a-z.]+\\)/\\1/'"
            (directory-file-name ; remove final slash
             (nrepl-project-directory-for (nrepl-current-dir)))
            append-option
            tags-table))
   (visit-tags-table tags-table)
   (message (if (eq 1 arg)
                "Appended to %s"
                "Wrote new %s")
            tags-table)))

;; cider rebinds M-., so make an alternative key binding for find-tag
(global-set-key (kbd "M-s-.") 'find-tag)
(global-set-key (kbd "M-s-≥") 'find-tag) ; OS X turns M-s-. into M-s-≥


;; load function pprint-def-in-place
(add-to-list 'load-path "~/.emacs.d/pprint")
(require 'pprint)


;; Also remember:
;; C-c C-d ? shows all doc key bindings
;; C-c C-z switches back and forth between the REPL and the last Clojure buffer
;; [f10] and C-c C-k are like C-c C-z, but also save, compile and set ns
;; [f9], M-s-down and C-c C-e evaluate expression preceding point
;; C-c C-p cider-pprint-eval-last-sexp
;; C-c C-r evaluates region
;; C-C C-c evaluates def at point
;; C-up, C-down and s-up, s-down go backward and forward in REPL history
;; C-c , save, compile, run all tests
;; C-c C-, save, compile, rerun failed tests
;; C-c M-, save, compile, run one test

(defun cider-custom-keys ()
  (define-key cider-mode-map      (kbd "C-c C-k")      'cider-save-load-switch-to-repl-set-ns)
  (define-key cider-mode-map      [f10]                'cider-save-load-switch-to-repl-set-ns)
  (define-key cider-mode-map      (kbd "<M-s-down>")   'cider-save-eval-last-sexp)
  (define-key cider-mode-map      (kbd "<C-M-s-down>") 'cider-save-eval-last-sexp)
  (define-key cider-mode-map      [f9]                 'cider-save-eval-last-sexp)
  (define-key cider-mode-map      (kbd "<s-f9>")       'save-insert-last-sexp-in-repl)
  (define-key cider-mode-map      [f8]                 'create-clj-tags)
  (define-key cider-mode-map      (kbd "C-M-q")        'pprint-def-in-place)
  (define-key cider-mode-map      (kbd "C-c ,")        'cider-test-save-load-run-tests)
  (define-key cider-mode-map      (kbd "C-c C-,")      'cider-test-save-load-rerun-tests)
  (define-key cider-mode-map      (kbd "C-c M-,")      'cider-test-save-load-run-test)

  (define-key cider-repl-mode-map (kbd "<s-up>")       'cider-repl-backward-input)
  (define-key cider-repl-mode-map (kbd "<s-down>")     'cider-repl-forward-input)
  (define-key cider-repl-mode-map (kbd "C-c p")        'cider-repl-toggle-pretty-printing)
  (define-key cider-repl-mode-map [f10]                'cider-switch-to-last-clojure-buffer))

(add-hook 'cider-mode-hook 'cider-custom-keys)


(defun clojure-enable-cider ()
  "Turn on CIDER mode (see command `cider-mode'). Useful in hooks.
Modified from the definition in cider-interaction.el to not work for project.clj."
  (when (not (string-match "project.clj" (buffer-name (current-buffer))))
    (cider-mode 1)
    (setq next-error-function 'cider-jump-to-compilation-error)))


(setq cider-repl-history-file "~/.emacs.d/cider-hist.dat")

(setq cider-repl-display-help-banner nil)


(setq inferior-lisp-program "/usr/local/bin/sbcl")
(require 'slime)
(slime-setup '(slime-fancy))


;; Indent (if ...) with CL convention for Lisp mode. Doesn't affect Emacs-Lisp or Clojure modes.
(add-hook 'lisp-mode-hook
          (lambda ()
            (set (make-local-variable 'lisp-indent-function)
                 'common-lisp-indent-function)))
