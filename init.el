;; turn off emacs startup message
(setq inhibit-startup-message t)

;; turn off menu bar
(menu-bar-mode -1)

;; turn off tool bar
(if (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; make transparent if the window manager supports it
(add-to-list 'default-frame-alist '(alpha 85 75))

(setq frame-title-format '("%f"))

;; allow emacsclient to open files in a running emacs
(server-start)

;; tab width as two, using spaces
(setq default-tab-width 2)
(setq-default indent-tabs-mode nil)
(setq sh-basic-offset 2)

;; show column numbers
(setq column-number-mode t)

;; turn off scroll bars
(if (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(put 'scroll-left 'disabled nil)

;; no audible or visible bell
(setq ring-bell-function (lambda () nil))

;; personally, I can do without all those ~ files
(setq make-backup-files nil)

;; mostly for revert-buffer I just want to type y, not yes
(defalias 'yes-or-no-p 'y-or-n-p)

;; just shut up and reload the tags file!
(setq tags-revert-without-query 1)

;; stop asking if I want to keep the current list of tags tables
(setq tags-add-tables nil)

;; undo/redo pane configuration with C-c left/right arrow
(winner-mode 1)


(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(global-set-key (kbd "M-RET") 'toggle-fullscreen)


(if (eq system-type 'darwin)
  (progn
    (global-set-key (kbd "<s-wheel-up>")       'text-scale-increase)
    (global-set-key (kbd "<s-wheel-down>")     'text-scale-decrease))

  ;; linux
  (progn
    (global-set-key (kbd "<s-mouse-4>")        'text-scale-increase)
    (global-set-key (kbd "<s-double-mouse-4>") 'text-scale-increase)
    (global-set-key (kbd "<s-triple-mouse-4>") 'text-scale-increase)
    (global-set-key (kbd "<s-mouse-5>")        'text-scale-decrease)
    (global-set-key (kbd "<s-double-mouse-5>") 'text-scale-decrease)
    (global-set-key (kbd "<s-triple-mouse-5>") 'text-scale-decrease)

    (global-set-key (kbd "s-s")                'save-buffer)
    (global-set-key (kbd "s-x")                'clipboard-kill-region)
    (global-set-key (kbd "s-c")                'clipboard-kill-ring-save)
    (global-set-key (kbd "s-v")                'clipboard-yank)
    (global-set-key (kbd "s-f")                'isearch-forward)
    (global-set-key (kbd "s-g")                'isearch-repeat-forward)
    (global-set-key (kbd "s-z")                'undo)
    (global-set-key (kbd "s-q")                'save-buffers-kill-terminal)))

(global-set-key [f7]                       'revert-buffer)
(global-set-key [f12]                      'other-window)

(global-set-key (kbd "<s-right>")          'other-window)
(global-set-key (kbd "<s-left>")           '(lambda () "backwards other-window" (interactive) (other-window -1)))

(global-set-key (kbd "C-c c")              'toggle-truncate-lines)
(global-set-key (kbd "C-c ;")              'comment-or-uncomment-region)

;; keyboard macro key bindings
(global-set-key (kbd "C-,")                'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-.")                'kmacro-end-or-call-macro)

(defun find-init-file ()
  "Visit init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "s-i")                'find-init-file)
(global-set-key (kbd "s-I")                'eval-buffer)

(global-set-key (kbd "s-{")                'shrink-window-horizontally)
(global-set-key (kbd "s-}")                'enlarge-window-horizontally)
(global-set-key (kbd "s-[")                'shrink-window)
(global-set-key (kbd "s-]")                'enlarge-window)

(global-set-key (kbd "C-s-s")              'isearch-forward-symbol-at-point)
(global-set-key (kbd "<C-s-268632083>")    'isearch-forward-symbol-at-point) ; OS X turns C-s-s into <C-s-268632083>


;; load color-theme
(add-to-list 'load-path "~/.emacs.d/color-theme")
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
;; use wombat
(load-file "~/.emacs.d/color-theme/themes/wombat.el")
(color-theme-wombat)


(let (refreshed)
  (dolist (package '(paredit
                     clojure-mode
                     cider
                     company
                     highlight-parentheses
                     fold-dwim fold-dwim-org
                     smex
                     markdown-mode
                     ace-jump-mode
                     json-mode
                     f ; projectile requires this, but doesn't always successfully load it
                     projectile
                     haskell-mode
                     slime
                     buffer-move))
    (unless (package-installed-p package)
      (when (not refreshed)
        (package-refresh-contents)
        (setq refreshed t))
      (package-install package))))


(setq inferior-lisp-program "/usr/local/bin/sbcl")
(require 'slime)
(slime-setup '(slime-fancy))


(require 'cider)
(require 'clojure-mode)

(setq cider-prompt-for-symbol nil)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; Indent (if ...) with CL convention for Lisp mode. Doesn't affect Emacs-Lisp or Clojure modes.
(add-hook 'lisp-mode-hook
          (lambda ()
            (set (make-local-variable 'lisp-indent-function)
                 'common-lisp-indent-function)))


(require 'company)
(global-company-mode)


;; Toggle fold-dwim-org mode with C-tab.
;; While fold-dwim-org mode is enabled:
;;  tab shows/hides block,
;;  S-tab shows/hides all blocks.
(require 'fold-dwim-org)
(global-set-key (kbd "<C-tab>") 'fold-dwim-org/minor-mode)

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
              (local-set-key (kbd "<C-M-s-left>")  'backward-select-sexp))))


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


(defun smart-line-beginning ()
  "Move point to the beginning of text
on the current line; if that is already
the current position of point, then move
it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

(global-set-key "\C-a" 'smart-line-beginning)


;; enable awesome file prompting
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
;     ido-use-filename-at-point t
      ido-max-prospects 10)

;; smex: ido for M-x
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; buffer-move
(setq buffer-move-stay-after-swap t)
(global-set-key (kbd "<C-s-up>")     'buf-move-up)
(global-set-key (kbd "<C-s-down>")   'buf-move-down)
(global-set-key (kbd "<C-s-left>")   'buf-move-left)
(global-set-key (kbd "<C-s-right>")  'buf-move-right)


(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
     "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(global-set-key [f11] 'toggle-window-dedicated)


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


(defun squeeze-whitespace ()
  "Squeeze white space (including new lines) between objects around point.
Leave one space or none, according to the context."
  (interactive "*")
  (skip-chars-backward " \t\r\n\f")
  (set-mark (point))
  (skip-chars-forward " \t\r\n\f")
  (delete-region (point) (mark))
  (insert ?\s)
  (fixup-whitespace))

(global-set-key (kbd "<s-backspace>") 'squeeze-whitespace)


(require 'ace-jump-mode)
;; C-c SPC and C-c C-SPC are ace-jump-word-mode
;; C-u C-c SPC and C-u C-c C-SPC and s-SPC are ace-jump-char-mode
;; C-u C-u C-c SPC and C-u C-u C-c C-SPC are ace-jump-line-mode
(global-set-key (kbd "C-c SPC")   'ace-jump-mode)
(global-set-key (kbd "C-c C-SPC") 'ace-jump-mode)
(global-set-key (kbd "s-SPC")     'ace-jump-char-mode)


(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; C-c C-f pretty prints a JSON buffer
(autoload 'json-mode "json-mode"
  "Major mode for editing JSON files" t)
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

;; It's more likely that a .mm file is Objective-C than nroff.
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

;; Timeless S-expression files
(add-to-list 'auto-mode-alist '("\\.tls\\'" . lisp-mode))


(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' 'exec-path' and PATH environment variable to match that used by the user's shell.
This is particularly useful under Mac OS X, where GUI apps are not started from a shell.
Modified from sanityinc's answer to http://stackoverflow.com/questions/8606954/path-and-exec-path-set-but-emacs-does-not-find-executable."
  (interactive)
  (let ((path-from-shell (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (mapc (lambda (p) (add-to-list 'exec-path
                              (replace-regexp-in-string "[ \t\n]*$" "" p)))
          (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)


(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(default ((t (:height 140)))))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(markdown-command "/usr/local/bin/markdown"))
