;; allow emacsclient to open files in a running emacs
(server-start)

;; turn off emacs startup message
(setq inhibit-startup-message t)

(setq frame-title-format '("%f"))

;; make transparent if the window manager supports it
(add-to-list 'default-frame-alist '(alpha 85 75))

;; do not wrap lines
(setq-default truncate-lines t)

;; tab width as two, using spaces
(setq default-tab-width 2)
(setq-default indent-tabs-mode nil)

;; show column numbers
(setq column-number-mode t)

;; turn off scroll-bars
(scroll-bar-mode -1)

(put 'scroll-left 'disabled nil)

;; personally, I can do without all those ~ files
(setq make-backup-files nil)

;; surprisingly, this dramatically speeds up Clojure compilation via slime
(setq font-lock-verbose nil)

(add-hook 'before-save-hook 'whitespace-cleanup)

;; probably OS X specific
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)

(global-set-key [f5] 'revert-buffer)
(global-set-key [f12] 'other-window)

(global-set-key (kbd "<s-right>") 'other-window)
(global-set-key (kbd "<s-left>") '(lambda () "backwards other-window" (interactive) (other-window -1)))

(global-set-key (kbd "C-c c") 'toggle-truncate-lines)
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

;; keyboard macro key bindings
(global-set-key (kbd "C-,")        'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-.")        'kmacro-end-or-call-macro)
(global-set-key (kbd "<C-return>") 'apply-macro-to-region-lines)

(global-set-key (kbd "<s-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<s-wheel-down>") 'text-scale-decrease)

(defun find-init-file ()
  "Visit init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "s-i") 'find-init-file)
(global-set-key (kbd "s-I") 'eval-buffer)

(global-set-key (kbd "s-{") 'shrink-window-horizontally)
(global-set-key (kbd "s-}") 'enlarge-window-horizontally)
(global-set-key (kbd "s-[") 'shrink-window)
(global-set-key (kbd "s-]") 'enlarge-window)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :family "Monaco")))))

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; add all subdirs of ~/.emacs.d to your load-path
(dolist (f (file-expand-wildcards "~/.emacs.d/*"))
  (add-to-list 'load-path f))

;; load color-theme
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
;; use wombat
(load-file "~/.emacs.d/color-theme/themes/wombat.el")
(color-theme-wombat)

;; load slime
(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))
          (setq slime-protocol-version 'ignore)))

(require 'slime)
(require 'slime-repl)

;; printing strings in slime with unusual characters crashes without this
(setq slime-net-coding-system 'utf-8-unix)

;; load clojure mode
(require 'clojure-mode)

;; indent let? the same as let
(define-clojure-indent
  (let? 1))

;; load clojure test mode
(autoload 'clojure-test-mode "clojure-test-mode" "Clojure test mode" t)
(autoload 'clojure-test-maybe-enable "clojure-test-mode" "" t)
(add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)

;; load paredit
(require 'paredit)
(dolist (mode '(clojure emacs-lisp lisp scheme lisp-interaction))
  (add-hook (first (read-from-string (concat (symbol-name mode) "-mode-hook")))
            (lambda ()
            (paredit-mode 1)
            (local-set-key (kbd "<M-left>") 'paredit-convolute-sexp))))

;; rainbow parentheses
(require 'highlight-parentheses)
(add-hook 'clojure-mode-hook '(lambda () (highlight-parentheses-mode 1)))
(setq hl-paren-colors
      '("orange1" "yellow1" "greenyellow" "green1"
        "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))

;; magic, haven't broken this down yet
(defmacro defclojureface (name color desc &optional others)
  `(defface ,name '((((class color)) (:foreground ,color ,@others))) ,desc :group 'faces))

; Dim parens - http://briancarper.net/blog/emacs-clojure-colors
(defclojureface clojure-parens       "DimGrey"   "Clojure parens")
(defclojureface clojure-braces       "#49b2c7"   "Clojure braces")
(defclojureface clojure-brackets     "SteelBlue" "Clojure brackets")
(defclojureface clojure-keyword      "khaki"     "Clojure keywords")
(defclojureface clojure-namespace    "#c476f1"   "Clojure namespace")
(defclojureface clojure-java-call    "#4bcf68"   "Clojure Java calls")
(defclojureface clojure-special      "#b8bb00"   "Clojure special")
(defclojureface clojure-double-quote "#b8bb00"   "Clojure special" (:background "unspecified"))

(defun tweak-clojure-syntax ()
  (mapcar (lambda (x) (font-lock-add-keywords nil x))
          '((("#?['`]*(\\|)"       . 'clojure-parens))
            (("#?\\^?{\\|}"        . 'clojure-brackets))
            (("\\[\\|\\]"          . 'clojure-braces))
            ((":\\w+"              . 'clojure-keyword))
            (("#?\""               0 'clojure-double-quote prepend))
            (("nil\\|true\\|false\\|%[1-9]?" . 'clojure-special))
            (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)" 1 'clojure-java-call)))))

(add-hook 'clojure-mode-hook 'tweak-clojure-syntax)

(eval-after-load 'slime-repl-mode
  '(progn (define-key slime-repl-mode-map (kbd "<C-return>") nil)))

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

;; auto-complete-mode
(require 'auto-complete-config)
(ac-config-default)

;; slime auto complete
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)

;; fix indenting in repl
(add-hook 'slime-repl-mode-hook
          (lambda ()
            (define-key slime-repl-mode-map (kbd "<C-return>") nil)
            (setq lisp-indent-function 'clojure-indent-function)
            (set-syntax-table clojure-mode-syntax-table)))

;; enable awesome file prompting
(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point t
        ido-max-prospects 10))

;; https://github.com/nonsequitur/smex/
;; smex: ido for M-x
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; display pretty lambdas
(font-lock-add-keywords 'emacs-lisp-mode
    '(("(\\(lambda\\)\\>" (0 (prog1 ()
                               (compose-region (match-beginning 1)
                                               (match-end 1)
                                               ?λ))))))

(defun lein-swank ()
  (interactive)
  (let ((root (locate-dominating-file default-directory "project.clj")))
    (when (not root)
      (error "Not in a Leiningen project."))
    ;; you can customize slime-port using .dir-locals.el
    (shell-command (format "source ~/.bashrc && cd %s && lein swank %s &" root slime-port)
                   "*lein-swank*")
    (set-process-filter (get-buffer-process "*lein-swank*")
                        (lambda (process output)
                          (when (string-match "Connection opened on" output)
                            (slime-connect "localhost" slime-port)
                            (set-process-filter process nil))))
    (message "Starting swank server...")))

(defun kill-lein-swank ()
  (interactive)
  (kill-process (get-buffer-process "*lein-swank*"))
  (message "Stopping swank server..."))

(global-set-key (kbd "s-=") 'lein-swank)
(global-set-key (kbd "s-+") 'kill-lein-swank)

(fset 'slime-repl-set-default-package
  [?\C-c ?\M-p return])

(defun slime-set-default-package-switch-to-repl ()
  (interactive)
  (execute-kbd-macro 'slime-repl-set-default-package)
  (slime-switch-to-output-buffer)
  (insert "(use 'clojure.repl)")
  (slime-repl-return))

(defun slime-save-compile-and-load-file ()
  (interactive)
  (save-buffer)
  (slime-compile-and-load-file))

(defun slime-save-compile-defun ()
  (interactive)
  (save-buffer)
  (slime-compile-defun)
  (slime-switch-to-output-buffer))

(defun slime-custom-keys ()
  (define-key slime-mode-map (kbd "C-c C-k") 'slime-save-compile-and-load-file)
  (define-key slime-mode-map (kbd "C-c C-c") 'slime-save-compile-defun)
  (define-key slime-mode-map (kbd "C-c C-n") 'slime-set-default-package-switch-to-repl))

(add-hook 'slime-mode-hook 'slime-custom-keys)

(defun slime-custom-repl-keys ()
  (define-key slime-repl-mode-map (kbd "<s-up>") 'slime-repl-backward-input)
  (define-key slime-repl-mode-map (kbd "<s-down>") 'slime-repl-forward-input))

(add-hook 'slime-repl-mode-hook 'slime-custom-repl-keys)

(defun squeeze-whitespace ()
  "Squeeze white space (including new lines) between objects around point.
Leave one space or none, according to the context."
  (interactive "*")
  (skip-chars-backward " \t\r\n\f")
  (set-mark (point))
  (skip-chars-forward " \t\r\n\f")
  (kill-region (point) (mark))
  (insert ?\s)
  (fixup-whitespace))

(global-set-key (kbd "s-6") 'squeeze-whitespace)

(defun insert-line-numbers (beg end &optional start-line)
  "Insert line numbers into buffer."
  (interactive "r")
  (save-excursion
    (let ((max (count-lines beg end))
          (line (or start-line 1))
          (counter 1))
      (goto-char beg)
      (while (<= counter max)
        (insert (format "%0d	" line))
        (beginning-of-line 2)
        (incf line)
        (incf counter)))))

(defun insert-line-numbers+ ()
  "Insert line numbers into buffer."
  (interactive)
  (if mark-active
      (insert-line-numbers (region-beginning) (region-end) (read-number "Start line: "))
    (insert-line-numbers (point-min) (point-max))))

(defun strip-blank-lines ()
  "Strip blank lines in region.
   If no region strip all blank lines in current buffer."
  (interactive)
  (strip-regular-expression-string "^[ \t]*\n"))

(defun strip-line-numbers ()
  "Strip line numbers in region.
   If no region strip all the line numbers in current buffer."
  (interactive)
  (strip-regular-expression-string "^[0-9]+[ \t]?"))

(defun strip-regular-expression-string (regex)
  "Strip all strings that match regex in region.
   If no region strip current buffer."
  (interactive)
  (let ((begin (point-min))
        (end (point-max)))
    (if mark-active
        (setq begin (region-beginning)
              end (region-end)))
    (save-excursion
      (goto-char end)
      (while (and (> (point) begin)
                  (re-search-backward regex nil t))
        (replace-match "" t t)))))

(require 'ace-jump-mode)
;; C-c SPC and C-c C-SPC are ace-jump-word-mode
;; C-u C-c SPC and C-u C-c C-SPC and s-SPC are ace-jump-char-mode
;; C-u C-u C-c SPC and C-u C-u C-c C-SPC are ace-jump-line-mode
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-c C-SPC") 'ace-jump-mode)
(global-set-key (kbd "s-SPC") 'ace-jump-char-mode)
