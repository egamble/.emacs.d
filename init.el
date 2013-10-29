;; turn off emacs startup message
(setq inhibit-startup-message t)

;; not necessary for OS X
(menu-bar-mode -1)
(tool-bar-mode -1)

;; make transparent if the window manager supports it
(add-to-list 'default-frame-alist '(alpha 85 75))

(setq frame-title-format '("%f"))

;; allow emacsclient to open files in a running emacs
(server-start)

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

(add-hook 'before-save-hook 'whitespace-cleanup)

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
    (global-set-key (kbd "<s-wheel-up>") 'text-scale-increase)
    (global-set-key (kbd "<s-wheel-down>") 'text-scale-decrease))

  ;; linux
  (progn
    (global-set-key (kbd "<s-mouse-4>") 'text-scale-increase)
    (global-set-key (kbd "<s-double-mouse-4>") 'text-scale-increase)
    (global-set-key (kbd "<s-triple-mouse-4>") 'text-scale-increase)
    (global-set-key (kbd "<s-mouse-5>") 'text-scale-decrease)
    (global-set-key (kbd "<s-double-mouse-5>") 'text-scale-decrease)
    (global-set-key (kbd "<s-triple-mouse-5>") 'text-scale-decrease)

    (global-set-key (kbd "s-s") 'save-buffer)
    (global-set-key (kbd "s-x") 'clipboard-kill-region)
    (global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
    (global-set-key (kbd "s-v") 'clipboard-yank)
    (global-set-key (kbd "s-f") 'isearch-forward)
    (global-set-key (kbd "s-g") 'isearch-repeat-forward)
    (global-set-key (kbd "s-z") 'undo)
    (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)))

(global-set-key [f5] 'revert-buffer)
(global-set-key [f12] 'other-window)

(global-set-key (kbd "<s-right>") 'other-window)
(global-set-key (kbd "<s-left>") '(lambda () "backwards other-window" (interactive) (other-window -1)))

(global-set-key (kbd "C-c c") 'toggle-truncate-lines)
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

;; keyboard macro key bindings
(global-set-key (kbd "C-,")        'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-.")        'kmacro-end-or-call-macro)

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


;; load color-theme
(add-to-list 'load-path "~/.emacs.d/color-theme")
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
;; use wombat
(load-file "~/.emacs.d/color-theme/themes/wombat.el")
(color-theme-wombat)


(let (refreshed)
  (dolist (package '(auto-complete
                     paredit
                     clojure-mode clojure-test-mode
                     cider ac-nrepl
                     highlight-parentheses
                     fold-dwim fold-dwim-org
                     smex
                     markdown-mode
                     ace-jump-mode
                     json-mode))
    (unless (package-installed-p package)
      (when (not refreshed)
        (package-refresh-contents)
        (setq refreshed t))
      (package-install package))))


(require 'cider)
;; hide *nrepl-server* and *nrepl-connection* buffers
(setq nrepl-hide-special-buffers t)
(setq cider-repl-use-pretty-printing t)

(require 'clojure-mode)

(require 'auto-complete-config)
(ac-config-default)

(require 'ac-nrepl)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

;; indent let? the same as let
(define-clojure-indent
  (let? 1))


;; Toggle fold-dwim-org mode with C-tab.
;; While fold-dwim-org mode is enabled:
;;  tab shows/hides block,
;;  S-tab shows/hides all blocks.
(require 'fold-dwim-org)
(global-set-key (kbd "<C-tab>") 'fold-dwim-org/minor-mode)

;; supports fold-dwim-org
;; add separately from other lispish mode hooks because it messes up the nrepl buffer
(add-hook 'clojure-mode-hook 'hs-minor-mode)


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


;; rainbow parentheses
(require 'highlight-parentheses)
(add-hook 'clojure-mode-hook '(lambda () (highlight-parentheses-mode 1)))
(setq hl-paren-colors
      '("orange1" "yellow1" "greenyellow" "green1"
        "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))

(dolist (mode '(clojure cider cider-repl emacs-lisp lisp scheme lisp-interaction))
  (add-hook (first (read-from-string (concat (symbol-name mode) "-mode-hook")))
            (lambda ()
            (highlight-parentheses-mode 1)
            (paredit-mode 1)
            (local-set-key (kbd "<M-left>") 'paredit-convolute-sexp)
            (local-set-key (kbd "<C-M-s-right>") 'forward-select-sexp)
            (local-set-key (kbd "<C-M-s-left>") 'backward-select-sexp)
            )))

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

(defun tweak-clojure-syntax ()
  (mapcar (lambda (x) (font-lock-add-keywords nil x))
          '((("#?['`]*(\\|)"       . 'clojure-parens))
            (("#?\\^?{\\|}"        . 'clojure-brackets))
            (("\\[\\|\\]"          . 'clojure-braces))
            ((":\\w+"              . 'clojure-keyword))
            (("#?\""               0 'clojure-special prepend))
            (("nil\\|true\\|false\\|%[1-9]?" . 'clojure-special))
            (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)" 1 'clojure-java-call)))))

(add-hook 'clojure-mode-hook 'tweak-clojure-syntax)


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


;; display pretty lambdas
(font-lock-add-keywords 'emacs-lisp-mode
    '(("(\\(lambda\\)\\>" (0 (prog1 ()
                               (compose-region (match-beginning 1)
                                               (match-end 1)
                                               ?λ))))))


(defun ensure-three-windows ()
  (let ((c (length (window-list))))
    (cond ((eq c 1) (progn (split-window-horizontally)
                           (ensure-three-windows)))
          ((eq c 2) (progn (other-window 1)
                           (split-window-vertically)
                           (other-window -1))))))

;; Temporary until the real function is fixed so it doesn't throw an error.
(defun nrepl-current-connection-buffer ()
  "The connection to use for nREPL interaction."
  (or nrepl-connection-dispatch
      nrepl-connection-buffer
      (car (nrepl-connection-buffers))))

(defvar repl-ns nil)

(defun start-cider ()
  (interactive)
  (ensure-three-windows)
  (dolist (connection nrepl-connection-list)
    (when connection
      (nrepl-close connection)))
  (nrepl-close-ancilliary-buffers)
  (setq repl-ns nil)
  (cider-jack-in))

(global-set-key (kbd "s-=") 'start-cider)

(defun cider-save-load-switch-to-repl-set-ns ()
  (interactive)
  (save-buffer)
  (cider-load-current-buffer)
  (let* ((ns (cider-current-ns))
         (arg (when (not (equal repl-ns ns))
                (setq repl-ns ns)
                t)))
    (cider-switch-to-repl-buffer arg)))

(defun cider-custom-keys ()
  (define-key cider-mode-map (kbd "C-c C-k") 'cider-save-load-switch-to-repl-set-ns)
  (define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)
  (define-key cider-repl-mode-map (kbd "<s-up>") 'cider-backward-input)
  (define-key cider-repl-mode-map (kbd "<s-down>") 'cider-forward-input))

(add-hook 'cider-mode-hook 'cider-custom-keys)


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


(require 'ace-jump-mode)
;; C-c SPC and C-c C-SPC are ace-jump-word-mode
;; C-u C-c SPC and C-u C-c C-SPC and s-SPC are ace-jump-char-mode
;; C-u C-u C-c SPC and C-u C-u C-c C-SPC are ace-jump-line-mode
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-c C-SPC") 'ace-jump-mode)
(global-set-key (kbd "s-SPC") 'ace-jump-char-mode)


(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; C-c C-f pretty prints a JSON buffer
(autoload 'json-mode "json-mode"
  "Major mode for editing JSON files" t)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))


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
