;; turn off emacs startup message
(setq inhibit-startup-message t)


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
                     buffer-move
                     go-mode
                     company-go
                     exec-path-from-shell
                     flycheck
                     typescript-mode
                     prettier-js))
    (unless (package-installed-p package)
      (when (not refreshed)
        (package-refresh-contents)
        (setq refreshed t))
      (package-install package))))


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

(global-set-key [f7]                       '(lambda () "revert-buffer noconfirm" (interactive) (revert-buffer nil t)))
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


(require 'company)
(add-hook 'after-init-hook 'global-company-mode)


;; Toggle fold-dwim-org mode with C-tab.
;; While fold-dwim-org mode is enabled:
;;  tab shows/hides block,
;;  S-tab shows/hides all blocks.
(require 'fold-dwim-org)
(global-set-key (kbd "<C-tab>") 'fold-dwim-org/minor-mode)


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

(global-set-key [f9] 'toggle-window-dedicated)


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


(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))
(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map [mouse-1] 'dired-find-file)
             (dired-hide-details-mode)))
(setq dired-auto-revert-buffer t)


(load-file "~/.emacs.d/lang-inits/fira-code.el")
(dolist (mode '(emacs-lisp))
  (add-hook (first (read-from-string (concat (symbol-name mode) "-mode-hook")))
            (lambda ()
              (fira-code))))


(load-file "~/.emacs.d/lang-inits/all-lisps.el")
(load-file "~/.emacs.d/lang-inits/other-langs.el")
;; (load-file "~/.emacs.d/lang-inits/timeless.el")


;; provides pretty lambda, etc.
(global-prettify-symbols-mode 1)


(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(default ((t (:height 150)))))
