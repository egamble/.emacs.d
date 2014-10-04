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
                     projectile))
    (unless (package-installed-p package)
      (when (not refreshed)
        (package-refresh-contents)
        (setq refreshed t))
      (package-install package))))


(require 'cider)
(require 'clojure-mode)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(require 'company)
(global-company-mode)


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


;; In addition to the following commands, remember these useful paredit commands:
;; paredit-forward-slurp-sexp <C-right>, C-)
;; paredit-forward-barf-sexp  <C-left>, C-}
;; paredit-raise-sexp         M-r
;; paredit-wrap-round         M-(
;; paredit-join-sexps         M-J

;; ...and this useful clojure-mode command:
;; clojure-toggle-keyword-string C-:

(dolist (mode '(clojure cider cider-repl emacs-lisp lisp scheme lisp-interaction))
  (add-hook (first (read-from-string (concat (symbol-name mode) "-mode-hook")))
            (lambda ()
            (highlight-parentheses-mode 1)
            (paredit-mode 1)
            (local-set-key (kbd "<M-left>")      'paredit-convolute-sexp)
            (local-set-key (kbd "<M-s-right>")   'forward-select-sexp)
            (local-set-key (kbd "<C-M-s-right>") 'forward-select-sexp)
            (local-set-key (kbd "<M-s-left>")    'backward-select-sexp)
            (local-set-key (kbd "<C-M-s-left>")  'backward-select-sexp)
            )))


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
  (cider-load-current-buffer)
  (let* ((ns (cider-current-ns))
         (arg (when (not (equal repl-ns ns))
                (setq repl-ns ns)
                4)))
    (cider-switch-to-repl-buffer arg)))


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


(defun ensure-four-windows ()
  (let ((c (length (window-list))))
    (cond ((eq c 1) (progn (split-window-horizontally)
                           (ensure-four-windows)))
          ((eq c 2) (progn (other-window 1)
                           (split-window-vertically)
                           (other-window -1)
                           (ensure-four-windows)))
          ((eq c 3) (progn (other-window 2)
                           (split-window-vertically)
                           (other-window -2))))))


(defvar start-clojure-window nil)

(defun start-cider ()
  (interactive)

  (if (let ((bn (buffer-name (current-buffer))))
        (and (string-match ".clj" bn)
             (not (string-match "project.clj" bn))))
      (progn
        (setq start-clojure-window (car (window-list)))

        ;; Undedicate all windows.
        (dolist (w (window-list))
          (set-window-dedicated-p w nil))

        (delete-other-windows)
        (ensure-four-windows)

        ;; Any other window with the current buffer is switched to something else, so after-start-cider won't get confused.
        (let ((ws (get-buffer-window-list (current-buffer))))
          (dolist (w (cdr ws))
            (set-window-buffer w "*Messages*")))

        (dolist (connection nrepl-connection-list)
          (when connection
            (nrepl-close connection)))
        (cider-close-ancilliary-buffers)
        (setq repl-ns nil)
        (cider-jack-in))

    (message "Buffer %s is not a Clojure source file" (current-buffer))))


;; 1. Tries to move the REPL to the lower right window.
;; 2. Puts the nrepl-server buffer in the window above the REPL window and shrinks the server window.
;; 3. Loads the Clojure buffer and sets the namespace in the REPL.
;; 4. Goes back to the Clojure window.
(defun after-start-cider ()
  (interactive)

  ;; Select the REPL window if it's not already selected.
  (when (not (string-match "cider-repl" (buffer-name (current-buffer))))
    (dolist (w (window-list))
      (when (string-match "cider-repl" (buffer-name (window-buffer w)))
        (select-window w))))

  (let* ((repl-win (car (window-list)))
         (repl-buf (current-buffer))
         (server-buf (replace-regexp-in-string
                      "cider-repl" "nrepl-server"
                      (buffer-name repl-buf))))

    ;; From the REPL window, switch to the Clojure window.
    (select-window start-clojure-window)

    ;; Switch the REPL window to some other buffer, in case there are more than three windows and the REPL is in the wrong one.
    (set-window-buffer repl-win "*Messages*")

    ;; Put the REPL in the window before the Clojure buffer, i.e. the bottom right window (usually).
    (other-window -1)
    (switch-to-buffer repl-buf)

    ;; Lock the REPL window to the REPL buffer.
    (set-window-dedicated-p (get-buffer-window (current-buffer)) t)

    ;; Put the server buffer in the window before the REPL, generally just above it.
    (other-window -1)
    (switch-to-buffer server-buf)

    ;; Lock the server window to the server buffer.
    (set-window-dedicated-p (get-buffer-window (current-buffer)) t)

    ;; Shrink the server window to window-min-height.
    (shrink-window 100)

    ;; Select the REPL window again.
    (other-window 1))

  ;; Go to the Clojure window and load the Clojure code (which goes back to the REPL window and sets the NS) then go back to the Clojure window.
  (select-window start-clojure-window)
  (cider-save-load-switch-to-repl-set-ns)
  (select-window start-clojure-window))


(defun start-cider-or-after-start ()
  "If the current buffer is not a cider REPL, run start-cider.
Otherwise run after-start-cider, which organizes the windows, loads the code from the
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


(defun pprint-def-in-place (&optional ARG)
  "Pretty-print the current top-level form in place.
Assumes comments follow the usual number-of-semicolons
convention. With a prefix ARG >1, ignores existing commas and
inserts new commas in map literals."
  (interactive "p")
  (let* ((defun-region (cider--region-for-defun-at-point))
         (orig-buffer (current-buffer))
         (eval-out-buffer (generate-new-buffer "temp"))
         (comment-marker ";¬")
         (comment-marker-len (length comment-marker))
         (newline-marker "n¬")
         (newline-marker-len (length newline-marker))
         (quote-marker "q¬")
         (backslash-marker "b¬")
         (comma-keyword ":c¬")
         (meta-keyword ":m¬")

         ;; Keyword marker to insert after stringified comments or
         ;; comma-marking keywords to ensure literal hash maps have an
         ;; even number of forms.
         (balancing-keyword ":a¬"))

    (with-temp-buffer
      (apply #'insert-buffer-substring-no-properties orig-buffer defun-region)
      (lisp-mode)

      ;; Replace newlines in strings with markers to allow their preservation
      ;; through pprinting. Otherwise they become indistinguishable
      ;; from "\n"s.
      (goto-char (point-min))
      (while (search-forward "\"" nil t)
        (when (paredit-in-string-p)
          (let ((end (cdr (paredit-string-start+end-points))))
            (while (search-forward "\n" end 1)
              (replace-match newline-marker)
              (setq end (+ end newline-marker-len -1))))))

      ;; Stringify comments with leading comment markers to preserve
      ;; them through pprinting. Preserve quotes and backslashes with
      ;; markers.
      (goto-char (point-min))
      (while (search-forward ";" nil t)
        (when (paredit-in-comment-p)
          (backward-char)
          (insert "\"")
          (insert comment-marker)
          (let ((start (point)))
            (while (search-forward "\"" (line-end-position) t)
              (replace-match quote-marker))
            (goto-char start)
            (while (search-forward "\\" (line-end-position) 1)
              (replace-match backslash-marker)))
          (insert "\"")
          ;; Add a keyword immediately after a stringified comment so
          ;; that literal hash maps are balanced.
          (insert balancing-keyword)))

      ;; Unless there's a prefix ARG >1, preserve commas by replacing
      ;; with comma-marking keywords.  Then insert another keyword so
      ;; that literal hash maps are balanced.
      (when (= ARG 1)
        (goto-char (point-min))
        (while (search-forward "," nil t)
          (when (not (paredit-in-string-p))
            ;; Can't use replace-match because it's messed up by paredit-in-string-p.
            (delete-backward-char 1)
            (insert ?\s)
            (insert comma-keyword)
            (insert ?\s)
            (insert balancing-keyword)
            (insert ?\s))))

      ;; Preserve metadata by replacing the leading ^s with
      ;; metadata-marking keywords. No need for another keyword to
      ;; keep hash maps balanced because the metadata itself provides the
      ;; balancing s-exp.
      (goto-char (point-min))
      (while (search-forward "^" nil t)
        (when (not (paredit-in-string-p))
          ;; Can't use replace-match because it's messed up by paredit-in-string-p.
          (delete-backward-char 1)
          (insert ?\s)
          (insert meta-keyword)
          (insert ?\s)))

      (let ((form (buffer-substring (point-min) (point-max))))
        (cider-eval (format "(clojure.pprint/with-pprint-dispatch clojure.pprint/code-dispatch (clojure.pprint/pprint '%s))" form)
                    (cider-popup-eval-out-handler eval-out-buffer)
                    (cider-current-ns))))

    (with-current-buffer eval-out-buffer
      ;; OPTIMIZE: I thought maybe putting all the following stuff in
      ;; a done-handler (by writing cider-popup-eval-out-done-handler)
      ;; would avoid the need to sleep. The done-handler worked, but
      ;; it still required a sleep afterward for some reason.
      (while (= (point-min) (point-max))
        (sleep-for 0.001))

      (lisp-mode)

      ;; Strip out the balancing keywords and any preceding whitespace.
      (goto-char (point-min))
      (while (re-search-forward (concat "[ \n]*" balancing-keyword) nil t)
        (replace-match ""))

      ;; Unless there's a prefix ARG >1, strip out all commas not in
      ;; strings (that were inserted by pprinting), then change all
      ;; comma-marking keywords (with preceding whitespace) into commas.
      (when (= ARG 1)
        (goto-char (point-min))
        (while (search-forward "," nil t)
          (when (not (paredit-in-string-p))
            ;; Can't use replace-match because it's messed up by paredit-in-string-p.
            (delete-backward-char 1)))

        (goto-char (point-min))
        (while (re-search-forward (concat "[ \n]*" comma-keyword) nil t)
          (replace-match ",")))

      ;; Change all metadata-marking keywords (with following whitespace) into ^s.
      (goto-char (point-min))
      (while (re-search-forward (concat meta-keyword "[ \n]*") nil t)
        (replace-match "^"))

      ;; Replace newline markers in strings with newlines.
      ;; Un-stringify comments and un-preserve their quotes and
      ;; backslashes. Add newlines before or after comments as needed.
      ;; Assumes more than one semicolon means the comment should be
      ;; on its own line.
      (goto-char (point-min))
      (while (search-forward "\"" nil t)
        (when (paredit-in-string-p)
          (let* ((b (paredit-string-start+end-points))
                 (start (car b))
                 (end (cdr b)))
            (if (search-forward comment-marker end t)

                (progn (replace-match "")
                       (delete-backward-char 1)
                       (goto-char (- end comment-marker-len 1))
                       (delete-forward-char 1)
                       ;; fix pprinted commas after comments only when there's an ARG >1
                       (when (> ARG 1)
                         (setq has-comma-after (= ?\, (char-after (point))))
                         (when has-comma-after (delete-forward-char 1)))
                       (when (/= (point) (line-end-position))
                         (open-line 1))
                       (goto-char start)
                       (progn
                         (skip-chars-backward " \n")
                         (delete-region (point) start))
                       ;; fix pprinted commas before comments only when there's an ARG >1
                       (when (> ARG 1)
                         (if (and (= ?\, (char-before (point)))
                                  (not has-comma-after))
                             (delete-backward-char 1)))
                       (setq start (point))
                       (while (search-forward quote-marker (line-end-position) t)
                         (replace-match "\""))
                       (goto-char start)
                       (while (search-forward backslash-marker (line-end-position) t)
                         (replace-match "\\\\"))
                       (goto-char start)
                       ;; multiple semicolons
                       (when (= ?\; (char-after (1+ (point))))
                         (open-line 1)))

              (while (search-forward newline-marker end t)
                (replace-match "
"
                               )
                (setq end (- end newline-marker-len 1))))))))

    (apply #'delete-region defun-region)
    (insert-buffer eval-out-buffer)
    (paredit-reindent-defun)
    (paredit-forward)
    (kill-buffer eval-out-buffer)))


;; Also remember:
;; C-c C-d ? shows all doc key bindings
;; C-c C-z switches back and forth between the REPL and the last Clojure buffer
;; [f10] and C-c C-k are like C-c C-z, but also save, compile and set ns
;; [f9], M-s-down and C-c C-e evaluate expression preceding point
;; C-c C-p cider-pprint-eval-last-sexp
;; C-c C-r evaluates region
;; C-C C-c evaluates def at point
;; C-up, C-down and s-up, s-down go backward and forward in REPL history
;; C-c , run all tests
;; C-c C-, rerun all tests
;; C-c M-, run one test

(defun cider-custom-keys ()
  (define-key cider-mode-map      (kbd "C-c C-k")      'cider-save-load-switch-to-repl-set-ns)
  (define-key cider-mode-map      [f10]                'cider-save-load-switch-to-repl-set-ns)
  (define-key cider-mode-map      (kbd "<M-s-down>")   'cider-save-eval-last-sexp)
  (define-key cider-mode-map      (kbd "<C-M-s-down>") 'cider-save-eval-last-sexp)
  (define-key cider-mode-map      [f9]                 'cider-save-eval-last-sexp)
  (define-key cider-mode-map      (kbd "<s-f9>")       'save-insert-last-sexp-in-repl)
  (define-key cider-mode-map      [f8]                 'create-clj-tags)
  (define-key cider-mode-map      (kbd "C-M-q")        'pprint-def-in-place)

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
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))


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
