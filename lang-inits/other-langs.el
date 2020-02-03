(setq sh-basic-offset 2)
(setq c-basic-offset 2)


(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent 2)))


(add-hook 'haskell-mode-hook 'interactive-haskell-mode)


(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-command "/usr/local/bin/markdown")


;; C-c C-f pretty prints a JSON buffer
(autoload 'json-mode "json-mode"
  "Major mode for editing JSON files" t)
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

;; It's more likely that a .mm file is Objective-C than nroff.
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

;; Timeless files
(add-to-list 'auto-mode-alist '("\\.tl\\'" . haskell-mode))

;; Timeless S-expression files
(add-to-list 'auto-mode-alist '("\\.tls\\'" . lisp-mode))

;; TypeScript and TS with embedded JSX
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))


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


(setenv "GOPATH" (shell-command-to-string "$SHELL --login -i -c 'echo $GOPATH'"))


(add-hook 'go-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") 'godef-jump)
            (local-set-key (kbd "M-*") 'pop-tag-mark)
            (local-set-key [f8] (lambda ()
                                  (interactive)
                                  (occur "^\\w+ ")))
            (add-hook 'before-save-hook 'gofmt-before-save)
            (set (make-local-variable 'company-backends) '(company-go))))
(add-hook 'go-mode-hook 'linum-mode)


(add-hook 'sh-mode-hook 'flycheck-mode)


(require 'prettier-js)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook 'linum-mode)


(dolist (mode '(haskell markdown python go))
  (add-hook (first (read-from-string (concat (symbol-name mode) "-mode-hook")))
            (lambda ()
              (when auto-fira-code-mode
                (fira-code-mode 1))     ; Fira Code Symbol ligatures
              )))
