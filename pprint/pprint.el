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
                    (nrepl-make-response-handler eval-out-buffer
                                                 '()
                                                 (lambda (buffer str)
                                                   (with-current-buffer buffer
                                                     (insert str)))
                                                 (lambda (buffer str)
                                                   (with-current-buffer buffer
                                                     (insert str)))
                                                 '())
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

(provide 'pprint)
