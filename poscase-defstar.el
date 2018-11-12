(defun sexp-font-lock-defstar-keywords ()
  `(;; defstar function type
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-defcommand-functions)
              "[ \t\n]+(")
     (sexp-font-lock-match-defstar-type
      ;; Pre-match form
      (progn
        (goto-char (match-end 0))
        ;; Search limit
        (save-excursion
          (backward-char)               ; Position point before "(".
          (ignore-errors (scan-sexps (point) 1))))
      ;; Post-match form
      (goto-char (match-end 0))
      (0 font-lock-type-face)))

    ;; defstar argument list name
    (,(concat "("
              (regexp-opt sexp-font-lock-defcommand-functions)
              "[ \t\n]+")
     (sexp-font-lock-match-defmethod-argument-list
      ;; Pre-match form
      (progn
        (goto-char (match-end 0))
        (condition-case nil
            (forward-sexp)
          (error (end-of-defun)))
        (forward-comment (buffer-size))
        (and
         (looking-at (regexp-opt sexp-font-lock-defmethod-keywords))
         (goto-char (match-end 0)))
        (forward-comment (buffer-size))
        (forward-char)
        ;; Search limit
        (save-excursion
          (backward-char)
          (ignore-errors (scan-sexps (point) 1))))
      ;; Post-match form
      (goto-char (match-end 0))
      (0 ,(lisp-extra-font-lock-variable-face-form '(match-string 0))
         nil t)))

    ;; defstar argument list type
    (,(concat "("
              (regexp-opt sexp-font-lock-defstar-functions)
              "[ \t']+")
     (sexp-font-lock-match-defmethod-argument-list-type
      ;; Pre-match form
      (progn
        (goto-char (match-end 0))
        (condition-case nil
            (forward-sexp)
          (error (end-of-defun)))
        (forward-comment (buffer-size))
        (and
         (looking-at (regexp-opt sexp-font-lock-defmethod-keywords))
         (goto-char (match-end 0)))
        (forward-comment (buffer-size))
        (forward-char)
        ;; Search limit
        (save-excursion
          (backward-char)               ; Position point before "(".
          (ignore-errors (scan-sexps (point) 1))))
      ;; Post-match form
      (goto-char (match-end 0))
      (0 font-lock-type-face)))))


(defvar sexp-font-lock-defstar--installed-keywords nil)

(defun sexp-font-lock-defstar-add-keywords ()
  "Add extra font-lock keywords to lisp."
  (set (make-local-variable 'font-lock-multiline) t)
  (when (local-variable-p 'sexp-font-lock-defstar--installed-keywords)
    (font-lock-remove-keywords nil sexp-font-lock-defstar--installed-keywords))
  (let ((keywords (sexp-font-lock-defstar-keywords)))
    (set (make-local-variable 'sexp-font-lock-defstar--installed-keywords)
         keywords)
    (font-lock-add-keywords nil keywords 'append)))


(defun sexp-font-lock-defstar-remove-keywords ()
  "Remove font-lock keywords for extra lisp highlithing."
  (font-lock-remove-keywords nil sexp-font-lock-defstar--installed-keywords))


(defgroup sexp-font-lock nil
  "Highlight bound variables and quoted expressions in lisp."
  :group 'faces)

;;;###autoload
(defcustom sexp-font-lock-defstar-modes '(emacs-lisp-mode lisp-mode)
  "List of modes where Lisp Extra Font Lock Global mode should be enabled."
  :type '(repeat symbol)
  :group 'sexp-font-lock)

;;;###autoload
(define-minor-mode sexp-font-lock-defstar-mode
  "Minor mode that highlights bound variables and quoted expressions in lisp."
  :group 'sexp-font-lock
  (if sexp-font-lock-defstar-mode
      (sexp-font-lock-defstar-add-keywords)
    (sexp-font-lock-defstar-remove-keywords))
  ;; As of Emacs 24.4, `font-lock-fontify-buffer' is not legal to
  ;; call, instead `font-lock-flush' should be used.
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))


;;;###autoload
(define-global-minor-mode sexp-font-lock-defstar-global-mode
  sexp-font-lock-defstar-mode
  (lambda ()
    (when (apply 'derived-mode-p sexp-font-lock-defstar-modes)
      (sexp-font-lock-defstar-mode 1)))
  :group 'sexp-font-lock)

