;;; pospcase-addform.el: A handy form for `pospcase-font-lock' -*- lexical-binding: t -*-

(require 'eieio-custom)

(defgroup pospcase nil
  "A `pcase' powered position extractor.")

(defcustom pospcase-user-file "~/.emacs.d/pospcase-user.el"
  "Elisp file for user customization."
  :group 'pospcase)

(defcustom pospcase-addform--default
  '(lisp-mode
    (`(defun* ,name ,args . ,_))
    font-lock-keyword-face
    ((name . (font-lock-function-name-face))
     ((args . list/1) .
      ((pospcase-font-lock-variable-face-form (match-string 1)))))
    (and (buffer-file-name)
         (equal (file-name-nondirectory (buffer-file-name))
                "asdf.lisp")))

  "Template Form for `pospcase-addform'."

  :type '(list (symbol :tag "Mode")
               (repeat :tag "List of patterns"
                       (sexp :tag "Pattern"))
               (symbol :tag "Keyword Face")
               (repeat :tag "List of Specs"
                       (cons :tag "Spec"
                             (choice (cons :tag "Name/Submatcher"
                                           (sexp :tag "Name")
                                           (sexp :tag "Submatcher"))
                                     (sexp :tag "Name")
                                     )
                             (list :tag "List of Faces"
                                   (sexp :tag "Face"))))
               (sexp :tag "Predicate"))
  :group 'pospcase)

(defclass pospcase-addform--container ()
  ((patterns :type list
             :accessor pospcase-addform-patterns
             :initarg :patterns
             :initform (cadr pospcase-addform--default)
             :custom (editable-list (sexp :tag ""))
             :documentation "⮴ Add pcase patterns.")
   (keyword-face :type symbol
                 :accessor pospcase-addform-keyword-face
                 :initarg :keyword-face
                 :initform (caddr pospcase-addform--default)
                 :custom (sexp :tag "")
                 :documentation "⮴ Specify a face for the heading keyword.")
   (specs :type cons
          :accessor pospcase-addform-specs
          :initarg :specs
          :initform (cadddr pospcase-addform--default)
          :custom (editable-list (cons :tag ""
                                       (choice (symbol :tag "Variable")
                                               (cons :tag "Variable/Submatcher"
                                                     (symbol :tag "Variable")
                                                     (symbol :tag "Submatcher")))
                                       (editable-list :tag "List of Faces"
                                             (sexp :tag "Face"))))
          :documentation "⮴ Add variable/submatcher pair and face list.")
   (mode :type symbol
         :accessor pospcase-addform-mode
         :initarg :mode
         :initform (car pospcase-addform--default)
         :custom (sexp :tag "")
         :documentation "⮴ Specify mode to active the highlighting rule.")
   (predicate :type (or null list)
              :accessor pospcase-addform-predicate
              :initform (car (cddddr pospcase-addform--default))
              :custom (choice (const :tag "Always" nil)
                              (sexp list))
              :documentation "⮴ Specify predicate expression when to enable.")))

(cl-defmethod eieio-done-customizing ((obj pospcase-addform--container))
  (quit-window)
  (with-current-buffer (find-file-noselect pospcase-user-file)
    (goto-char (point-max))
    (insert (pp-to-string
             (if (pospcase-addform-predicate obj)
                 `(add-hook ',(intern (concat (symbol-name (mode obj)) "-hook"))
                            (lambda ()
                              (when ,(pospcase-addform-predicate obj)
                                (pospcase-font-lock ',(mode obj)
                                                    ',(pospcase-addform-patterns obj)
                                                    ',(cons (keyword-face obj)
                                                            (pospcase-addform-specs obj))
                                                    t))))
               `(pospcase-font-lock ',(mode obj)
                                    ',(pospcase-addform-patterns obj)
                                    ',(cons (keyword-face obj)
                                            (pospcase-addform-specs obj))))))))

(add-hook 'eieio-custom-mode-hook
          (lambda ()
            (when (string-match "pospcase-addform--container" (buffer-name))
              (setq-local widget-button-face custom-button)
              (setq-local widget-push-button-prefix "")
              (setq-local widget-push-button-suffix "")
              (setq-local completion-at-point-functions '(elisp-completion-at-point t)))))

(put 'eieio-custom-mode 'derived-mode-parent 'emacs-lisp-mode) ; for smartparens

;;;###autoload
(defun pospcase-addform ()
  (interactive)
  (let ((obj (make-instance 'pospcase-addform--container))
        (g 'default))
    (select-window
     (display-buffer (get-buffer-create (concat
                                         "*CUSTOMIZE "
                                         (eieio-object-name obj)
                                         " "
                                         (symbol-name g)
                                         "*"))))
    (eieio-customize-object obj)))

(provide 'pospcase-addform)
