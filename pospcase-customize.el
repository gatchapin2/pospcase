(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defvar pospcase-wizard--patterns)
(defvar pospcase-wizard--specs)

(defun pospcase-wizard ()
  "Create the widgets for `pospcase-font-lock'."
  (interactive)
  (switch-to-buffer "*Pospcase Font Lock*")
  (kill-all-local-variables)
  (make-local-variable 'pospcase-wizard--patterns)
  (make-local-variable 'pospcase-wizard--specs)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "Add Pospcase Font Lock Keywords.\n\n")


  (widget-create 'sexp
                 :size 13
                 :format "Mode: %v "
                 'lisp-mode)

  (widget-create 'menu-choice
                 :tag "Scope: "
                 :value nil
                 :notify (lambda (widget &rest ignore)
                           (message "buffer-local-p is %s."
                                    (widget-value widget)))
                 '(item :tag "Global" :value nil)
                 '(choice-item :tag "Buffer-local" t))

  (widget-insert "\nPatterns:\n")
  (setq pospcase-wizard--patterns
        (widget-create 'editable-list
                       :entry-format "%i %d %v"
                       :notify
                       (lambda (widget &rest ignore)
                         (let ((old (widget-get widget
                                                ':list-length))
                               (new (length (widget-value widget))))
                           (unless (eq old new)
                             (widget-put widget ':list-length new))))
                       :value '(`(defun ,name ,args . ,_))
                       '(sexp :value `(defun ,name ,args . ,_))))

  (widget-insert "\nGuess specs ")
  (widget-create 'link
                 :notify (lambda (&rest ignore)
                           (widget-value-set pospcase-wizard--specs
                                             '("En" "To" "Tre"))
                           (widget-setup))
                 "Generate")

  (widget-create 'editable-list
                 :entry-format "%i %d %v"
                 :children (widget-create-child-and-convert
		            widget 'custom-comment
		            :parent widget
		            :value (or comment "")))

  (widget-insert "\n\nSpecs:\n")
  (setq pospcase-wizard--specs
        (widget-create 'editable-list
                       :entry-format "%i %d %v"
                       :notify
                       (lambda (widget &rest ignore)
                         (let ((old (widget-get widget
                                                ':list-length))
                               (new (length (widget-value widget))))
                           (unless (eq old new)
                             (widget-put widget ':list-length new))))
                       :value '((nil . font-lock-keyword-face)
                                (name . (font-lock-function-name-face))
                                ((args . list/1) . ((pospcase-font-lock-variable-face-form (match-string 1)))))
                       '(sexp :type (cons var-pair font-list) :value nil)))

  (widget-insert "\n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (if (= (length
                                   (widget-value pospcase-wizard--specs))
                                  3)
                               (message "Congratulation!")
                             (error "Three was the count!")))
                 "Save Keyword")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (pospcase-wizard))
                 "Reset Keyword")
  (widget-insert "\n")

  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min)))


;;; eieio based attempt

(defclass pospcase-customize--container ()
  ((patterns :type list
             :initarg :patterns
             :initform '(`(defun ,name ,args . ,_))
             :custom (editable-list sexp)
             :documentation "The list `pcase' patterns to match.")

   (keyword-face :type symbol
                 :initarg :keyword-face
                 :initform 'font-lock-keyword-face
                 :custom sexp
                 :documentation "The face of the heading keyword.")

   (specs :type cons
          :initarg :specs
          :initform '(((foo . bar) . (baz)))
          :custom (editable-list (cons sexp sexp))
          :documentation "The spec list for `pospcase-font-lock'. The first of a list is (var-name . submatcher). The second is a list of faces.")))

(eieio-customize-object
 (make-instance 'pospcase-customize--container))
