;;; pospcase-addform.el: A handy forms for `pospcase-font-lock' customization -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 gatchapin

;; Author: gatchapin
;; Keywords: languages, faces
;; URL: https://github.com/gatchapin2/pospcase

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'eieio-custom)

(defgroup pospcase-addform nil
  "A `pcase' powered position extractor."
  :group 'eieio)

(defcustom pospcase-user-file "~/.emacs.d/pospcase-user.el"
  "Elisp file for user customization."
  :type 'string
  :group 'pospcase-addform)

(defcustom pospcase-addform--default
  '(lisp-mode
    (`(defun* ,name ,args . ,_))
    ((heading-keyword . (font-lock-keyword-face))
     (name . (font-lock-function-name-face))
     ((args . list/1) .
      ((pospcase-font-lock-variable-face-form (match-string 3)))))
    (and (buffer-file-name)
         (equal (file-name-nondirectory (buffer-file-name))
                "asdf.lisp")))

  "Template Form for `pospcase-addform'."

  :type '(list (symbol :tag "Mode")
               (repeat :tag "List of patterns"
                       (sexp :tag "Pattern"))
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
  :group 'pospcase-addform)

(defclass pospcase-addform--container ()
  ((patterns :type list
             :initarg :patterns
             :custom (editable-list (sexp :tag ""))
             :documentation "⮴ Add pcase patterns.")
   (specs :type cons
          :initarg :specs
          :custom (editable-list (cons :tag ""
                                       (choice (symbol :tag "Variable")
                                               (cons :tag "Variable/Submatcher"
                                                     (symbol :tag "Variable")
                                                     (symbol :tag "Submatcher")))
                                       (editable-list :tag "List of Faces"
                                             (sexp :tag "Face"))))
          :documentation "⮴ Add variable/submatcher pair and face list.")
   (mode :type symbol
         :initarg :mode
         :custom (sexp :tag "")
         :documentation "⮴ Specify mode to active the highlighting rule.")
   (predicate :type (or null list)
              :initarg :predicate
              :custom (choice (const :tag "Always" nil)
                              (sexp list))
              :documentation "⮴ Specify predicate expression when to enable.")))

(cl-defmethod eieio-done-customizing ((obj pospcase-addform--container))
  (quit-window)
  (with-current-buffer (find-file-noselect pospcase-user-file)
    (goto-char (point-max))
    (insert (pp-to-string
             (if (slot-value obj 'predicate)
                 `(add-hook ',(intern (concat (symbol-name (slot-value obj 'mode)) "-hook"))
                            (lambda ()
                              (when ,(slot-value obj 'predicate)
                                (pospcase-font-lock ',(slot-value obj 'mode)
                                                    ',(slot-value obj 'patterns)
                                                    ',(slot-value obj 'specs)
                                                    t))))
               `(pospcase-font-lock ',(slot-value obj 'mode)
                                    ',(slot-value obj 'patterns)
                                    ',(slot-value obj 'specs)))))))

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
  (let ((obj (make-instance 'pospcase-addform--container
                            :patterns (cadr pospcase-addform--default)
                            :specs (caddr pospcase-addform--default)
                            :mode (car pospcase-addform--default)
                            :predicate (car (cdddr pospcase-addform--default))))
        (g 'default))
    (select-window
     (display-buffer (get-buffer-create (concat
                                         "*CUSTOMIZE "
                                         (eieio-object-name obj)
                                         " "
                                         (symbol-name g)
                                         "*"))))
    (eieio-customize-object obj)
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (let ((inhibit-read-only t))
        (delete-region (point) (progn
                                 (forward-line 3)
                                 (backward-char)
                                 (point)))))))

(provide 'pospcase-addform)
