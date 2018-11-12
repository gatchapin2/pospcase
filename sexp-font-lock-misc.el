(defun  sexp-font-lock-match-flat-list (limit) ; none uses this so far
  (sexp-font-lock-iterate
   (mapcar (lambda (srpair) (list (cdr srpair)))
           (car (sexp-font-lock-read-at-point)))
   limit))

(defun sexp-mark-pattern-at-point (pat)
  (interactive "xPattern to match: ")
  (let ((match (sexp-font-lock-match-at-point-do pat)))
    (when match
      (cond
       ((numberp (cdr match))                                ; (BEG . END)
        (goto-char (car match))
        (push-mark (cdr match) nil t))
       ((and (consp (cdr (car match)))
             (numberp (cddr (car match))))                   ; ((SEXP BEG . END)
                                                             ;  ... (SEXP BEG . END))
        (goto-char (cadr (car match)))
        (push-mark (cddr (car (last match))) nil t))
       (t
        (goto-char (if (numberp (car (car match)))           ; ((BEG . END) ...)
                       (car (car match))
                     (cadar (car match))))                   ; (((SEXP BEG . END) ...) ...)
        (push-mark (if (numberp (cdr (car (last match))))    ; (... (BEG . END))
                       (cdr (car (last match)))
                     (cddr (car (last (car (last match)))))) ; (... (... (SEXP BEG . END)))
                   nil t))))))
