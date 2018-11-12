(defun  sexp-font-lock-match-flat-list (limit) ; none uses this so far
  (sexp-font-lock-iterate
   (mapcar (lambda (srpair) (list (cdr srpair)))
           (car (sexp-font-lock-read-at-point)))
   limit))
