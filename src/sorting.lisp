;;; Insertion Sort

(defun insert (elem lst)
  (cond ((null lst) (list a))
        ((> (car lst) a) (cons a lst))
        (t (cons (car lst) (insert a (cdr lst))))))

(defun insertion-sort (lst)
  (cond ((null lst) nil)
        (t (insert (car lst) (insertion-sort (cdr lst))))))


;;; Quicksort - this is not the most efficient version of Quicksort,
;;;             as I'm not randomizing the pivot.

(defun quicksort (lst)
  (when lst
    (destructuring-bind (x . xs) lst
      (let ((lt (remove-if-not #'(lambda (y) (< y x)) xs))
            (ge (remove-if-not #'(lambda (y) (>= y x)) xs)))
        (append (quicksort lt) (list x) (quicksort ge))))))
