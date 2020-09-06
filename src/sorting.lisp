;;; Insertion Sort

(defun insert (elem lst)
  (cond ((null lst) (list elem))
        ((> (car lst) elem) (cons elem lst))
        (t (cons (car lst) (insert elem (cdr lst))))))

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


;;; Merge Sort

(defun halve (lst)
  "Splits a sequence in half and returns both
  halves"
  (let ((n (floor (/ (length lst) 2))))
    (values (subseq lst 0 n)
            (subseq lst n))))

(defun simple-merge (xs ys)
  (cond ((null xs) ys)
        ((null ys) xs)
        ((< (first xs) (first ys))
         (cons (first xs) (simple-merge (rest xs) ys)))
        (t (cons (first ys) (simple-merge xs (rest ys))))))

(defun merge-sort (lst)
  (cond ((null lst) nil)
        ((null (rest lst)) lst)
        (t (multiple-value-bind (left right)
             (halve lst)
             (simple-merge (merge-sort left)
                           (merge-sort right))))))
