;; I saw this challenge for an interview question, where the candidate
;; is expected to write a program that prints a Christmas Tree with a
;; certain height. If the specified height is N, then the first row should
;; have a '*' at the top and the following rows should be filled with '0',
;; as follows:
;;
;; N = 5
;;
;;   *
;;   0
;;  000
;; 00000
;;0000000
;;
;; N = 10
;;
;;        *
;;        0
;;       000
;;      00000
;;     0000000
;;    000000000
;;   00000000000
;;  0000000000000
;; 000000000000000
;;00000000000000000
;;
;; I decided to implement this in Lisp, first with a single compact non-functional
;; solution, but then I thought that it might be interesting to write something
;; a bit more comprehensible.


;; Solution 1

(defun xmas (n)
  (let ((padding (1- (* 2 (1- n)))))
    (labels ((print-row (p xs) (format t "~v,,,' @:<~{~d~}~>~%" p xs)))
      (print-row padding '(*))
      (loop for x from 1 to (1- n) do
	    (let ((z (loop for y from 1 to (1- (* 2 x)) collect 0)))
	      (print-row padding z))))))


;; Solution 2

(defun build-row (width j row-symbol)
  "Builds a string representing a row with a certain width,
  given a row number and a symbol."
  (format nil 
	  "~v,,,' @:<~{~d~}~>" 
	  width
	  (loop for i from 1 to (1- (* 2 j)) collect row-symbol)))

(defun build-tree (n)
  "Builds a Christmas Tree with height n, 
  represented as a list of strings."
  (let ((width (1- (* 2 (1- n)))))
    (cons (build-row width 1 #\*)
	  (loop for i from 1 to (1- n)
		collect (build-row width i 0)))))

(defun print-xmas-tree (rows)
  (dolist (e rows) (format t "~a~%" e)))

(defun xmas2 (n)
  (print-xmas-tree (build-tree n)))
