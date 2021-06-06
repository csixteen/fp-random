#lang racket

(define fibonacci
  (letrec ([f (lambda (x y) (cons (+ x y) (lambda () (f y (+ x y)))))])
    (lambda () (f 0 1))))

(define powers-of-two
  (letrec ([f (lambda (x) (cons (expt 2 x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))
