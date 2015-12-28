#lang racket/base

(define-syntax-rule (times-repeat n fn)
  (for/list ([i (in-range n)])
    fn))

(define (random-range a b)
  (+ a (random (+ 1 (- b a)))))

(define (random-choice list)
  (list-ref list (random (length list))))

(define (random-sign exp)
  ;; Randomly return +exp or -exp
  (* exp (random-choice (list -1 1))))

(provide times-repeat random-range random-choice random-sign)