#lang racket/base

(require racket/list)

(define-syntax-rule (times-repeat n fn)
  (for/list ([i (in-range n)])
    fn))

(define (random-range a b)
  (+ a (random (+ 1 (- b a)))))

(define (random-choice list)
  (if (empty? list)
      #f
      (list-ref list (random (length list)))))

(define (random-sign exp)
  ;; Randomly return +exp or -exp
  (* exp (random-choice (list -1 1))))

(define (every-other list)
  (cond
    [(empty? list) '()]
    [(= (length list) 1) list]
    [else
     (cons (car list) (every-other (cddr list)))]))

(provide times-repeat random-range random-choice random-sign
         every-other)