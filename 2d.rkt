#lang racket

;; Functions for 2d drawing and transformation

(struct pos (x y) #:transparent)

(define (inside-circle? circle-pos radius a-pos)
  (define distance
    (sqrt (+ (expt (- (pos-x a-pos) (pos-x circle-pos)) 2)
             (expt (- (pos-y a-pos) (pos-y circle-pos)) 2))))
  (<= distance radius))

(define (between? a x y)
  "Is a between x and y?"
  (or (<= x a y)
      (>= x a y)))

(define (inside-rect? rpos1 rpos2 a-pos)
  "Is a-pos inside the rectangle defined by corners rpos1 and 2?"
  (and (between? (pos-x a-pos) (pos-x rpos1) (pos-x rpos2))
       (between? (pos-y a-pos) (pos-y rpos1) (pos-y rpos2))))

;; -----------------------------------------------------------

(provide pos pos-x pos-y
         between? inside-circle? inside-rect?)
