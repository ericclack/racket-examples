#lang racket

#|
Draw a fractal tree

DONE:
- Refactored recursion

TODO:
- Separate out generating branches from drawing tree
- Get rid of constants and make a tree-maker function
  to create trees of a family
|#


(require 2htdp/image)
(require racket/trace)
(require "util.rkt")

(define BRANCH-COLOUR "green")
(define BG-COLOUR "black")

(struct point (x y) #:transparent)
(struct branch (start end) #:transparent)

(define (translate-point p length angle)
  (let ([a (degrees->radians angle)])
    (point (+ (point-x p) (* (cos a) length))
           (+ (point-y p) (* (sin a) length)))))

(define (tree p length num-branches angle angle-delta)
  ;; Return a list of branches representing a tree
  (let ([end-point (translate-point p length angle)])
    (list
     (branch p end-point)
     (if (> length 10)
         (for/list ([b (range num-branches)])
           (tree end-point (* length 0.6) num-branches
                 (+ angle (* (- (/ num-branches 2) b) angle-delta))
                 angle-delta))
         '()
         ))))

(define (scene+branch b scene)
  (scene+line scene (point-x (branch-start b)) (point-y (branch-start b))
              (point-x (branch-end b)) (point-y (branch-end b))
              BRANCH-COLOUR))

(define (scene+tree t scene)
  (foldl scene+branch
         scene
         (flatten t)))

(define (draw-tree size)
  ;; Guess at width and height
  (let* ([width (* size 5)]
         [height (* size 5)])
    (scene+tree
     (tree
      (point (/ width 2) (- height 10))
      size
      4 ;; branches
      270 ;; up
      35 ;; angle between branches
      )
     (empty-scene width height BG-COLOUR)
   )))

(draw-tree 100)