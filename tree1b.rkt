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
(define NUM-BRANCHES 4)
(define MIN-SIZE 10)
(define DS 0.7)
(define DA 30)
(define BG-COLOUR "black")


;; How likely we have a different num of branches?
(define RAND-BRANCHES 0.5)
;; And how many more / less?
(define RAND-BRANCHES-SCALE 1)
;; How likely we have different branch lengths?
(define RAND-SIZE 0.2)
;; And how much bigger / smaller?
(define RAND-SIZE-SCALE 0.3)
;; How likely we have different angles?
(define RAND-ANGLE 0.4)
;; And how much bigger / smaller?
(define RAND-ANGLE-SCALE 0.1)

(struct point (x y) #:transparent)
(struct branch (start end) #:transparent)

(define (randomise n scale likely)
  ;; increase or decrease n by up to scale randomly
  ;; depending on likely (0 = never, 1 = always)
  (if (< (random) likely)
      (let ([range (exact-round (abs (+ 1 (* 2 scale))))])
        (+ n (- scale (random range))))
      n))

(define (translate-point p length angle)
  (let ([a (degrees->radians angle)])
    (point (+ (point-x p) (* (cos a) length))
           (+ (point-y p) (* (sin a) length)))))

(define (tree p length n angle angle-delta)
  ;; Return a list of lines representing a tree
  (let ([end-point (translate-point p length angle)])
    (list
     (branch p end-point)
     (if (> length 10)
         (for/list ([b (range n)])
           (tree end-point (* length 0.6) n
                 (+ angle (* b angle-delta))
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
      3
      270
      30)
     (empty-scene width height BG-COLOUR)
   )))

;;(draw-tree 80 240)