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

(define (randomise n scale likely)
  ;; increase or decrease n by up to scale randomly
  ;; depending on likely (0 = never, 1 = always)
  (if (< (random) likely)
      (let ([range (exact-round (abs (+ 1 (* 2 scale))))])
        (+ n (- scale (random range))))
      n))

(define (translate-point x y length angle)
  (let ([a (degrees->radians angle)])
    (list (+ x (* (cos a) length))
          (+ y (* (sin a) length)))))

(define (branch-lines x y length n angle angle-delta)
  ;; Return a list of n lines representing branches radiating from x, y
  ;; spread out by angle-delta degrees
  (cond [(zero? n) '()]
        [else
         (cons (list* x y (translate-point x y length angle))
               (branch-lines x y length (sub1 n) (+ angle angle-delta) angle-delta))]))

(define (scene+branch branch scene)
  (scene+line scene (first branch) (second branch)
              (third branch) (fourth branch)
              BRANCH-COLOUR))

(define (scene+tree x y size angle scene)
  (foldl scene+branch
         scene
         (branch-lines x y size NUM-BRANCHES angle DA)))

(define (tree size angle)
  ;; Guess at width and height
  (let* ([width (* size 5)]
         [height (* size 5)])
    (scene+tree
     (/ width 2) (- height 10)
     size angle
     (empty-scene width height BG-COLOUR)
   )))

(tree 80 240)