#lang racket

#|
Draw a fractal tree

DONE:
- Split out generating tree (branches) from drawing it

TODO:
- Vary branch width according to length
- Draw sub-branches along branch rather than at end?
|#


(require 2htdp/image)
(require racket/trace)
(require (planet williams/science/random-distributions/gaussian))

(require "util.rkt")

(define BRANCH-COLOUR "white")
(define BG-COLOUR "black")

(struct point (x y) #:transparent)
(struct branch (start end) #:transparent)

(define (translate-point p length angle)
  (let ([a (degrees->radians angle)])
    (point (+ (point-x p) (* (cos a) length))
           (+ (point-y p) (* (sin a) length)))))

(define (random-adjust mean stddev factor)
  ;; Scale stddev by factor (0,1) before returning a random number
  (let ([stddev2 (* stddev factor)])
    (random-gaussian mean stddev2)))

(define (tree p length num-branches angle angle-between-branches randomness)
  ;; Return a list of branches representing a tree starting at point p
  ;; angle is the direction of the trunk 
  ;; randomness is 0 to 1, with 1 creating variation with stddev = mean
  (let* ([random-length (random-adjust length length randomness)]
         [random-angle (random-adjust angle angle-between-branches randomness)]
         [end-point (translate-point p random-length random-angle)])
    (list
     (branch p end-point)
     (if (> length 10)
         (for/list ([b (range num-branches)])
           (tree end-point (* length 0.6) num-branches
                 (+ angle (* (- (/ num-branches 2) b) angle-between-branches))
                 angle-between-branches randomness))
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

(define (draw-tree size num-branches angle-between-branches randomness)
  ;; Guess at width and height
  (let* ([width (* size 3)]
         [height (* size 3)])
    (scene+tree
     (tree
      (point (/ width 2) (- height 10))
      size
      num-branches
      270 ;; up
      angle-between-branches
      randomness
      )
     (empty-scene width height BG-COLOUR)
   )))

(draw-tree 100 3 60 0.2)
(draw-tree 75 4 20 0.3)