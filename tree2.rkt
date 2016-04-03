#lang racket

#|
Draw a fractal tree

DONE:
- Make the branches join up!

TODO:
- Have more than 2 branches
- Vary stem thickness
- Introduce some randomness
|#


(require 2htdp/image)
(require racket/trace)

(define BRANCH-COLOUR "white")
(define MIN-SIZE 3)
(define DS 0.6)
(define DA 30)

(define (tree scene x y size angle)
  ;; Return a new scene with a tree on it
  (if (< size MIN-SIZE)
      scene
      (let* ([a (degrees->radians angle)]
             [x2 (+ x (* (cos a) size))]
             [y2 (+ y (* (sin a) size))]
             [next-size (* size DS)]
             [next-angle-1 (+ angle DA)]
             [next-angle-2 (- angle DA)])
        (tree
         (tree 
          (add-line scene x y x2 y2 BRANCH-COLOUR)
          x2 y2 next-size next-angle-1)
         x2 y2 next-size next-angle-2))))

;; (tree (empty-scene 0 0 "black") 0 0 50 90)
;; (tree (empty-scene 200 200 "blue") 100 190 50 270)