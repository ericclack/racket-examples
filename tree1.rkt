#lang racket

#|
Draw a fractal tree

TODO:
- Make the branches join up!
|#


(require 2htdp/image)

(define BRANCH-COLOUR "white")
(define MIN-SIZE 5)
(define DS 0.7)
(define DA 30)

(define (tree size angle)
  (if (> size MIN-SIZE)
      (let* ([a (degrees->radians angle)]
             [x (* (cos a) size)]
             [y (* (sin a) size)])
        (above
         (beside
          (tree (* size DS) (- angle DA))
          (tree (* size DS) (+ angle DA)))
         (line x y BRANCH-COLOUR)))
      (empty-scene 0 0 "black")))

;; (tree 50 90)