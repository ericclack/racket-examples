#lang racket

#|
Draw a fractal tree

DONE:
- Have more than 2 branches
- Vary stem thickness
- Introduce some randomness

TODO:
- Get rid of constants and make a tree-maker function
  to create trees of a family
|#


(require 2htdp/image)
(require racket/trace)
(require "util.rkt")

(define BRANCH-COLOUR "white")
(define NUM-BRANCHES 4)
(define MIN-SIZE 10)
(define DS 0.7)
(define DA 30)

;; How likely we have a different num of branches?
(define RAND-BRANCHES 0.5)
;; And how many more / less?
(define RAND-BRANCHES-SCALE 1)
;; How likely we have different branch lengths?
(define RAND-SIZE 0.2)
;; And how much bigger / smaller?
(define RAND-SIZE-SCALE 0.2)

(define (randomise n scale likely)
  ;; increase or decrease n by up to scale randomly
  ;; depending on likely (0 = never, 1 = always)
  (if (< (random) likely)
      (let ([range (inexact->exact (floor (+ 1 (* 2 scale))))])
        (+ n (- scale (random range))))
      n))

(define (branches scene n x y size angle)
  ;; Draw n branches (trees) on scene
  (if (= n 0)
      scene
      (branches
       (scene+tree scene x y size angle)
       (sub1 n) x y size (+ angle DA))))
;;(trace branches)
  
(define (scene+tree scene x y size angle)
  ;; Return a new scene with a tree on it
  (if (< size MIN-SIZE)
      scene
      (let* ([a (degrees->radians angle)]
             [x2 (+ x (* (cos a) size))]
             [y2 (+ y (* (sin a) size))]
             [pen-width (floor (+ 1 (/ size 50)))]
             [a-pen (pen BRANCH-COLOUR pen-width "solid" "round" "round")]
             [next-size (randomise (* size DS) (* (* size DS) RAND-SIZE-SCALE) RAND-SIZE)]
             [num-branches (randomise NUM-BRANCHES RAND-BRANCHES-SCALE RAND-BRANCHES)]
             [next-angle (- angle (* DA (/ num-branches 2)))])
        (branches
         (scene+line scene x y x2 y2 a-pen)
          num-branches x2 y2 next-size next-angle))))

(define (tree size angle)
  (let* ([width (* size 5)]
         [height (* size 5)])
    (scene+tree
     (empty-scene width height "blue")
     (/ width 2) (- height 10)
     size angle
   )))

;; (tree (empty-scene 0 0 "black") 0 0 50 90)
;; (tree (empty-scene 200 200 "blue") 100 190 50 270)
;; (tree 50 90)