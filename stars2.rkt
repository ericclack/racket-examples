#lang racket

;; Version 2, in 3D 

(require 2htdp/universe 2htdp/image)
(require "util.rkt")

;; Screen size
(define WIDTH 600)
(define HEIGHT 600)

;; - to + of this value for new stars
(define MAX-STAR-XY 25000)

(define MAX-STARS 100)
(define TICK-RATE 1/25)
(define ACCEL 1)
(define START-Z 100)

;; -----------------------------------------------------------

(struct starfield (stars) #:transparent)
(struct astar (x y z) #:transparent)

(define (start-space)
  (big-bang (starfield (times-repeat MAX-STARS (new-star)))
            (on-tick fly TICK-RATE)
            (to-draw render-space)
            (stop-when end-flight)))

(define (screen-x s) (+ (/ (astar-x s) (astar-z s)) (/ WIDTH 2)))
(define (screen-y s) (+ (/ (astar-y s) (astar-z s)) (/ HEIGHT 2)))
(define (random-star-xy) (- (random MAX-STAR-XY) (/ MAX-STAR-XY 2)))

(define (new-star)
  (astar (random-star-xy)
         (random-star-xy)
         (+ (random START-Z) 10)))

(define (move-star s)
  (astar (astar-x s) (astar-y s) (- (astar-z s) ACCEL)))

(define (stars-in-view stars)
  (define (replace-star s)
    (if (star-out-of-view? s) (new-star) s))
  (map replace-star stars))

(define (star-out-of-view? s)
  (<= (astar-z s) 1))

(define (fly w)
  (starfield (map move-star (stars-in-view (starfield-stars w)))))

;; -----------------------------------------------------------

(define (render-space w)
  (stars+scene (starfield-stars w) (empty-scene WIDTH HEIGHT "black")))

(define (stars+scene stars scene)
  (foldl (λ (s scene)
           (place-image (circle (star-size s) "solid" (star-colour s))
                        (screen-x s)
                        (screen-y s)
                        scene))
         scene stars))

(define (star-size s)
  (define z (astar-z s))
  (cond [(> z 75) 1]
        [else (+ 1 (/ (- 75 z) 20)) ]))

(define (star-colour s)
  (define z (astar-z s))
  (cond [(> z 90) (color 255 255 255 20)]
        [else (color 255 255 255 (+ 20 (* 2 (- 90 z))))]))

(define (end-flight w) #f)

;;(start-space)