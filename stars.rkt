#lang racket

(require 2htdp/universe 2htdp/image)
(require "util.rkt")

(define WIDTH 600)
(define HEIGHT 600)

;; 0,0 is centre, so...
(define MAXX (/ WIDTH 2))
(define MAXY (/ HEIGHT 2))

(define MAX-STARS 100)
(define TICK-RATE 1/25)
(define ACCEL 1.05)

(struct starfield (stars) #:transparent)
(struct astar (x y) #:transparent)

(define (start-space)
  (big-bang (starfield (times-repeat MAX-STARS (new-star)))
            (on-tick fly TICK-RATE)
            (to-draw render-space)
            (stop-when end-flight)))

(define (screen-x x) (+ x MAXX))
(define (screen-y y) (+ y MAXY))

(define (new-star)
  ;; 0,0 is centre of screen
  (astar (- (random WIDTH) MAXX)
         (- (random HEIGHT) MAXY)))

(define (move-star s)
  (astar (* (astar-x s) ACCEL) (* (astar-y s) ACCEL)))

(define (stars-in-view stars)
  (define (replace-star s)
    (if (star-out-of-view? s) (new-star) s))
  (map replace-star stars))

(define (star-out-of-view? s)
  (or
   (> (astar-x s) MAXX)
   (< (astar-x s) (- 0 MAXX))
   (> (astar-y s) MAXY)
   (< (astar-y s) (- 0 MAXY))))

(define (fly w)
  (starfield (map move-star (stars-in-view (starfield-stars w)))))

(define (stars+scene stars scene)
  (foldl (λ (s scene)
           (place-image (circle 2 "solid" "black")
                        (screen-x (astar-x s))
                        (screen-y (astar-y s))
                        scene))
         scene stars))

(define (render-space w)
  (stars+scene (starfield-stars w) (empty-scene WIDTH HEIGHT)))

(define (end-flight w) #f)

(start-space)