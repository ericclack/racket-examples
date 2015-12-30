#lang racket

#|

TODO:
- Shooting
- Splitting asteroids
- Score
- Moving ship
|#

(require 2htdp/universe 2htdp/image)
(require "util.rkt")

(struct world (asteroids ship) #:transparent)
(struct pos (x y) #:transparent)
(struct ship (pos direction speed) #:transparent)
(struct asteroid (pos direction speed size) #:transparent)

(define BIG-ASTEROID 60)
(define NUM-ASTEROIDS 10)

(define TICK-RATE 1/30)
(define WIDTH 800)
(define HEIGHT 600)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (new-asteroid)
  (asteroid (pos (random WIDTH) (random HEIGHT))
            (random 360) (+ 1 (random 2)) (random BIG-ASTEROID)))

(define (move-pos a-pos a-direction a-speed)
  (define r (degrees->radians a-direction))
  (pos (+ (pos-x a-pos) (* a-speed (cos r)))
       (+ (pos-y a-pos) (* a-speed (sin r)))))

(define (wrap-pos a-pos a-size)
  (define x (pos-x a-pos))
  (define y (pos-y a-pos))
  (pos (cond
         [(> x (+ WIDTH a-size)) (- 0 a-size)]
         [(< x (- 0 a-size)) (+ WIDTH a-size)]
         [else x])
       (cond
         [(> y (+ HEIGHT a-size)) (- 0 a-size)]
         [(< y (- 0 a-size)) (+ HEIGHT a-size)]
         [else y])))

(define (move-asteroid a)
  (asteroid (wrap-pos
             (move-pos (asteroid-pos a) (asteroid-direction a) (asteroid-speed a))
             (asteroid-size a))
            (asteroid-direction a)
            (asteroid-speed a)
            (asteroid-size a)))

(define (next-world w)
  (world (map move-asteroid (world-asteroids w))
         (world-ship w)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Rendering

(define (img+scene pos img scene)
  (place-image img (pos-x pos) (pos-y pos) scene))

(define (ship+scene a-ship scene)
  (img+scene (ship-pos a-ship)
             (rotate (ship-direction a-ship)
                     (triangle 40 "solid" "white"))
             scene))

(define (asteroids+scene asteroids scene)
  (foldl (λ (a scene)
           (img+scene (asteroid-pos a)
                      (circle (asteroid-size a) "solid" "gray")
                      scene))
         scene asteroids))

(define (render-world w)
  (ship+scene (world-ship w)
              (asteroids+scene (world-asteroids w)
                               (empty-scene WIDTH HEIGHT "black"))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (direct-ship w a-key)
  (define a-ship (world-ship w))
  (define a-direction
    (cond
      [(key=? a-key "left")
       (+ (ship-direction a-ship) 5)]
      [(key=? a-key "right")
       (- (ship-direction a-ship) 5)]))
  (world (world-asteroids w)
         (ship (ship-pos a-ship) a-direction (ship-speed a-ship))))

(define (go)
  (big-bang (world (times-repeat NUM-ASTEROIDS (new-asteroid))
                   (ship (pos (/ WIDTH 2) (/ HEIGHT 2)) 0 0))
            (on-tick next-world TICK-RATE)
            (on-key direct-ship)
            (to-draw render-world)))
