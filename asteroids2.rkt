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

(struct world (asteroids ship bullets) #:transparent)
(struct pos (x y) #:transparent)
(struct ship (pos direction speed) #:transparent)
(struct asteroid (pos direction speed size) #:transparent)
(struct bullet (pos direction speed) #:transparent)

(define BIG-ASTEROID 60)
(define NUM-ASTEROIDS 10)
(define BULLET-SPEED 5)

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

(define (inside-circle circle-pos radius a-pos)
  (define distance
    (sqrt (+ (expt (- (pos-x a-pos) (pos-x circle-pos)) 2)
             (expt (- (pos-y a-pos) (pos-y circle-pos)) 2))))
  (<= distance radius))

(define (bullet-in-range a-bullet)
  (define x (pos-x (bullet-pos a-bullet)))
  (define y (pos-y (bullet-pos a-bullet)))
  (and (> x 0) (< x WIDTH) (> y 0) (< y HEIGHT)))

(define (move-asteroid a)
  (asteroid (wrap-pos
             (move-pos (asteroid-pos a) (asteroid-direction a) (asteroid-speed a))
             (asteroid-size a))
            (asteroid-direction a)
            (asteroid-speed a)
            (asteroid-size a)))

(define (new-bullet a-ship)
  (bullet (ship-pos a-ship)
          (ship-direction a-ship)
          BULLET-SPEED))

(define (move-bullet b)
  (bullet (move-pos (bullet-pos b) (bullet-direction b) (bullet-speed b))
          (bullet-direction b)
          (bullet-speed b)))

(define (hit-asteroids asteroids bullets)
  ;; If any asteroids have been hit, split them in half
  ;; A list like this (a a a a a) will result in a list
  ;; like this (a a (a a) a a)
 
  (define (hit-asteroid a bullets)
    ;; Has this asteroid been hit by any of the bullets?
    (cond
      [(empty? bullets) #f]
      [(inside-circle (asteroid-pos a) (asteroid-size a) (bullet-pos (car bullets)))
       #t]
      [else
       (hit-asteroid a (cdr bullets))]))

  (define (no-bullets-hit-asteroid a)
    (not (hit-asteroid a bullets)))
  
  (filter no-bullets-hit-asteroid asteroids))

(define (next-world w)
  (world (map move-asteroid
              (hit-asteroids (world-asteroids w) (world-bullets w)))
         (world-ship w)
         (filter bullet-in-range (map move-bullet (world-bullets w)))))

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

(define (bullets+scene bullets scene)
  (foldl (λ (b scene)
           (img+scene (bullet-pos b)
                      (circle 2 "solid" "yellow")
                      scene))
         scene bullets))

(define (render-world w)
  (ship+scene (world-ship w)
              (asteroids+scene (world-asteroids w)
                               (bullets+scene (world-bullets w)
                                              (empty-scene WIDTH HEIGHT "black")))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (direct-ship w a-key)
  (define a-ship (world-ship w))
  (define a-direction
    (cond
      [(key=? a-key "left")
       (+ (ship-direction a-ship) 5)]
      [(key=? a-key "right")
       (- (ship-direction a-ship) 5)]
      [else (ship-direction a-ship)]))
  (define bullets
    (cond
      [(key=? a-key " ") (cons (new-bullet a-ship) (world-bullets w))]
      [else (world-bullets w)]))

  (world (world-asteroids w)
         (ship (ship-pos a-ship) a-direction (ship-speed a-ship))
         bullets))

(define (go)
  (big-bang (world (times-repeat NUM-ASTEROIDS (new-asteroid))
                   (ship (pos (/ WIDTH 2) (/ HEIGHT 2)) 0 0)
                   '())
            (on-tick next-world TICK-RATE)
            (on-key direct-ship)
            (to-draw render-world)))
