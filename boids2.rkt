#lang racket

#| Boids - bird like objects - https://en.wikipedia.org/wiki/Boids

Version 2: Boids don't like to get too close to the mouse.

Run (go) to start.

--

Copyright 2018, Eric Clack, eric@bn7.net
This program is distributed under the terms of the GNU General
Public License
|#

(require 2htdp/universe 2htdp/image)
(require "util.rkt")
(require "2d.rkt")

(struct world (boids mousex mousey) #:transparent)
(struct boid (pos direction speed size) #:transparent)

(define BIG-BOID 20)
(define NUM-BOIDS 15)

(define TICK-RATE 1/30)
(define WIDTH 800)
(define HEIGHT 600)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (new-boid)
  (boid (pos (random WIDTH) (random HEIGHT))
        (random 360)
        (+ 1 (random 2))
        BIG-BOID))

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
  
(define (move-boid a)
  ;; Move a boid based on its speed, direction and size
  (boid (wrap-pos
         (move-pos (boid-pos a) (boid-direction a) (boid-speed a))
         (boid-size a))
        (boid-direction a)
        (boid-speed a)
        (boid-size a)))

(define (distance-force d)
  ;; What force applied for distance d
  (define force-limit 100)
  (if (< d force-limit)
      (- (/ (- force-limit d)
            (* 2 force-limit)))
      0))

(define (avoid-mouse all-boids mousex mousey)
  ;; Adjust boid speed and direction to avoid collisions

  (define (apply-force a-boid)
    ;; The mouse position applies a force to this boid
    ;; based on distance and angle between each
    (define angle (angle-between (boid-pos a-boid)
                                 (pos mousex mousey)))
    (define mag (distance-force (distance-between (boid-pos a-boid)
                                                  (pos mousex mousey))))
    (define new-d-s (add-direction-speeds
                     (boid-direction a-boid) (boid-speed a-boid)
                     angle mag))
                     
    (boid (boid-pos a-boid)
          (first new-d-s)
          (second new-d-s)
          (boid-size a-boid)))
  
  (map apply-force all-boids))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (next-world w)
  (world (map move-boid
              (avoid-mouse (world-boids w)
                           (world-mousex w)
                           (world-mousey w)))
         (world-mousex w) (world-mousey w)))

(define (mouse-event w x y e)
  (world (world-boids w) x y))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Rendering

(define (img+scene pos img scene)
  (place-image img (pos-x pos) (pos-y pos) scene))

(define (boids+scene boids scene)
  (foldl (λ (a scene)
           (img+scene (boid-pos a)
                      (circle (boid-size a) "solid" "gray")
                      scene))
         scene boids))

(define (render-world w)
  (boids+scene (world-boids w)                
               (empty-scene WIDTH HEIGHT "black")))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (go)
  (big-bang (world (times-repeat NUM-BOIDS (new-boid)) 0 0)
            (on-tick next-world TICK-RATE)
            (on-mouse mouse-event)
            (to-draw render-world)))
