#lang racket

;; Version 2, in 3D
;; Version 3, now with Aliens!

(require 2htdp/universe 2htdp/image)
(require "util.rkt")

;; Screen size
(define WIDTH 1000)
(define HEIGHT 600)

;; - to + of this value for new stars
(define MAX-STAR-XY 25000)

(define MAX-STARS 200)
(define MAX-ALIENS 10)
(define ALIEN-SIZE 80)

(define TICK-RATE 1/50)
(define ACCEL 1)
(define START-Z 100)

;; -----------------------------------------------------------

(struct starfield (stars aliens) #:transparent)
(struct pos (x y z) #:transparent)
(struct astar (pos) #:transparent)
(struct alien (pos speed colour) #:transparent)
  
(define (start-space)
  (big-bang (init-world)
            (on-tick fly TICK-RATE)
            (to-draw render-space)
            (stop-when end-flight)))

(define (init-world)
  (starfield (times-repeat MAX-STARS (new-star))
             (times-repeat MAX-ALIENS (new-alien))))

(define (screen-x p) (+ (/ (pos-x p) (pos-z p)) (/ WIDTH 2)))
(define (screen-y p) (+ (/ (pos-y p) (pos-z p)) (/ HEIGHT 2)))
(define (screen-size s p)
  ;; How big does s appear at pos p?
  (/ s (pos-z p)))
(define (random-star-xy) (- (random MAX-STAR-XY) (/ MAX-STAR-XY 2)))

(define (new-star)
  (astar (pos (random-star-xy)
              (random-star-xy)
              (+ (random START-Z) 10))))

(define (move-star s)
  (define p (astar-pos s))
  (astar (pos (pos-x p) (pos-y p) (- (pos-z p) ACCEL))))

(define (stars-in-view stars)
  (define (replace-star s)
    (if (star-out-of-view? s) (new-star) s))
  (map replace-star stars))

(define (star-out-of-view? s)
  (<= (pos-z (astar-pos s)) 1))

(define (new-alien)
  (alien (pos (random-range -100 100) (random-range -100 100) 50)
         (pos (random-range -10 10) (random-range -10 10) (/ (random-range -4 -1) 4.0))
         (color (random-range 100 255)
                             (random-range 100 255)
                             (random-range 100 255))))

(define (move-alien a)
  (define p (alien-pos a))
  (define s (alien-speed a))
  (define x (+ (pos-x p) (pos-x s)))
  (define y (+ (pos-y p) (pos-y s)))
  (define z (+ (pos-z p) (pos-z s)))
  (alien (pos x y z) s (alien-colour a)))

(define (aliens-in-view aliens)
  (define (replace-alien s)
    (if (alien-out-of-view? s) (new-alien) s))
  (map replace-alien aliens))

(define (alien-out-of-view? s)
  (<= (pos-z (alien-pos s)) 1))

(define (fly w)
  (starfield (map move-star (stars-in-view (starfield-stars w)))
             (map move-alien (aliens-in-view (starfield-aliens w)))))

;; -----------------------------------------------------------

(define (render-space w)
  (aliens+scene (starfield-aliens w) 
                (stars+scene (starfield-stars w) (empty-scene WIDTH HEIGHT "black"))))

(define (stars+scene stars scene)
  (foldl (λ (s scene)
           (place-image (circle (star-size s) "solid" (star-colour s))
                        (screen-x (astar-pos s))
                        (screen-y (astar-pos s))
                        scene))
         scene stars))

(define (star-size s)
  (define z (pos-z (astar-pos s)))
  (cond [(> z 75) 1]
        [else (+ 1 (/ (- 75 z) 20)) ]))

(define (star-colour s)
  (define z (pos-z (astar-pos s)))
  (cond [(> z 90) (color 255 255 255 20)]
        [else (color 255 255 255 (+ 20 (* 2 (- 90 z))))]))

(define (aliens+scene aliens scene)
  (foldl (λ (a scene)
           (place-image (alien-image a)
                        (screen-x (alien-pos a))
                        (screen-y (alien-pos a))
                        scene))
         scene aliens))

(define (alien-image a)
  (radial-star 12 (screen-size ALIEN-SIZE (alien-pos a))
               (screen-size (* ALIEN-SIZE 0.5) (alien-pos a))
               "solid" (alien-colour a)))

;; -----------------------------------------------------------

(define (end-flight w) #f)

;;(start-space)