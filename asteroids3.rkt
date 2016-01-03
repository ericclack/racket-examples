#lang racket

#|

DONE:
- Don't start asteroids on top of ship
- Bullet speed added to craft speed

TODO:
- Ship crashing into asteroids + lives
- Score
|#

(require 2htdp/universe 2htdp/image)
(require "util.rkt")

(struct world (asteroids ship bullets) #:transparent)
(struct pos (x y) #:transparent)
(struct ship (pos direction speed) #:transparent)
(struct asteroid (pos direction speed size) #:transparent)
(struct bullet (pos direction speed) #:transparent)

(define BIG-ASTEROID 60)
(define NUM-ASTEROIDS 5)
(define BULLET-SPEED 5)
(define SHIP-SIZE 30)

(define TICK-RATE 1/30)
(define WIDTH 800)
(define HEIGHT 600)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (new-asteroid)
  (asteroid (pos (random WIDTH) (random HEIGHT))
            (random 360) (+ 1 (random 2)) BIG-ASTEROID))

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
          (+ (ship-speed a-ship) BULLET-SPEED)))

(define (move-bullet b)
  (bullet (move-pos (bullet-pos b) (bullet-direction b) (bullet-speed b))
          (bullet-direction b)
          (bullet-speed b)))

(define (hit-asteroids asteroids bullets)
  ;; If any asteroids have been hit, split them in half.
  ;; Asteroids that are too small are deleted.
  
  ;; A list like this (a a a a a) will result in a list
  ;; like this (a a (a a) a a) on hit, we use flatten
  ;; to return the right thing.
 
  (define (hit-asteroid? a bullets)
    ;; Has this asteroid been hit by any of the bullets?
    (cond
      [(empty? bullets) #f]
      [(inside-circle (asteroid-pos a) (asteroid-size a)
                      (bullet-pos (car bullets))) #t]
      [else
       (hit-asteroid? a (cdr bullets))]))

  (define (split-asteroid a)
    (list (asteroid (asteroid-pos a) (- (asteroid-direction a) 90)
                    (asteroid-speed a) (/ (asteroid-size a) 2))
          (asteroid (asteroid-pos a) (+ (asteroid-direction a) 90)
                    (asteroid-speed a) (/ (asteroid-size a) 2))))

  (define (bullets-hit-asteroid a)
    (if (hit-asteroid? a bullets)
        (split-asteroid a)
        a))

  (define (big-enough a)
    (> (asteroid-size a) 5))
  
  (filter big-enough (flatten (map bullets-hit-asteroid asteroids))))


(define (live-bullets asteroids bullets)
  ;; Like hit-asteroids, but returns only bullets that
  ;; have not hit an asteroid

  (define (bullet-hit? b asteroids)
    (cond
      [(empty? asteroids) #f]
      [(inside-circle (asteroid-pos (car asteroids))
                      (asteroid-size (car asteroids))
                      (bullet-pos b)) #t]
      [else (bullet-hit? b (cdr asteroids))]))
  
  (define (bullet-hit-no-asteroids b)
    (not (bullet-hit? b asteroids)))

  (filter bullet-hit-no-asteroids bullets))

(define (move-ship a-ship)
  (ship (wrap-pos
         (move-pos (ship-pos a-ship) (ship-direction a-ship) (ship-speed a-ship))
         SHIP-SIZE)
        (ship-direction a-ship)
        (ship-speed a-ship)))
  
(define (next-world w)
  (define next-asteroids (hit-asteroids (world-asteroids w) (world-bullets w)))
  (define next-bullets (live-bullets (world-asteroids w) (world-bullets w)))
  
  (world (map move-asteroid next-asteroids)
         (move-ship (world-ship w))
         (filter bullet-in-range (map move-bullet next-bullets))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Rendering

(define (img+scene pos img scene)
  (place-image img (pos-x pos) (pos-y pos) scene))

(define (ship-img a-direction)
  (rotate (- 270 a-direction)
          (overlay/offset (triangle SHIP-SIZE "solid" "white") 0 8
                          (triangle SHIP-SIZE "solid" "white"))))

(define (ship+scene a-ship scene)
  (img+scene (ship-pos a-ship)
             (ship-img (ship-direction a-ship))
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
    (+ (ship-direction a-ship)
    (cond
      [(key=? a-key "left") -5]
      [(key=? a-key "right") 5]
      [else 0])))
  (define a-speed
    (+ (ship-speed a-ship)
       (cond
         [(key=? a-key "up") 1]
         [(key=? a-key "down") -1]
         [else 0])))
  (define bullets
    (cond
      [(key=? a-key " ") (cons (new-bullet a-ship) (world-bullets w))]
      [else (world-bullets w)]))

  (world (world-asteroids w)
         (ship (ship-pos a-ship) a-direction a-speed)
         bullets))

(define (ship-crashed? w)
  (define a-ship (world-ship w))
  (define (ship-hit-asteroids? asteroids)
    (cond
      [(empty? asteroids) #f]
      [(inside-circle (asteroid-pos (car asteroids))
                      (+ (asteroid-size (car asteroids))
                         (/ SHIP-SIZE 2))
                      (ship-pos a-ship)) #t]
      [else (ship-hit-asteroids? (cdr asteroids))]))

  (ship-hit-asteroids? (world-asteroids w)))

(define (new-world)
  ;; Produce a world in which the ship has not just crashed
  (define asteroids (times-repeat NUM-ASTEROIDS (new-asteroid)))
  (define a-ship (ship (pos (/ WIDTH 2) (/ HEIGHT 2)) 0 0))
  (define a-world
    (world asteroids a-ship '()))
  (if (ship-crashed? a-world)
      (new-world)
      a-world))
  
(define (go) 
  (big-bang (new-world)
            (on-tick next-world TICK-RATE)
            (on-key direct-ship)
            (to-draw render-world)
            (stop-when ship-crashed?)))
