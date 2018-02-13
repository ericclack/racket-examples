#lang racket

#|

Asteroids - (go) to run.

Left / right to rotate
Up / down to speed up, slow down
Space to fire.

DONE:
- Multiple key presses, e.g. moving and shooting at the
  same time.
- Some motivaton for the user to do more than just fire
  in circles endlessely - maybe limitted bullets?

TODO:
- It's too hard to begin with
- Level up
- Can leave bullets just sitting there (reverse and fire)
- Lives

|#

(require 2htdp/universe 2htdp/image)
(require "util.rkt")

(struct world (asteroids ship bullets score) #:transparent)
(struct pos (x y) #:transparent)
(struct ship (pos direction speed) #:transparent)
(struct asteroid (pos direction speed size) #:transparent)
(struct bullet (pos direction speed) #:transparent)

(define BIG-ASTEROID 50)
(define NUM-ASTEROIDS 4)
(define BULLET-SPEED 5)
(define SHIP-SIZE 30)
(define MAX-BULLETS 25)

(define TICK-RATE 1/30)
(define WIDTH 800)
(define HEIGHT 600)

(define KEY-STATE (make-hash))

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

(define (asteroids-diff prev-asteroids next-asteroids)
  ;; +1 point each time the number of asteroids decreases
  ;; regardless of size
  (define diff (- (length prev-asteroids)
                  (length next-asteroids)))
  (if (> diff 0) diff 0))


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
  (move-world (direct-ship w)))

(define (move-world w)
  (define next-asteroids (hit-asteroids (world-asteroids w) (world-bullets w)))
  (define next-bullets (live-bullets (world-asteroids w) (world-bullets w)))
  (define add-score (asteroids-diff (world-asteroids w) next-asteroids))
  
  (world (map move-asteroid next-asteroids)
         (move-ship (world-ship w))
         (filter bullet-in-range (map move-bullet next-bullets))
         (+ add-score (world-score w))))

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

(define (score+scene score scene)
  (place-image (text (string-append "Score: "
                                    (number->string score))
                     24 "white") 55 20 scene))

(define (render-world w)
  (score+scene (world-score w)
               (ship+scene (world-ship w)
                           (asteroids+scene (world-asteroids w)
                                            (bullets+scene (world-bullets w)
                                                           (empty-scene WIDTH HEIGHT "black"))))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (key-down w a-key)
  (hash-set! KEY-STATE a-key #t)
  w)

(define (key-up w a-key)
  (hash-remove! KEY-STATE a-key)
  w)

(define (key-pressed? a-key)
  (hash-ref KEY-STATE a-key #f))

(define (direct-ship w)
  (define a-ship (world-ship w))
  (define a-direction
    (+ (ship-direction a-ship)
    (cond
      [(key-pressed? "left") -5]
      [(key-pressed? "right") 5]
      [else 0])))
  (define a-speed
    (+ (ship-speed a-ship)
       (cond
         [(key-pressed? "up") 1]
         [(key-pressed? "down") -1]
         [else 0])))
  (define bullets
    (cond
      [(and (key-pressed? " ")
            (< (length (world-bullets w)) MAX-BULLETS)) 
       (cons (new-bullet a-ship) (world-bullets w))]
      [else (world-bullets w)]))

  (world (world-asteroids w)
         (ship (ship-pos a-ship) a-direction a-speed)
         bullets
         (world-score w)))

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
    (world asteroids a-ship '() 0))
  (if (ship-crashed? a-world)
      (new-world)
      a-world))
  
(define (go)
  (hash-clear! KEY-STATE)
  (big-bang (new-world)
            (on-tick next-world TICK-RATE)
            (on-key key-down)
            (on-release key-up)
            (to-draw render-world)
            (stop-when ship-crashed?)))
