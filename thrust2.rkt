#lang racket

#|

Thrust - (go) to run.

Left / right to rotate
Up to thrust
Space to fire.

TO DO:
- What's wrong with the little triangle and collisions?
- Refactor 2d stuff with vectors
- Scroll around as you fly


DONE:
- Asteroids bounce don't travel through triangles
- Gravity
- Better collision detection, not just centre of ship

|#

(require 2htdp/universe 2htdp/image lang/posn)
(require "util.rkt")
(require "2d.rkt")

;; Debug
(require racket/trace)

(struct world (landscape asteroids ship bullets score level) #:transparent)
(struct ship (pos facing-direction speed travel-direction) #:transparent)
(struct asteroid (pos direction speed size) #:transparent)
(struct bullet (pos direction speed) #:transparent)

;; Each level is a list of triangles that represent the landscape
(define LEVEL1 (list
                (list (pos 0 0) (pos 800 0) (pos 200 50))
                (list (pos 500 500) (pos 800 500) (pos 800 600))
                (list (pos 0 400) (pos 300 300) (pos 0 550))
                (list (pos 600 300) (pos 700 350) (pos 650 375))
                ))

(define LEVEL2 (list
                (list (pos 0 0) (pos 800 0) (pos 200 150))
                (list (pos 500 500) (pos 800 500) (pos 800 600))
                (list (pos 0 400) (pos 500 300) (pos 0 550))
                (list (pos 600 300) (pos 700 350) (pos 650 375))
                ))

(define BIG-ASTEROID 50)
(define NUM-ASTEROIDS 3)
(define BULLET-SPEED 5)
(define SHIP-SIZE 30)
(define MAX-BULLETS 15)
(define ASTEROID-IMG (bitmap "images/space-pizza.png"))
(define SPACESHIP-IMG (bitmap "images/spaceship2.png"))
(define GRAVITY-DIR 90)
(define GRAVITY-MAG .01)

(define TICK-RATE 1/30)
(define WIDTH 800)
(define HEIGHT 600)

(define KEY-STATE (make-hash))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

(define (new-asteroid)
  (asteroid (pos (random WIDTH) (random HEIGHT))
            (random 360) (+ 1 (random 2)) BIG-ASTEROID))

(define (bullet-in-range a-bullet)
  (define x (pos-x (bullet-pos a-bullet)))
  (define y (pos-y (bullet-pos a-bullet)))
  (and (> x 0) (< x WIDTH) (> y 0) (< y HEIGHT)))

(define (move-asteroid a landscape)
  (let* ([new-pos (wrap-pos
                   (move-pos (asteroid-pos a) (asteroid-direction a) (asteroid-speed a))
                   (asteroid-size a))]
         [new-pos-a-hit? (thing-hit-landscape? (list new-pos) landscape)])
    (if new-pos-a-hit?
        ;; bounce
        (asteroid (asteroid-pos a)
                  (- (asteroid-direction a) 180)
                  (asteroid-speed a)
                  (asteroid-size a))
        (asteroid new-pos
                  (asteroid-direction a)
                  (asteroid-speed a)
                  (asteroid-size a)))))

(define (new-bullet a-ship)
  (bullet (ship-pos a-ship)
          (ship-facing-direction a-ship)
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
      [(inside-circle? (asteroid-pos a) (asteroid-size a)
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


(define (live-bullets asteroids landscape bullets)
  ;; Like hit-asteroids, but returns only bullets that
  ;; have not hit an asteroid or the landscape

  (define (bullet-hit-asteroid? b asteroids)
    (cond
      [(empty? asteroids) #f]
      [(inside-circle? (asteroid-pos (car asteroids))
                      (asteroid-size (car asteroids))
                      (bullet-pos b)) #t]
      [else (bullet-hit-asteroid? b (cdr asteroids))]))
  (define (bullet-hit-landscape? b landscape)
    (cond
      [(empty? landscape) #f]
      [(inside-triangle? (car landscape) (bullet-pos b)) #t]
      [else (bullet-hit-landscape? b (cdr landscape))]))
  
  (define (bullet-hit-nothing? b)
    (not (or (bullet-hit-asteroid? b asteroids)
             (bullet-hit-landscape? b landscape))))

  (filter bullet-hit-nothing? bullets))

(define (move-ship a-ship)
  (ship (wrap-pos
         (move-pos (ship-pos a-ship) (ship-travel-direction a-ship) (ship-speed a-ship))
         SHIP-SIZE)
        (ship-facing-direction a-ship)
        (ship-speed a-ship)
        (ship-travel-direction a-ship)))
  
(define (next-world w)
  (move-world (direct-ship w)))

(define (move-world w)
  (define next-asteroids (hit-asteroids (world-asteroids w) (world-bullets w)))
  (define next-bullets (live-bullets (world-asteroids w) (world-landscape w)
                                     (world-bullets w)))
  (define add-score (asteroids-diff (world-asteroids w) next-asteroids))
  
  (world (world-landscape w)
         (map (λ (a) (move-asteroid a (world-landscape w))) next-asteroids)
         (move-ship (world-ship w))
         (filter bullet-in-range (map move-bullet next-bullets))
         (+ add-score (world-score w))
         (world-level w)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Rendering

(define (img+scene pos img scene)
  (place-image img (pos-x pos) (pos-y pos) scene))

(define (ship-img a-direction)
  (rotate (- 0 a-direction)
          (scale 2 SPACESHIP-IMG)))

(define (ship+scene a-ship scene)
  (img+scene (ship-pos a-ship)
             (ship-img (ship-facing-direction a-ship))
             scene))

(define (landscape+scene landscape scene)
  (foldl (λ (points scene)
           (scene+polygon scene
                          (pos->posn points) "solid" "gray"))
         scene landscape))

(define (asteroids+scene asteroids scene)
  (foldl (λ (a scene)
           (img+scene (asteroid-pos a)
                      (scale (/ (asteroid-size a) 11)
                             ASTEROID-IMG)
                      scene))
         scene asteroids))

(define (bullets+scene bullets scene)
  (foldl (λ (b scene)
           (img+scene (bullet-pos b)
                      (circle 2 "solid" "yellow")
                      scene))
         scene bullets))

(define (score+scene score level scene)
  (place-image
   (above/align "left"
    (text (string-append "Score: " (number->string score))
          24 "white")
    (text (string-append "Level: " (number->string level))
          24 "white"))
   55 35
   scene))

(define (render-world w)
  (score+scene (world-score w) (world-level w)
        (ship+scene (world-ship w)
             (asteroids+scene (world-asteroids w)
                       (bullets+scene (world-bullets w)
                               (landscape+scene (world-landscape w)
                                         (empty-scene WIDTH HEIGHT "black")))))))

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
  (let* ([a-ship (world-ship w)]
         [new-facing-direction (+ (ship-facing-direction a-ship)
                                  (cond
                                    [(key-pressed? "left") -5]
                                    [(key-pressed? "a") -5]
                                    [(key-pressed? "right") 5]
                                    [(key-pressed? "s") 5]
                                    [else 0]))]

         [new-direction-speed (add-direction-speeds
                               (ship-travel-direction a-ship)
                               (ship-speed a-ship)
                               new-facing-direction
                               (if (or (key-pressed? "up")
                                       (key-pressed? "t"))
                                   0.5 0))]
         [new-direction-speed-w-gravity (add-direction-speeds
                                         (first new-direction-speed)
                                         (second new-direction-speed)
                                         GRAVITY-DIR GRAVITY-MAG)]
         [bullets
          (cond
            [(and (key-pressed? " ")
                  (< (length (world-bullets w)) MAX-BULLETS)) 
             (cons (new-bullet a-ship) (world-bullets w))]
            [else (world-bullets w)])])
    (world (world-landscape w)
           (world-asteroids w)
           (ship (ship-pos a-ship) new-facing-direction
                 (second new-direction-speed-w-gravity)
                 (first new-direction-speed-w-gravity))
           bullets
           (world-score w)
           (world-level w))))

(define (ship-points a-ship)
  ;; The most important points that make up the ship, for collision detection
  (points-around-centre (ship-pos a-ship) (/ SHIP-SIZE 2) 30))

(define (thing-hit-landscape? thing-points landscape)
  (cond
    [(empty? landscape) #f]
    [(ormap (λ (p) (inside-triangle? (car landscape) p))
            thing-points) #t]
    [else (thing-hit-landscape? thing-points (cdr landscape))]))

(define (ship-crashed? w)
  (define a-ship (world-ship w))
  (define (ship-hit-asteroids? asteroids)
    (cond
      [(empty? asteroids) #f]
      [(inside-circle? (asteroid-pos (car asteroids))
                      (+ (asteroid-size (car asteroids))
                         (/ SHIP-SIZE 2))
                      (ship-pos a-ship)) #t]
      [else (ship-hit-asteroids? (cdr asteroids))]))

  (or (ship-hit-asteroids? (world-asteroids w))
      (thing-hit-landscape? (ship-points a-ship) (world-landscape w))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (new-world)
  ;; Produce a world in which the ship has not just crashed
  (define asteroids (times-repeat NUM-ASTEROIDS (new-asteroid)))
  (define a-ship (ship (pos 400 200) 0 0 0))
  (define a-world
    (world LEVEL1 asteroids a-ship '() 0 1))
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
