#lang racket

;; 3D forest game

;; Version 1

;; To Do:
;; - Adapt from stars.rkt
;; - How big does something look at position x,y,z? Not just about z
;; - Why do trees get smaller as they get closer?
;; - move-tree should be move-player
;; - We currently have space physics -- up/down shouldn't keep us running
;; - Turn left/right

(require 2htdp/universe 2htdp/image)
(require "util.rkt")
(require "3d.rkt")

(require unstable/debug)

;; - to + of this value for new stars
(define MAX-TREE-XY 500)

(define MAX-TREES 25)
(define MAX-ALIENS 10)
(define ALIEN-SIZE 150)

(define TICK-RATE 1/50)
(define START-Z 100)

;; User controllable speed
(define speed 0.1)
(define MAX-SPEED 2.9)

(define score 0)

(define (random-tree-xy) (- (random MAX-TREE-XY) (/ MAX-TREE-XY 2)))
(define (random-angle) (* (random) 2 pi))

;; -----------------------------------------------------------
;; The world and init

(struct forest (stars aliens) #:transparent)
(struct atree (pos) #:transparent)
(struct alien (pos direction dirchange speed colour) #:transparent)

  
(define (start-forest)
  (big-bang (init-world)
            (on-tick run TICK-RATE)
            (on-mouse mouse-event)
            (on-key key-event)
            (to-draw render-space)
            (stop-when end-game)))

(define (init-world)
  (forest (times-repeat MAX-TREES (new-tree))
             (times-repeat MAX-ALIENS (new-alien))))

;; -----------------------------------------------------------
;; Stars and Aliens

(define (new-tree)
  (atree (point (random-tree-xy)
              0
              (+ (random START-Z) 10))))

(define (move-tree s)
  (define p (atree-pos s))
  (atree (point (point-x p) (point-y p) (- (point-z p) speed))))

(define (trees-in-view stars)
  (define (replace-tree s)
    (if (tree-out-of-view? s) (new-tree) s))
  (map replace-tree stars))

(define (tree-out-of-view? s)
  (or 
   (<= (point-z (atree-pos s)) 1)
   (> (point-z (atree-pos s)) 200)))

;; ...........................................................

(define (new-alien)
  (define path (random-choice '(one two)))
  ;; direction is: inclination, azimuth
  ;; azimuth - 1.570 (pi/2) is left or right, no forward/backwards
  (define adirection
    (cond [(eq? path 'one) (direction 0 1.573)]
          [(eq? path 'two) (direction 1.573 1.571)]
          ))
  ;; Change in direction angles: inclination, azimuth
  (define achdirection
    (cond [(eq? path 'one) (direction (random-sign (/ (random) 10)) 0)]
          [(eq? path 'two) (direction (random-sign 0.09) 0.0002)]
          ))
  (define speed (+ 15 (random 15)))
  
  (alien
   (point 0 0 50) adirection achdirection speed
   (color (random-range 100 255)
          (random-range 100 255)
          (random-range 100 255))))

(define (move-alien a)
  (alien (add-points (move-point (alien-pos a) (alien-direction a) (alien-speed a))
                     (point 0 0 (- 0 speed)))
         (change-direction (alien-direction a) (alien-dirchange a))
         (alien-dirchange a)
         (alien-speed a)
         (alien-colour a)))

(define (aliens-in-view aliens)
  ;; Replace any aliens out of view with new ones
  (define (replace-alien s)
    (if (alien-out-of-view? s) (new-alien) s))
  (map replace-alien aliens))

(define (alien-out-of-view? s)
  (or (<= (point-z (alien-pos s)) 1)
      (> (point-z (alien-pos s)) 100)))

(define (run w)
  (forest (map move-tree (trees-in-view (forest-stars w)))
             (map move-alien (aliens-in-view (forest-aliens w)))))

;; -----------------------------------------------------------
;; Input events

(define (mouse-event w x y event)
  (define (kill-alien a)
    ;; If mouse pos x,y is inside the shape of the alien
    ;; then kill it, replacing it with a new one, otherwise leave it as it
    (define pos (alien-pos a))
    (define size (screen-size ALIEN-SIZE pos))
    (if (and (>= x (- (screen-x pos) size))
             (<= x (+ (screen-x pos) size))
             (>= y (- (screen-y pos) size))
             (<= y (+ (screen-y pos) size)))
        (begin
          (set! score (add1 score))
          (new-alien))
        a))
  
  (cond [(eq? event "button-down")
         (forest (forest-stars w)
                    (map kill-alien (forest-aliens w)))]
        [else w]))

(define (key-event w akey)
  (cond
    [(key=? akey "up")    (set! speed (min (+ speed 0.1) MAX-SPEED))]
    [(key=? akey "down")  (set! speed (max (- speed 0.1) (- 0 MAX-SPEED)))])
  w)

;; -----------------------------------------------------------
;; Rendering space, stars and aliens

(define (render-space w)
  (score+scene 
   (trees+scene (forest-stars w)
                (empty-scene WIDTH HEIGHT "black"))))

(define (score+scene scene)
  (place-image
   (text (format "score : ~s" score) 24 "white")
   60 20 
   scene))

(define (tree-img t)
  (above (circle (tree-size t) "solid" (tree-colour t))
         (rectangle (/ (tree-size t) 5) (tree-size t) "solid" (tree-colour t)))
  )

(define (trees+scene trees scene)
  ;; Place the stars on the scene
  (foldl (λ (s scene)
           (place-image (tree-img s)
                        (screen-x (atree-pos s))
                        (screen-y (atree-pos s))
                        scene))
         scene trees))

(define (tree-size t)
  (define d (abs (round (point-distance (atree-pos t) (point 0 0 0)))))
  (cond [(> d 100) 1]
        [else (+ 1 (/ (- 100 d) 4)) ]))

(define (tree-colour s)
  (define z (round (point-z (atree-pos s))))
  (cond [(> z 90) (color 255 255 255 20)]
        [else
         (define alph (min (+ 20 (* 2 (- 90 z)))
                           255))
         (color 255 255 255 alph)]))

;; -----------------------------------------------------------

(define (end-game w)
  ;; No end!
  #f)

(start-forest)