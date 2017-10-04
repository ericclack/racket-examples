#lang racket/base

;; Screen size
(define WIDTH 1000)
(define HEIGHT 600)

;; A point in 3D space and a direction
(struct point (x y z) #:transparent)
(struct direction (inclination azimuth) #:transparent)
;; https://en.wikipedia.org/wiki/Spherical_coordinate_system

;; -----------------------------------------------------------

;; Translation to screen x and y co-ords
(define (screen-x p) (+ (/ (point-x p) (point-z p)) (/ WIDTH 2)))
(define (screen-y p) (+ (/ (point-y p) (point-z p)) (/ HEIGHT 2)))
(define (screen-size size p)
  ;; How big does s appear at pos p?
  ;; TODO: this should take into account distances x, y and z
  (if (> (point-z p) 0)
      (/ size (point-z p))
      0))

(define (point-distance p1 p2)
  (let ([dx (- (point-x p1) (point-x p2))]
        [dy (- (point-y p1) (point-y p2))]
        [dz (- (point-z p1) (point-z p2))])
    (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
;; http://math.stackexchange.com/questions/42640/calculate-distance-in-3d-space

;; -----------------------------------------------------------


(define (move-point p dir dist)
  ;; Return new xyz by moving a distance in a direction

  ;; x= r * sin azimuth * cos inclination
  ;; y= r * sin azimuth * sin inclination
  ;; z= r * cos azimuth
  
  (define inc (direction-inclination dir))
  (define az (direction-azimuth dir))
  
  (define dx (* dist (sin az) (cos inc)))
  (define dy (* dist (sin az) (sin inc)))
  (define dz (* dist (cos az)))
  
  (point (+ (point-x p) dx)
         (+ (point-y p) dy)
         (+ (point-z p) dz)))

(define (add-points point1 point2)
  (point (+ (point-x point1) (point-x point2))
         (+ (point-y point1) (point-y point2))
         (+ (point-z point1) (point-z point2))))

(define (change-direction dir dir2)
  (direction (+ (direction-inclination dir) (direction-inclination dir2))
             (+ (direction-azimuth dir) (direction-azimuth dir2))))

;; -----------------------------------------------------------

(provide WIDTH HEIGHT
         screen-x screen-y screen-size
         point-distance
         move-point add-points change-direction 
         (struct-out point)
         (struct-out direction))