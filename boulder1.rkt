#lang racket

#|

Boulder Dash clone

DONE:

TODO:

|#

(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)
(require "util.rkt")

;; Debug
(require unstable/debug)
(require racket/trace)

(define WIDTH 16)
(define HEIGHT 16)
(define BLOCK-SIZE 50)
(define TICK-RATE 0.25)

(struct world (landscape fred level) #:transparent)
(struct pos (x y) #:transparent)
(struct fred (pos) #:transparent)
(define landscape (make-vector (* WIDTH HEIGHT)))

(define FRED-IMG (bitmap "images/smallface.gif"))
(define MUD-IMG (bitmap "images/mud.gif"))
(define WALL-IMG (bitmap "images/wall.gif"))
(define GEM-IMG (bitmap "images/gem.gif"))

(define (vec-ref x y)
  (+ (* y WIDTH) x))

(define (pos->px p)
  (+ (/ BLOCK-SIZE 2) (* p BLOCK-SIZE)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (next-world w)
  w)

(define (direct-fred w a-key)
  w)

(define (fred-dead w)
  #f)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Rendering

(define (img+scene pos img scene)
  (place-image img
               (pos->px (pos-x pos)) (pos->px (pos-y pos))
               scene))

(define (fred+scene a-fred scene)
  (img+scene (fred-pos a-fred) FRED-IMG
             scene))

(define (block->img a-block)
  (cond
    [(eq? a-block 'wall)
     WALL-IMG]
    [(eq? a-block 'mud)
     MUD-IMG]
    [(eq? a-block 'gem)
     GEM-IMG]
    [else (empty-scene BLOCK-SIZE BLOCK-SIZE "transparent")]))

(define (landscape-images a-landscape)
  (map block->img (vector->list a-landscape)))

(define (landscape-posns)
  (for*/list ([y (range HEIGHT)]
              [x (range WIDTH)])
    (make-posn (pos->px x) (pos->px y))
    ))
    
(define (landscape+scene a-landscape scene)
  (place-images (landscape-images a-landscape)
                (landscape-posns) scene))

(define (render-world w)
  (fred+scene (world-fred w)
              (landscape+scene (world-landscape w)
                               (empty-scene (* WIDTH BLOCK-SIZE)
                                            (* HEIGHT BLOCK-SIZE) "black"))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (make-landscape)
  (for*/vector ([y (range HEIGHT)]
                [x (range WIDTH)])
    (cond
      ;; Walls around the edges
      [(or (= x 0) (= x (- WIDTH 1))
           (= y 0) (= y (- HEIGHT 1)))
       'wall]
      ;; Fred's pos
      [(and (= x 1) (= y 1))
       0]
      [else 'mud])))


(define (go)
  (big-bang (world (make-landscape) (fred (pos 1 1)) 1)
            (on-tick next-world TICK-RATE)
            (on-key direct-fred)
            (to-draw render-world)
            (stop-when fred-dead)))
