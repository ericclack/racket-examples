#lang racket

#|

Boulder Dash clone

DONE:
- Add boulders and gems to landscape
- Make boulders fall

TODO:
- Pushing boulders
- Fred blanks out falling boulders sometimes
- Collect gems
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
;; A landscape is (make-vector (* WIDTH HEIGHT))
(struct pos (x y) #:transparent)
(struct fred (pos) #:transparent)
(struct block (what pos) #:transparent)
  
(define FRED-IMG (bitmap "images/smallface.gif"))
(define MUD-IMG (bitmap "images/mud.gif"))
(define BOULDER-IMG (bitmap "images/boulder.png"))
(define WALL-IMG (bitmap "images/wall.gif"))
(define GEM-IMG (bitmap "images/gem.gif"))

(define (block->img a-block)
  (cond
    [(eq? a-block 'mud) MUD-IMG]
    [(eq? a-block 'boulder) BOULDER-IMG]
    [(eq? a-block 'wall) WALL-IMG]
    [(eq? a-block 'gem) GEM-IMG]
    [else (empty-scene BLOCK-SIZE BLOCK-SIZE "transparent")]))

(define (vec-index a-pos)
  (+ (* (pos-y a-pos) WIDTH) (pos-x a-pos)))

(define (pos->px p)
  (+ (/ BLOCK-SIZE 2) (* p BLOCK-SIZE)))

(define (move-pos a-pos dx dy)
  (pos (+ (pos-x a-pos) dx)
       (+ (pos-y a-pos) dy)))

(define (what_is_next_to a-landscape a-pos dx dy)
  (vector-ref a-landscape (vec-index (move-pos a-pos dx dy))))

(define (what-is-below a-landscape a-pos)
  (what_is_next_to a-landscape a-pos 0 1))

(define (can-fall a-landscape a-block)
  (eq? (what-is-below a-landscape (block-pos a-block)) 0))

(define (landscape-filter a-landscape what)
  ;; Return a list of blocks that match 'what'
  ;; A block is a (what pos)
  (for*/list ([y (range HEIGHT)]
              [x (range WIDTH)]
              #:when (eq? (vector-ref a-landscape (vec-index (pos x y)))
                          what))
    (block what (pos x y))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (fred-can-move a-landscape a-fred dx dy)
  (member (what_is_next_to a-landscape (fred-pos a-fred) dx dy)
          '(0 mud gem)))

(define (move-fred a-landscape a-fred dx dy)
  (define p (fred-pos a-fred))
  (if (fred-can-move a-landscape a-fred dx dy)
      (fred (move-pos p dx dy))
      a-fred))

(define (random-block)
  ;; mud is more likely
  (random-choice '(mud mud mud mud mud mud mud
                       boulder boulder
                       wall gem)))

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
      [else (random-block)])))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Events

(define (clear-freds-block! a-landscape a-fred)
  (vector-set! a-landscape (vec-index (fred-pos a-fred)) 0))

(define (boulders-fall! a-landscape)
  (define boulders (landscape-filter a-landscape 'boulder))
  (for ([b (filter (curry can-fall a-landscape) boulders)])
    (vector-set! a-landscape (vec-index (block-pos b)) 0)
    (vector-set! a-landscape (vec-index (move-pos (block-pos b) 0 1))
                 (block-what b))
    ))

(define (next-world w)
  (boulders-fall! (world-landscape w))
  w)

(define (direct-fred w a-key)
  (define f (world-fred w))
  (define l (world-landscape w))
  (define newf
    (cond
      [(key=? a-key "left") (move-fred l f -1 0)]
      [(key=? a-key "right") (move-fred l f 1 0)]
      [(key=? a-key "up") (move-fred l f 0 -1)]
      [(key=? a-key "down") (move-fred l f 0 1)]
      [else f]))
  (clear-freds-block! (world-landscape w) newf)
  (world (world-landscape w) newf (world-level w)))

(define (fred-dead? w)
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

(define (go)
  (big-bang (world (make-landscape) (fred (pos 1 1)) 1)
            (on-tick next-world TICK-RATE)
            (on-key direct-fred)
            (to-draw render-world)
            (stop-when fred-dead?)))
