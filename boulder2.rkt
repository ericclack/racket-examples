#lang racket

#|

Boulder Dash clone

DONE:
- Add boulders and gems to landscape
- Make boulders fall
- Store 'fred in landscape so that falling boulders works
- Pushing boulders
- Fred blanks out falling boulders sometimes
- Collect gems

TODO:
- Boulder falling on fred ends game
- Reset level with r
- Aliens?
- Bombs?
- Make levels harder as you progress
- Save and load levels -- or use text encoding
- Still inconsistent use of block for struct and symbol
|#

(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)
(require "util.rkt")

;; Debug
;;(require unstable/debug)
;;(require racket/trace)

(define WIDTH 16)
(define HEIGHT 16)
(define BLOCK-SIZE 50)
(define TICK-RATE 0.1)

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

(define (blocksym->img a-symbol)
  (cond
    [(eq? a-symbol 'mud) MUD-IMG]
    [(eq? a-symbol 'boulder) BOULDER-IMG]
    [(eq? a-symbol 'wall) WALL-IMG]
    [(eq? a-symbol 'gem) GEM-IMG]
    [else (empty-scene BLOCK-SIZE BLOCK-SIZE "transparent")]))

(define (vec-index a-pos)
  (+ (* (pos-y a-pos) WIDTH) (pos-x a-pos)))

(define (set-block! a-landscape a-block)
  (vector-set! a-landscape (vec-index (block-pos a-block))
               (block-what a-block)))

(define (clear-block! a-landscape a-pos)
  (set-block! a-landscape (block 0 a-pos)))

(define (pos->px p)
  (+ (/ BLOCK-SIZE 2) (* p BLOCK-SIZE)))

(define (move-pos a-pos dx dy)
  (pos (+ (pos-x a-pos) dx)
       (+ (pos-y a-pos) dy)))

(define (what_is_next_to a-landscape a-pos dx dy)
  ;; What is at a-pos + dx/dy?
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

(define (no-gems-left? a-landscape)
  (empty? (landscape-filter a-landscape 'gem)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
;; Events & movement

(define (set-freds-block! a-landscape old-pos new-pos)
        (clear-block! a-landscape old-pos)
        (set-block! a-landscape (block 'fred new-pos)))

(define (boulders-fall! a-landscape)
  (define boulders (landscape-filter a-landscape 'boulder))
  (for ([b (filter (curry can-fall a-landscape) boulders)])
    (let* ([cur-pos (block-pos b)]
          [new-pos (move-pos cur-pos 0 1)]
          [new-boulder (block 'boulder new-pos)])
      (clear-block! a-landscape cur-pos)
      (set-block! a-landscape new-boulder)
    )))

(define (next-world w)
  (if (no-gems-left? (world-landscape w))
      ;; Next level
      (world (make-landscape) (fred (pos 1 1)) (add1 (world-level w)))
      (begin
        (boulders-fall! (world-landscape w))
        w)))

(define (fred-can-move a-landscape a-fred dx dy)
  (member (what_is_next_to a-landscape (fred-pos a-fred) dx dy)
          '(0 mud gem)))

(define (fred-can-push-boulder a-landscape a-fred dx dy)
  (and (zero? dy)
       (eq? (what_is_next_to a-landscape (fred-pos a-fred) dx 0) 'boulder)
       (eq? (what_is_next_to a-landscape (fred-pos a-fred) (* dx 2) 0) 0)))

(define (try-move-fred! a-landscape a-fred dx dy)
  ;; Fred can move if there's mud, gem or empty space.
  ;; Change the landscape (fred's pos) and maybe a boulder
  ;; if fred pushes it
  (let* ([cur-pos (fred-pos a-fred)]
        [new-pos (move-pos cur-pos dx dy)])
    (if (fred-can-move a-landscape a-fred dx dy)
      (begin
        (set-freds-block! a-landscape cur-pos new-pos)
        (fred new-pos))
      (if (fred-can-push-boulder a-landscape a-fred dx dy)
          (begin
            (clear-block! a-landscape new-pos)
            (set-block! a-landscape (block 'boulder (move-pos new-pos dx dy)))
            (set-freds-block! a-landscape cur-pos new-pos)
            (fred new-pos))
          a-fred))))

(define (direct-fred w a-key)
  (define f (world-fred w))
  (define l (world-landscape w))
  (define newf
    (cond
      [(key=? a-key "left") (try-move-fred! l f -1 0)]
      [(key=? a-key "right") (try-move-fred! l f 1 0)]
      [(key=? a-key "up") (try-move-fred! l f 0 -1)]
      [(key=? a-key "down") (try-move-fred! l f 0 1)]
      [else f]))
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
  (map blocksym->img (vector->list a-landscape)))

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
