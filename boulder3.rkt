#lang racket

#|

Boulder Dash clone

DONE:
- Boulder falling on fred ends game
- Reset level with r
- Levels get harder
- Dragons

TODO:
- Dragons - move more slowly
- Dragons - kill fred
- R to restart the same level, N for a new one
- Bombs?
- Save and load levels -- or use text encoding
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
(define DEAD-FRED-IMG (bitmap "images/deadfred.png"))
(define MUD-IMG (bitmap "images/mud.gif"))
(define BOULDER-IMG (bitmap "images/boulder.png"))
(define FALLING-BOULDER-IMG (bitmap "images/falling-boulder.png"))
(define WALL-IMG (bitmap "images/wall.gif"))
(define GEM-IMG (bitmap "images/gem.gif"))
(define DRAGON-IMG (bitmap "images/dragon.png"))

(define (blocksym->img a-symbol)
  (cond
    [(eq? a-symbol 'mud) MUD-IMG]
    [(eq? a-symbol 'boulder) BOULDER-IMG]
    [(eq? a-symbol 'falling-boulder) FALLING-BOULDER-IMG]
    [(eq? a-symbol 'wall) WALL-IMG]
    [(eq? a-symbol 'gem) GEM-IMG]
    [(eq? a-symbol 'dragon) DRAGON-IMG]
    [else (empty-scene BLOCK-SIZE BLOCK-SIZE "transparent")]))

(define (vec-index a-pos)
  (+ (* (pos-y a-pos) WIDTH) (pos-x a-pos)))

(define (get-blocksym a-landscape a-pos)
  (vector-ref a-landscape (vec-index a-pos)))

(define (get-block a-landscape a-pos)
  (block (get-block a-landscape a-pos) a-pos))

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

(define (what-is-next-to a-landscape a-pos dx dy)
  ;; What is at a-pos + dx/dy? Return symbol
  (get-blocksym a-landscape (move-pos a-pos dx dy)))

(define (what-is-below a-landscape a-pos)
  (what-is-next-to a-landscape a-pos 0 1))  

(define (block-next-to a-landscape a-pos dx dy)
  ;; Like what-is-next-to but return block
  (let ([d-pos (move-pos a-pos dx dy)])
    (block (get-blocksym a-landscape d-pos)
           d-pos)))

(define (can-fall? a-landscape a-block)
  (let ([this-block (block-what a-block)]
        [block-below (what-is-below a-landscape (block-pos a-block))])
    (if (eq? this-block 'falling-boulder)
        (member block-below '(0 fred))
        (eq? block-below 0))))

(define (is-boulder? a-block)
  (or (eq? (block-what a-block) 'boulder)
      (eq? (block-what a-block) 'falling-boulder)))

(define (is-gem? a-block) (eq? (block-what a-block) 'gem))
(define (is-dragon? a-block) (eq? (block-what a-block) 'dragon))
(define (is-empty? a-block) (eq? (block-what a-block) 0))

(define (landscape-filter a-landscape a-pred)
  ;; Return a list of blocks that match predicate
  ;; A block is a (what pos)
  (for*/list ([y (range HEIGHT)]
              [x (range WIDTH)]
              #:when (a-pred (block
                              (vector-ref a-landscape (vec-index (pos x y)))
                              (pos x y))))
    (block (vector-ref a-landscape (vec-index (pos x y)))
           (pos x y))))


(define (no-gems-left? a-landscape)
  (empty? (landscape-filter a-landscape is-gem?)))

(define (blanks-next-to a-landscape a-pos)
  ;; Return blanks poses next to this pos (N, E, S, W)
  (define bn block-next-to)
  (define blank-blocks
    (filter is-empty? (list 
           (bn a-landscape a-pos -1 0)
           (bn a-landscape a-pos 1 0)
           (bn a-landscape a-pos 0 -1)
           (bn a-landscape a-pos 0 1))
            ))
  (map block-pos blank-blocks))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (random-block level)
  ;; mud is more likely in lower levels
  (random-choice
   (append
    (times-repeat (+ 5 (- 10 (* 2 level))) 'mud)
    '(boulder boulder wall gem dragon))))

(define (make-landscape level)
  (for*/vector ([y (range HEIGHT)]
                [x (range WIDTH)])
    (cond
      ;; Walls around the edges
      [(or (= x 0) (= x (- WIDTH 1))
           (= y 0) (= y (- HEIGHT 1)))
       'wall]
      ;; Fred's pos
      [(and (= x 1) (= y 1))
       'fred]
      [else (random-block level)])))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Events & movement

(define (set-freds-block! a-landscape old-pos new-pos)
        (clear-block! a-landscape old-pos)
        (set-block! a-landscape (block 'fred new-pos)))

(define (boulders-fall! a-landscape)
  (define boulders (landscape-filter a-landscape is-boulder?))
  (for ([b boulders])
    (if (can-fall? a-landscape b)
        (let* ([cur-pos (block-pos b)]
               [new-pos (move-pos cur-pos 0 1)]
               [new-boulder (block 'falling-boulder new-pos)])
          (clear-block! a-landscape cur-pos)
          (set-block! a-landscape new-boulder))
        (set-block! a-landscape (block 'boulder (block-pos b))))
    ))

(define (dragons-move! a-landscape)
  (if (< (random) 0.1)
      (let ([dragons (landscape-filter a-landscape is-dragon?)])
        (for ([d dragons])
          (let* ([can-move (blanks-next-to a-landscape (block-pos d))]
                 [new-pos (random-choice can-move)])
            (if new-pos
                (begin
                  (clear-block! a-landscape (block-pos d))
                  (set-block! a-landscape (block 'dragon new-pos)))
                #f))))
      #f))
  

(define (next-world! w)
  (if (no-gems-left? (world-landscape w))
      (let ([next-level (add1 (world-level w))])
        (world (make-landscape next-level) (fred (pos 1 1)) next-level))
      (begin
        (boulders-fall! (world-landscape w))
        (dragons-move! (world-landscape w))
        w)))

(define (fred-can-move a-landscape a-fred dx dy)
  (member (what-is-next-to a-landscape (fred-pos a-fred) dx dy)
          '(0 mud gem)))

(define (fred-can-push-boulder a-landscape a-fred dx dy)
  (and (zero? dy)
       (eq? (what-is-next-to a-landscape (fred-pos a-fred) dx 0) 'boulder)
       (eq? (what-is-next-to a-landscape (fred-pos a-fred) (* dx 2) 0) 0)))

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

(define (direct-fred! w a-key)
  (define f (world-fred w))
  (define l (world-landscape w))
  (define newf
    (cond
      [(key=? a-key "left") (try-move-fred! l f -1 0)]
      [(key=? a-key "right") (try-move-fred! l f 1 0)]
      [(key=? a-key "up") (try-move-fred! l f 0 -1)]
      [(key=? a-key "down") (try-move-fred! l f 0 1)]
      [else f]))
  (if (key=? a-key "r")
      (world (make-landscape (world-level w)) (fred (pos 1 1)) (world-level w))
      (world (world-landscape w) newf (world-level w))))

(define (fred-dead? w)
  ;; Fred is dead if he's not at his location in the landscape
  ;; e.g. there's a boulder there
  (let ([fp (fred-pos (world-fred w))])
    (not (eq? (get-blocksym (world-landscape w) fp) 'fred))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Rendering

(define (img+scene pos img scene)
  (place-image img
               (pos->px (pos-x pos)) (pos->px (pos-y pos))
               scene))

(define (fred+scene a-fred scene)
  (img+scene (fred-pos a-fred) FRED-IMG scene))

(define (dead-fred+scene a-fred scene)
  (img+scene (fred-pos a-fred) DEAD-FRED-IMG scene))

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
  (define scene (landscape+scene (world-landscape w)
                                 (empty-scene (* WIDTH BLOCK-SIZE)
                                              (* HEIGHT BLOCK-SIZE) "black")))
  (if (fred-dead? w)
      (dead-fred+scene (world-fred w) scene)
      (fred+scene (world-fred w) scene)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (go)
  (big-bang (world (make-landscape 1) (fred (pos 1 1)) 1)
            (on-tick next-world! TICK-RATE)
            (on-key direct-fred!)
            (to-draw render-world)
            (stop-when fred-dead?)))
