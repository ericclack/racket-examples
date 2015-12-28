#lang racket

#|
Show notes, then play them, see if you get them right!

TODO:
- Add extender bars above and below main stave for high and low notes
- Show a succession of notes
|#

(require racket/trace)
(require srfi/1)
(require 2htdp/image)
(require rsound)
(require rsound/piano-tones)
(require "util.rkt")

(define WIDTH 300)
(define HEIGHT 200)

(define NOTES
  '(e2 f2 g2 a3 b3 c3 d3 e3 f3 g3 a4 b4 c4 d4 e4 f4 g4 a5 b5 c5 d5))

(define MIDI-NOTES
  '(52 53 55 57 59 60 62 64 65 67 69 71 72 74 76 77 79 81 83 84 86))

(define PIX-PER-NOTE 11)
(define PIX-BETWEEN-LINES (* 2 PIX-PER-NOTE))

(define (note-index a-note)
  (list-index (curry equal? a-note) NOTES))

(define above/align-left
  ((curry above/align) "left"))

(define (stave)
  (apply above/align-left 
         (cons (line 300 0 "white")
               (times-repeat 4
                             (above/align-left
                              (line 0 20 "white")
                              (line 300 0 "white")
                              ))))
 )

(define (note-pos-relative-b4 a-note)
  ;; b4 is the middle of the stave
  ;; b4 = 0, a4 = -1, c4 = 1, etc
  (- (note-index a-note) 11))

(define (note-y-pos a-note)
  (* PIX-PER-NOTE (note-pos-relative-b4 a-note)))

(define (extender-line)
  (line 30 0 "white"))

(define (extenders a-note-pos)
  ;; Draw extenders from b4 up or down to note
  ;; the first few will be obscured by the 5 stave lines

  ;; Use absolute value of note pos:
  (if (< a-note-pos 0) (extenders (- 0 a-note-pos))
      (cond
        [(= a-note-pos 0) (extender-line)]
        ;; No lines at odd note positions
        [(odd? a-note-pos)
         (extenders (sub1 a-note-pos))]
        [(overlay/align/offset
          "left" "top"
          (extender-line)
          0 PIX-BETWEEN-LINES
          (extenders (sub1 a-note-pos)))])))

(define (extenders-above a-note)
  (>= (note-pos-relative-b4 a-note) 0))
      
(define (show-note a-note)
  (place-image/align
   (extenders (note-pos-relative-b4 a-note))
   150 (/ HEIGHT 2) "middle"
   (if (extenders-above a-note) "bottom" "top")
   (overlay/offset
    (circle 10 "solid" "white")
    0 (note-y-pos a-note)
    (overlay
     (stave) (empty-scene WIDTH HEIGHT "black")))))

(define (play-note a-note)
  (play (piano-tone 
         (list-ref MIDI-NOTES (note-index a-note)))))

(define (play-and-show-note a-note)
  (play-note a-note)
  (show-note a-note))

(show-note 'b4)
;;(play-and-show-note 'b4)
