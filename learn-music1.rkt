#lang racket

#|
Show notes, then play them, see if you get them right!

TODO:
- Add extender bars above and below main stave for high and low notes
- Show a succession of notes
|#


(require srfi/1)
(require 2htdp/image)
(require rsound)
(require rsound/piano-tones)
(require "util.rkt")

(define NOTES
  '(e2 f2 g2 a3 b3 c3 d3 e3 f3 g3 a4 b4 c4 d4 e4 f4 g4 a5))

(define MIDI-NOTES
  '(52 53 55 57 59 60 62 64 65 67 69 71 72 74 76 77 79 81))

(define PIX-PER-NOTE 11)

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

(define (note-y-pos a-note)
  (* PIX-PER-NOTE (- (note-index a-note) 11)))

(define (show-note a-note)
  (overlay/offset
   (circle 10 "solid" "white")
   0 (note-y-pos a-note)
   (stave)))

(define (play-note a-note)
  (play (piano-tone 
         (list-ref MIDI-NOTES (note-index a-note)))))

(define (play-and-show-note a-note)
  (play-note a-note)
  (show-note a-note))

(play-and-show-note 'b4)
