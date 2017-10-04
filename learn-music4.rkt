#lang racket

#|
Show notes, then play them, see if you get them right!
Press any key if you find the note easy, you'll then
get less time to play this note next time.

Challenge: How to avoid practicing mistakes or quick
corrections, wait for the player to remember first?

DONE:
- Blue colouring for easy notes only considers default set
- Play easy-notes less often
- Show the note played, on the last play

TODO:
- Merge in updates from learn-music-phrases code
- Fix display of extenders that should be hidden by
  stave lines
- Sort easy-notes for better display, or show them on
  the stave?
- Save easy-notes for next time, so that the player
  doesn't need to edit the source code
|#

(require srfi/1)
(require 2htdp/universe 2htdp/image)
(require 2htdp/image)
(require rsound)
(require rsound/piano-tones)

;; What notes do we want to practice?
(define NOTES
  '(e2 f2 g2 a3 b3 c3 d3 e3 f3 g3 a4 b4 c4 d4 e4 f4 g4)) 
;; We need MIDI numbers to play them, these are the standard set
(define PIANO-MIDI-NOTES
  '(52 53 55 57 59 60 62 64 65 67 69 71 72 74 76 77 79)) 
;; Guitar midi notes are one octave lower
(define MIDI-NOTES
  (map (λ (x) (- x 12)) PIANO-MIDI-NOTES))

;; We want to show the open string notes differently
(define OPEN-STRINGS
  '(e2 a3 d3 g3 b4 e4))
;; The initial set of easy notes for *me* to play - change this
;; to suit your needs
;; When recalling note names: '(a3 c4 g4 e4 c3 a4 b4 f3)
(define EASY-NOTES
  '())
;;  '(a3 c4 g4 e4 c3 a4 b4 f3))
;; How likely to skip easy phrases (0-1)?
(define SKIP-EASY-NOTES 1)

;; The canvas
(define WIDTH 400)
(define HEIGHT 300)
(define G-CLEF (bitmap "GClef.png"))

;; How many seconds between notes? Change this to suit your needs
(define TICK-RATE 2)

(define PIX-PER-NOTE 11)
(define PIX-BETWEEN-LINES (* 2 PIX-PER-NOTE))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define-syntax-rule (times-repeat n fn)
  (for/list ([i (in-range n)])
    fn))

(define (random-choice list)
  (list-ref list (random (length list))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (note-index a-note)
  (list-index (curry equal? a-note) NOTES))

(define above/align-left
  ((curry above/align) "left"))

(define (stave)
  (apply above/align-left 
         (cons (line 300 0 "black")
               (times-repeat 4
                             (above/align-left
                              (line 0 20 "black")
                              (line 300 0 "black")
                              )))))

(define (note-pos-relative-b4 a-note)
  ;; b4 is the middle of the stave
  ;; b4 = 0, a4 = -1, c4 = 1, etc
  (- (note-index a-note) PIX-PER-NOTE))

(define (note-y-pos a-note)
  (* PIX-PER-NOTE (note-pos-relative-b4 a-note)))

(define (extender-line)
  (line 30 0 "black"))

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
  ;; Are the extenders above the stave (or below)?
  (>= (note-pos-relative-b4 a-note) 0))

(define (note-img a-note)
  ;; A note, with a hint for open strings
  (circle 10
          "solid"
          (if (member a-note OPEN-STRINGS) "darkgreen" "black")))

(define (show-note a-note)
  ;; Show the note on the stave with extenders and the G-Clef
  (overlay/offset
   (scale 0.53 G-CLEF)
   120 -6
   (place-image/align
    (extenders (note-pos-relative-b4 a-note))
    (/ WIDTH 2) (/ HEIGHT 2) "middle"
    (if (extenders-above a-note) "bottom" "top")
    (overlay/offset
     (note-img a-note)
     0 (note-y-pos a-note)
     (overlay
      (stave) (empty-scene WIDTH HEIGHT "white"))))))

(define (play-note a-note)
  (play (piano-tone 
         (list-ref MIDI-NOTES (note-index a-note)))))

(define (play-and-show-note a-note)
  (play-note a-note)
  (show-note a-note))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; big-bang world

(struct world (note plays easy-notes) #:transparent)

(define (next-random-note last-note easy-notes)
  ;; Next random note, but not last-note
  ;; also play easy-notes less often
  (define note (random-choice NOTES))
  (if (or (eq? note last-note)
          (and (member note easy-notes)
               (< (random) SKIP-EASY-NOTES)))
      (next-random-note last-note easy-notes)
      note))

(define (play-note-times a-note easy-notes)
  (if (member a-note easy-notes) 2 4))

(define (next-note w)
  ;; Play the next note, but first check if we've finished
  ;; playing this note. If we have, pick a new one.
  (cond
    [(zero? (world-plays w))
     (let* ((note (next-random-note (world-note w) (world-easy-notes w)))
           (plays (play-note-times note (world-easy-notes w))))
       (next-note (world note plays (world-easy-notes w))))]
    [else
     (play-note (world-note w))
     (world (world-note w) (sub1 (world-plays w)) (world-easy-notes w))]))

(define (easy-note w a-key)
  ;; The user finds the current note easy - stop playing it
  ;; and add it to the set
  (let ((note (world-note w))
        (easy-notes (world-easy-notes w)))
    (world note 0
           (if (member note easy-notes)
               easy-notes
               (cons note easy-notes)))))

(define (render-scene w)
  (define scene
    (place-image/align
     (above/align "left"
                  (text (string-append "Easy notes: "
                                       (string-join (map symbol->string (world-easy-notes w)) ", "))
                        15 "black")
                  (text "Press any key to add current note" 15 "black"))
     5 5 "left" "top"
     (show-note (world-note w))))
  (if (= 0 (world-plays w))
      (place-image/align
       (text (symbol->string (world-note w)) 50 "black")
       (- WIDTH 8) HEIGHT "right" "bottom" 
       scene)
      scene))

(define (go)
  (big-bang (world (random-choice NOTES) 0 EASY-NOTES)
            (on-tick next-note TICK-RATE)
            (on-key easy-note)
            (to-draw render-scene)))

(go)