#lang racket

#|
Show notes, then play them, see if you get them right!
Press any key if you find the note easy, you'll then
get less time to play this note next time.

Challenge: How to avoid practicing mistakes or quick
corrections, wait for the player to remember first?

CHANGES from v3:
- Play easy notes less often
- Add drills to practice phrases of notes:
  (note, note+1 note+2)
  (note, note+2, note+4)

TODO:
- Show current note and the next one
- Add back in easy-note logic
- Add more drills to practice phrases of notes:
  (note, note-12) (one octave lower)
  (note, note-3, note-1, note-4...
- Sort easy-notes for better display, or show them on
  the stave?
- Save easy-notes for next time, so that the player
  doesn't need to edit the source code
- Fix display of extenders that should be hidden by
  stave lines
|#

(require srfi/1)
(require 2htdp/universe 2htdp/image)
(require 2htdp/image)
(require rsound)
(require rsound/piano-tones)
(require "util.rkt")
(require racket/trace)

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
(define EASY-NOTES
  '(e2 f2 g2 a3 g3 b4 c4 d4 e4 f4 g4))
;; How likely to skip easy notes (0-1)?
(define SKIP-EASY-NOTES 0.7)

;; The canvas
(define WIDTH 400)
(define HEIGHT 300)
(define G-CLEF (bitmap "GClef.png"))

;; How many seconds between notes? Change this to suit your needs
(define TICK-RATE 0.4)

(define PIX-PER-NOTE 11)
(define PIX-BETWEEN-LINES (* 2 PIX-PER-NOTE))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (note-index a-note)
  (list-index (curry equal? a-note) NOTES))

(define above/align-left
  ((curry above/align) "left"))

(define (stave)
  (overlay/offset
   (scale 0.53 G-CLEF)
   120 -6
  (overlay
   (apply above/align-left 
          (cons (line 300 0 "black")
                (times-repeat 4
                              (above/align-left
                               (line 0 20 "black")
                               (line 300 0 "black")
                               ))))
   (empty-scene WIDTH HEIGHT "white"))))

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
          (if (member a-note OPEN-STRINGS) "outline" "solid")
          "black"))

(define (note-on-stave-img a-note)
  (place-image/align
   (extenders (note-pos-relative-b4 a-note))
   (/ WIDTH 2) (/ HEIGHT 2) "middle" (if (extenders-above a-note) "bottom" "top")
   (overlay/offset
    (note-img a-note)
    0 (note-y-pos a-note)
    (empty-scene 0 HEIGHT))))

(define (note-phrase-img notes)
  ;; A sequence of notes including extenders
  (foldr (λ (n scene) (beside (note-on-stave-img n)
                              (empty-scene 15 0) scene))
         (empty-scene 15 0)
         notes))
    
(define (show-notes notes)
  (overlay
   (note-phrase-img notes)
   (stave)))

(define (show-note a-note)
  (show-notes (list a-note)))

(define (play-note a-note)
  (play (piano-tone 
         (list-ref MIDI-NOTES (note-index a-note)))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

;; The procs next-note-phrase-??? return a sequence of notes
;; that make some kind of pleasing phrase

(define (random-note-phrase-123)
  (random-note-phrase '(1 2 3)))

(define (random-note-phrase-135)
  (random-note-phrase '(1 3 5)))

(define (pick-notes deltas notes)
  (cond
    [(empty? deltas) '()]
    [else (cons (list-ref notes (- (car deltas) 1))
                (pick-notes (cdr deltas) notes))]))

(define (random-note-phrase deltas)
  (define first-note (next-random-note #f '()))
  (define notes (member first-note NOTES))
  (define max-delta (apply max deltas))
  (if (< (length notes) max-delta)
      (random-note-phrase deltas)
      (pick-notes deltas notes)))

;; Which type of note phrase to use?
(define (next-note-phrase)
  (random-note-phrase (random-choice '((1 2 3) (1 3 5) (1 5 3) (1 5 7)))))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; big-bang world

(struct world (note notes-left phrase plays easy-notes) #:transparent)

(define (next-note w)
  ;; Play the next note from the current phrase, repeat the phrase
  ;; or generate a new one.

  (cond
    [(empty? (world-notes-left w))
     ;; We've finished playing the phrase
     (cond
       [(= 1 (world-plays w))
        ;; Finished repeats, make a new phrase
        (let* ([phrase (next-note-phrase)]
               [plays 4])
          (next-note (world (car phrase) phrase phrase
                            plays (world-easy-notes w))))]
       [else
        ;; More repeats left, restart phrase
        (next-note (world #f (world-phrase w) (world-phrase w)
                          (sub1 (world-plays w)) (world-easy-notes w)))])]
    [else
     ;; We're still playing the phrase
     (play-note (car (world-notes-left w)))
     (world (car (world-notes-left w)) (cdr (world-notes-left w)) (world-phrase w)
            (world-plays w) (world-easy-notes w))]))

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
  (place-image/align
   (above/align "left"
    (text (string-append "Easy notes: "
                         (string-join (map symbol->string (world-easy-notes w)) ", "))
          15 "black")
    (text "Press any key to add current note" 15 "black"))
   5 5 "left" "top"
   (show-notes (world-phrase w))))

(define (go)
  (define phrase (next-note-phrase))
  (big-bang (world (car phrase) phrase phrase 4 EASY-NOTES)
            (on-tick next-note TICK-RATE)
            (on-key easy-note)
            (to-draw render-scene)))

;;(go)