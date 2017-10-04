#lang racket

#|
Show notes, then play them, see if you get them right!
Press any key if you find the note easy, you'll then
get less time to play this note next time.

Challenge: How to avoid practicing mistakes or quick
corrections, wait for the player to remember first?

CHANGES from v1:
- Fix e2 ledger lines
- Bigger display, scalable with some variable

TODO:
- Drills
  E.g. (show-notes '(e3 g3 b4 d4 f4))
       (show-notes '(f3 a4 c4 e4))
- Reverse phrases randomly
- Save easy-notes for next time, so that the player
  doesn't need to edit the source code
- Fix display of ledgers
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
  '(e2 f2 g2 a3 b3 c3 d3 e3 f3 g3 a4 b4 c4 d4 e4 f4 g4 a5 b5 c5)) 
;; We need MIDI numbers to play them, these are the standard set
(define PIANO-MIDI-NOTES
  '(52 53 55 57 59 60 62 64 65 67 69 71 72 74 76 77 79 81 83 84)) 
;; Guitar midi notes are one octave lower
(define MIDI-NOTES
  (map (λ (x) (- x 12)) PIANO-MIDI-NOTES))

;; We want to show the open string notes differently
(define OPEN-STRINGS
  '(e2 a3 d3 g3 b4 e4))

;; Our phrases to practice, expressed in terms of the root
;; note 1, so (1 2 3) is the root note and the next two in
;; the scale
(define NOTE-PHRASES
  '((1 2 3) (1 3 5) (1 5 3) (1 5 7) (4 2 3 1) (1 8 5)))
;; The initial set of easy phrases for *me* to play - change this
;; to suit your needs
(define EASY-PHRASES
  '((a3 b3 c3) (e4 f4 g4) (a4 b4 c4) (e3 e4 b4) (e3 g3 b4) (g3 a4 b4) (d3 e3 f3)))
;; How likely to skip easy phrases (0-1)?
(define SKIP-EASY-PHRASES 0.7)

;; The canvas
(define WIDTH 500)
(define HEIGHT 400)
(define G-CLEF (bitmap "GClef.png"))
(define NOTE-SIZE 30)
(define STAVE-WIDTH 400)

;; How many seconds between notes? Change this to suit your needs
(define TICK-RATE 1)


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (note-index a-note)
  (list-index (curry equal? a-note) NOTES))

(define above/align-left
  ((curry above/align) "left"))

(define (stave)
  (define scaled-clef-height (* (* NOTE-SIZE 5) 1.35))
  (overlay/offset
   (scale (/ scaled-clef-height (image-height G-CLEF)) G-CLEF)
   140 (+ 1 (- 0 (/ NOTE-SIZE 3)))
  (overlay
   (apply above/align-left 
          (cons (line STAVE-WIDTH 0 "black")
                (times-repeat 4
                              (above/align-left
                               (line 0 (- NOTE-SIZE 1) "black")
                               (line STAVE-WIDTH 0 "black")
                               ))))
   (empty-scene WIDTH HEIGHT "white"))))

(define (note-pos-relative-b4 a-note)
  ;; b4 is the middle of the stave
  ;; b4 = 0, a4 = -1, c4 = 1, etc
  (define b4-pos (note-index 'b4))
  (- (note-index a-note) b4-pos))

(define (note-y-pos a-note)
  (* (/ NOTE-SIZE 2) (note-pos-relative-b4 a-note)))

(define (ledger-line)
  (line (* NOTE-SIZE 1.3) 0 "black"))

(define (ledger-lines a-note)
  (define num-lines
    (/ (- (abs (note-pos-relative-b4 a-note)) 5) 2))
  (define ledger-images
    (times-repeat num-lines (ledger-line)))
  (define ledger-lines-img
    (foldr (λ (i scene) (above i (empty-scene 0 NOTE-SIZE) scene))
          (empty-scene 0 0)
          ledger-images))

  (if (ledger-lines-above? a-note)
      ;; TODO: replace pix numbers with formulae
      (overlay/align/offset
       "middle" "bottom"
       ledger-lines-img
       0 (+ (/ HEIGHT 2) (* NOTE-SIZE 2))
       (empty-scene 0 HEIGHT))
      (overlay/align/offset
       "middle" "top"
       ledger-lines-img
       0 (- 0 (+ (/ HEIGHT 2) (* NOTE-SIZE 3))) 
       (empty-scene 0 HEIGHT))))
       
(define (ledger-lines-above? a-note)
  ;; Are the extenders above the stave (or below)?
  (>= (note-pos-relative-b4 a-note) 0))

(define (note-img a-note)
  ;; A note, with a hint for open strings
  (circle (/ NOTE-SIZE 2)
          "solid"
          (if (member a-note OPEN-STRINGS) "darkgreen" "black")))

(define (note+ledger-line-img a-note)
  (overlay
   (ledger-lines a-note)
   (overlay/offset
    (note-img a-note)
    0 (note-y-pos a-note)
    (empty-scene 0 HEIGHT))))

(define (note-phrase-img notes)
  ;; A sequence of notes including extenders
  (foldr (λ (n scene) (beside (note+ledger-line-img n)
                              (empty-scene (/ NOTE-SIZE 2) 0) scene))
         (empty-scene (/ NOTE-SIZE 2) 0)
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
               (< (random) SKIP-EASY-PHRASES)))
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
(define (next-note-phrase easy-phrases)
  (define phrase (random-note-phrase
                  (random-choice NOTE-PHRASES)))
  (if (and (member phrase easy-phrases)
           (< (random) SKIP-EASY-PHRASES))
      (next-note-phrase easy-phrases)
      phrase))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; big-bang world

(struct world (note notes-left phrase plays easy-phrases) #:transparent)

(define (next-note w)
  ;; Play the next note from the current phrase, repeat the phrase
  ;; or generate a new one.

  (cond
    [(empty? (world-notes-left w))
     ;; We've finished playing the phrase
     (cond
       [(= 1 (world-plays w))
        ;; Finished repeats, make a new phrase
        (let* ([phrase (next-note-phrase (world-easy-phrases w))]
               [plays 4])
          (next-note (world (car phrase) phrase phrase
                            plays (world-easy-phrases w))))]
       [else
        ;; More repeats left, restart phrase
        (next-note (world #f (world-phrase w) (world-phrase w)
                          (sub1 (world-plays w)) (world-easy-phrases w)))])]
    [else
     ;; We're still playing the phrase
     (play-note (car (world-notes-left w)))
     (world (car (world-notes-left w)) (cdr (world-notes-left w)) (world-phrase w)
            (world-plays w) (world-easy-phrases w))]))

(define (easy-phrase w a-key)
  ;; The user finds the current note easy - stop playing it
  ;; and add it to the set
  (let ((phrase (world-phrase w))
        (easy-phrases (world-easy-phrases w)))
    (world (world-note w) '() phrase 1
           (if (member phrase easy-phrases)
               easy-phrases
               (cons phrase easy-phrases)))))

(define (render-scene w)
  (place-image/align
   (above/align "left"
    (text (string-append "Easy phrases: "
                         (string-join (map (λ (x) (format "~a" x))
                                           (world-easy-phrases w)) ", "))
          15 "black")
    (text "Press any key to add current note" 15 "black"))
   5 5 "left" "top"
   (show-notes (world-phrase w))))

(define (go)
  (define phrase (next-note-phrase EASY-PHRASES))
  (big-bang (world (car phrase) phrase phrase 4 EASY-PHRASES)
            (on-tick next-note TICK-RATE)
            (on-key easy-phrase)
            (to-draw render-scene)))

(go)