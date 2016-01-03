#lang racket

(require rackunit "learn-music-phrase1.rkt")
(require "util.rkt")
(require/expose "learn-music-phrase1.rkt" (NOTES next-note-phrase-123
                                          next-note-phrase-135
                                          note-index world))

(test-case
 "tests for next-note-phrase-123"
 (check-true (list? (next-note-phrase-123)))
 (times-repeat 100 (check-equal? (length (next-note-phrase-123)) 3))
 (let* ((phrase (next-note-phrase-123))
        (n1 (first phrase))
        (n2 (second phrase))
        (n3 (third phrase)))
   (check-true (and (= (note-index n1)
                       (- (note-index n2) 1)
                       (- (note-index n3) 2)))))
 )

(test-case
 "tests for next-note-phrase-135"
 (check-true (list? (next-note-phrase-135)))
 (times-repeat 100 (check-equal? (length (next-note-phrase-135)) 3))
 (let* ((phrase (next-note-phrase-135))
        (n1 (first phrase))
        (n2 (second phrase))
        (n3 (third phrase)))
   (check-true (and (= (note-index n1)
                       (- (note-index n2) 2)
                       (- (note-index n3) 4)))))
)

(test-case
 "tests for every-other"
 (check-equal? (every-other '(a b c d e f))
               '(a c e))
 (check-equal? (every-other '(a b c d e f g))
               '(a c e g))
 (check-equal? (every-other '()) '())
 (check-equal? (every-other '(a)) '(a)))