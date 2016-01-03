#lang racket

(require rackunit "learn-music4.rkt")
(require "util.rkt")
(require/expose "learn-music4.rkt" (NOTES next-random-note-phrase note-index
                                          world))

(test-case
 "tests for next-random-note-phrase"
 (check-true (list? (next-random-note-phrase)))
 (times-repeat 100
               (let* ((phrase (next-random-note-phrase))
                      (len (length phrase)))
                 (check-equal? len 3)))
 (let* ((phrase (next-random-note-phrase))
        (n1 (first phrase))
        (n2 (second phrase))
        (n3 (third phrase)))
   (check-true (and (= (note-index n1)
                       (- (note-index n2) 1)
                       (- (note-index n3) 2)))))
 )

