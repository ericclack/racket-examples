#lang racket

(require rackunit "learn-music4.rkt")
(require "util.rkt")
(require/expose "learn-music4.rkt" (next-random-note-phrase NOTES))

(test-case
 "tests for next-random-note-phrase"
 (check-true (list? (next-random-note-phrase)))
 (times-repeat 100
               (let* ((phrase (next-random-note-phrase))
                     (len (length phrase))
                     (n1 (car phrase))
                     (last-note (last NOTES))
                     (second-last-note (car (take-right NOTES 2))))
                 (check-true (or (= len 3)
                                 (and (eq? n1 last-note) (= len 1))
                                 (and (eq? n1 second-last-note) (= len 2))))))
 )
                                      