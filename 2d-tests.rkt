#lang racket

(require rackunit)
(require "2d.rkt")

(test-case
 "tests for between"
 (check-true (between? 5 3 12))
 (check-true (between? 9 11 9)))

(test-case
 "tests for inside circle"
 (check-true (inside-circle? (pos 0 0) 10 (pos 7 7)))
 (check-true (inside-circle? (pos 0 0) 10 (pos -7 -7)))
 (check-false (inside-circle? (pos 0 0) 10 (pos 8 8))))

(test-case
 "tests for inside rectangle"
 (check-true (inside-rect? (pos 0 0) (pos 100 10) (pos 99 10)))
 (check-false (inside-rect? (pos 0 0) (pos 100 10) (pos 101 10)))
 (check-true (inside-rect? (pos 50 -10) (pos -50 0) (pos 0 -5)))
 (check-false (inside-rect? (pos 100 0) (pos 95 1000) (pos 101 100)))
 )

;;(test-case
;; "tests for inside triangle"
;; (check-true (inside-triangle? (pos 0 0) (pos 100 100) (pos 5 50) (pos 50 50))))