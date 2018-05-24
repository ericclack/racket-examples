#lang racket

(require rackunit)
(require "2d.rkt")

(test-case
 "tests pos to angles and distances"
 (check-= (angle-between (pos 100 100) (pos 100 200))
          90 0.01)
 (check-= (angle-between (pos 100 100) (pos 150 150))
          45 0.01)
 (check-= (angle-between (pos 0 0) (pos 50 50))
          45 0.01)
 (check-= (angle-between (pos 0 0) (pos 0 50))
          90 0.01)
 (check-= (angle-between (pos 0 0) (pos -50 -50))
          -135 0.01)

 (check-= (distance-between (pos 100 100) (pos 200 100))
          100 0.01)
 (check-= (distance-between (pos 0 0) (pos 100 100))
          141.42 0.01))

;; ---------------------------------------------------------

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

(test-case
 "tests for inside triangle"
 (check-true (inside-triangle? (list (pos 0 0) (pos 100 100) (pos 5 50))
                               (pos 50 50)))
 (check-false (inside-triangle? (list (pos 0 0) (pos 100 100) (pos 5 50))
                                (pos 51 50)))
 (check-true (inside-triangle? (list (pos 0 0) (pos 100 100) (pos 5 50))
                               (pos 2.5 25))))