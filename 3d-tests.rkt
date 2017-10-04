#lang racket/base

(require rackunit "3d.rkt")

(test-case
 "tests for point distances"
 (check-equal? (point-distance (point 0 0 0) (point 0 0 0))
               0)
 (check-equal? (point-distance (point 3 4 0) (point 0 0 0))
               5)
 (check-equal? (point-distance (point 3 0 4) (point 0 0 0))
               5)
 (check-equal? (point-distance (point 3 3 3) (point 0 0 0))
               (sqrt 27))
 (check-equal? (point-distance (point 1 2 3) (point -2 -1 0))
               (sqrt 27))

)
