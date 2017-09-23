#lang racket

(require rackunit)
(require/expose "asteroids6.rkt" (pos pos-x pos-y move-pos add-direction-speeds))

(define (check-equal-pos? pos1 pos2)
  (check-= (pos-x pos1) (pos-x pos2) 0.01)
  (check-= (pos-y pos1) (pos-y pos2) 0.01))

(test-case
 "move-pos tests"
 (check-equal-pos? (move-pos (pos 0 0) 90 5)
               (pos 0.0 5.0))
 (check-equal-pos? (move-pos (pos 0 0) 180 5)
               (pos -5.0 0.0))
 (check-equal-pos? (move-pos (pos 0 0) 270 5)
               (pos -0.0 -5.0))
 (check-equal-pos? (move-pos (pos 0 0) 0 5)
               (pos 5 0))
 )

(define (check-equal-list? list1 list2)
  (check-= (first list1) (first list2) 0.01)
  (check-= (second list1) (second list2) 0.01))

(test-case
 "tests for add-direction-speeds"
 (check-equal-list? (add-direction-speeds 0 0 90 5)
                    (list 90.0 5.0))
 (check-equal-list? (add-direction-speeds 0 0 180 5)
                    (list 180.0 5.0))
 (check-equal-list? (add-direction-speeds 0 0 270 5)
                    (list -90.0 5.0))
 (check-equal-list? (add-direction-speeds 0 0 0 5)
                    (list 0 5))
 
 (check-equal-list? (add-direction-speeds 0 0 45 5)
               (list 45.0 5.0))
 (check-equal-list? (add-direction-speeds 0 0 135 5)
               (list 135.0 5.0))
 (check-equal-list? (add-direction-speeds 0 0 225 5)
               (list -135 5.0))
 (check-equal-list? (add-direction-speeds 0 0 295 5)
               (list -65 5.0))

 ) 
