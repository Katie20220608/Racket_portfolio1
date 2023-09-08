#lang racket
; Katie LI
; Student ID:18003055

(define (all-are predicate)
  (lambda (lst)
    (define (all-elements-satisfy lst)
      (cond
       ((null? lst) #t)                    ; Base case: Empty list satisfies the condition
       ((predicate (car lst))              ; Check if the first element satisfies the predicate
        (all-elements-satisfy (cdr lst)))  ; Recurse with the rest of the list
       (else #f)))                         ; If the first element doesn't satisfy, return #f
    (all-elements-satisfy lst)))

(define positive? (lambda (x) (> x 0)))
(define even? (lambda (x) (= (remainder x 2) 0)))

(display ((all-are positive?) '(1 2 3 -1))) ; Should output #f
(newline)
(display ((all-are even?) '(2 4 6 12)))   ; Should output #t
(newline)
(display ((all-are even?) '(2 4 6 9)))     ; Should output #f
(newline)
