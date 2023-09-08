#lang racket
; Katie LI
; Student ID:18003055

; Function to apply a list of functions to a given number and return a list of results
(define (apply-all functions number)
  ; Use the map function to apply each function in the list to the number
  (map (lambda (func) (func number))
       functions))

; List of functions to be applied
(define functions (list sqrt sqr (lambda (x) (expt x 3))))

; Number to which the functions will be applied
(define number 9)

; Display the list of functions
(display "Functions: ")
(display functions)
(newline)

; Display the number to which the functions will be applied
(display "Number: ")
(display number)
(newline)

; Apply the functions to the number and display the results
(display "Results: ")
(display (apply-all functions number))
(newline)
