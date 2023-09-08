#lang racket
; Katie LI
; Student ID:18003055


; Function to compute the value of a single term in a polynomial
(define (compute-poly-term coefficient exponent)
  (* coefficient (expt x exponent)))

; Function to compute the value of a polynomial given coefficients and x
(define (compute-poly sum coefficients exponent)
  (if (null? coefficients)
      sum
      (compute-poly (+ sum (compute-poly-term (car coefficients) exponent))
                    (cdr coefficients)
                    (+ exponent 1))))

; List of coefficients representing the polynomial coefficients
(define coefficients '(3 4 5))

; Value of x for polynomial evaluation
(define x 4)

; Display the list of polynomial coefficients
(display "Polynomial Coefficients: ")
(display coefficients)
(newline)

; Display the value of x
(display "Value of x: ")
(display x)
(newline)

; Calculate and display the result of polynomial evaluation
(display "Result of Polynomial Evaluation: ")
(display (compute-poly 0 coefficients 0)) ; Starting exponent is 0
(newline)
