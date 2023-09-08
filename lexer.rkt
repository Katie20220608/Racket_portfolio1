#lang racket
; Katie LI
; Student ID: 18003055

; Define a structure for tokens
(define-struct token
  (type attribute))

; Define constants for token types
(define token-types
  (list "number" "identifier" "keyword" "operator" "left-paren" "right-paren"))

; Helper function to check if a character is a digit
(define (is-digit? c)
  (char-numeric? c))

; Helper function to check if a character is a letter
(define (is-letter? c)
  (char-alphabetic? c))

; Helper function to check if a character is a special character (_,$,-)
(define (is-special-char? c)
  (or (char=? c #\_)(char=? c #\$)(char=? c #\-)))

; Helper function to convert a string to a number
(define (lex-number str)
  (if (regexp-match? #rx"\\.\\d" str)
      (string->number str)  ; Convert to floating-point number
      (string->number str))) ; Convert to integer

; Helper function to convert a string to an identifier symbol
(define (lex-identifier str)
  (string->symbol str))

; Helper function to convert a string to a keyword or an identifier symbol
(define (lex-keyword-or-identifier str)
  (if (is-keyword? str)
      (string->symbol str)  ; Convert to keyword symbol
      (string->symbol str))) ; Convert to identifier symbol

; Check if a string is a keyword
(define (is-keyword? str)
  (or (equal? str "let")
      (equal? str "print")))

; Lexer function that takes an input port and produces a list of tokens
(define (lex input)
  (define integer-tokens '()) ; List for integer tokens
  (define float-tokens '())   ; List for floating-point tokens
  (define tokens '())         ; List for all tokens
  
  ; Helper function to add a token to the list of tokens
  (define (add-token type attribute)
    (set! tokens (cons (make-token type attribute) tokens)))
  
  ; Helper function to consume a character from the input
  (define (consume-char)
    (read-char input))
  
  ; Helper function to peek at the next character in the input
  (define (peek-next-char)
    (peek-char input))
  
  ; Helper function to read characters while a condition is true
  (define (read-while pred)
    (let loop ((acc "")
               (char (peek-next-char)))
      (if (and (not (eof-object? char))
               (pred char))
          (begin
           (consume-char)
           (loop (string-append acc (string char)) (peek-next-char)))
        acc)))
  
  ; Main loop of the lexer
  (let loop ()
    (let ((char (peek-next-char)))
      (cond
       ((eof-object? char) (reverse tokens)) ; End of file, return the reversed list of tokens
       ((char-whitespace? char) (consume-char) (loop)) ; Whitespace, consume and continue
       ((is-digit? char)
        (let ((num-str (read-while (lambda (c) (or (is-digit? c) (char=? c #\.))))))
          (if (regexp-match? #rx"\\.\\d" num-str)
              (begin
                (set! float-tokens (cons (lex-number num-str) float-tokens))
                (set! float-tokens (sort float-tokens <))) ; Sort float tokens
              (begin
                (set! integer-tokens (cons (lex-number num-str) integer-tokens))
                (set! integer-tokens (sort integer-tokens <))) ; Sort integer tokens
          )
          (add-token "number" num-str) ; Add original number token to the list
          (loop)))
       ((is-letter? char)
        (let ((id-str (read-while (lambda (c) (or (is-letter? c) (is-digit? c) (is-special-char? c))))))
          (if (is-keyword? id-str)
              (add-token "keyword" (lex-keyword-or-identifier id-str)) ; Add keyword token
              (add-token "identifier" (lex-identifier id-str)) ; Add identifier token
          )
          (loop)))
       ((char=? char #\;)
        (consume-char) ; Ignore comments by consuming characters
        (loop))
       ((char=? char #\=)
        (add-token "operator" "=") ; Add operator token =
        (consume-char)
        (loop))
       ((char=? char #\*)
        (add-token "operator" "*") ; Add operator token *
        (consume-char)
        (loop))
       ((char=? char #\-)
        (let ((next-char (peek-char input 1))) ; Look ahead to the next character
          (if (or (is-digit? next-char) (char=? next-char #\.)) ; Check for digit or decimal point
              (begin
                (consume-char) ; Consume the '-' character
                (let ((num-str (read-while (lambda (c) (or (is-digit? c) (char=? c #\.)))))) ; Read the negative number
                  (add-token "number" (string-append "-" num-str)) ; Add negative number token
                  (loop)))
              (begin
                (add-token "operator" "-") ; Add operator token -
                (consume-char)
                (loop)))))
       ((char=? char #\^)
        (add-token "operator" "^") ; Add operator token ^
        (consume-char)
        (loop))
       ((char=? char #\.)
        (add-token "operator" ".") ; Add operator token .
        (consume-char)
        (loop))
       ((char=? char #\()
        (add-token "left-paren" "(") ; Add opening parenthesis token
        (consume-char)
        (loop))
       ((char=? char #\))
        (add-token "right-paren" ")") ; Add closing parenthesis token
        (consume-char)
        (loop))
       (else
        (error "Unexpected character: ~a" (string char)))))))

(define input-file "some-file.txt")
; Call the lexer and display the list of structured tokens
(call-with-input-file input-file
  (lambda (input-port)
    (define token-list (lex input-port))
    (for-each (lambda (token)
                (display (token-type token))
                (display ": ")
                (display (token-attribute token))
                (newline))
              token-list)))
