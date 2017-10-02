#!/usr/bin/racket
#lang racket

(display "Hello, World!")
(newline)
(define var1 5)
(display var1)
(newline)
(define (two-times x)
  (+ x x)
)
(display (two-times 5))
(newline)
(define var2 2)
(define (min a b)
  (if (< a b)
      a
      b))
(display (min var1 var2))
(newline)
(cons 1 #t)
'(1 . #t)
;(1 . #t) ; ERROR
;(define (area length width)
					;(* length width))

;(define area *)

(define area
  (lambda (length width)
    (* length width)))
(area 2 3)
; 'apply' lets us call a procedure on a list of stuff
(define x '(1 2 3))
(display "Using 'apply': ")
(apply + x)
(newline)
(set! x 20) ; Now x is a global var = 20
; 'let' creates a list of local vars to use
; within its body
(let ((x 1) ; Notice how I've redefined x here.
      (y 2)
      (z 3))
  (begin
    (display "Using 'let': ")
    (list x y z)))
(newline)
; Learning how to define loops with local scope
(display "letrec ex: ")
(newline)
;(letrec ((countdown (lambda (i)
					;(if (= i 0) 'Liftoff
					;(begin
					;(display i)
					;(newline)
					;(countdown (- i 1)))))))
					;(countdown 10))

; More compact version of above 'let' using 'named let'
(let countdown ((i 10))
  (if (= i 0) 'Liftoff
      (begin
	(display i)
	(newline)
	(countdown (- i 1))))) ; the "tail call"
(newline)
; Checking if an object is in a list.
(define list-position
  (lambda (object in_list)
    (let loop ((i 0) (in_list in_list)) ; Let has local scope so I
      (if (null? in_list) #f            ; have to say in_list = in_list
	  (if (eqv? (car in_list) object) i
	      (loop (+ i 1) (cdr in_list)))))))
; Applies a procedure to 
(for-each display
  (list "one " "two " "buckle my shoe"))
(newline)
