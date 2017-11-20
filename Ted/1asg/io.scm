#!/usr/bin/racket
#lang racket

(define filename "./sbir-files/00-hello-world.sbir")
(define in_port (open-input-file filename))
;(define lines (file->lines "./sbir-files/00-hello-world.sbir"))

; Checking if an object is in a list.
(define list_position
	(lambda (object in_list)
		(let loop ((i 0) (in_list in_list)) ; Let has local scope so I
			(if (null? in_list) #f            ; have to say in_list = in_list
					(if (eqv? (car in_list) object) i
							(loop (+ i 1) (cdr in_list)))))))

; Reads a single line
;(read-line read_in)

;; Hopefully this will be my function to go through the list
;; of lines, and decide what each line is.
(define (parse_list lst)
	(when (not (null? lst))
		(display (car lst))
		(newline)
		(parse_list (cdr lst))))

; raw-data is an "s-expression" - very unfortunate name
; since it is just a data struc.
(define raw-data (read in_port))
(close-input-port in_port)
;(display (car raw-data))
(parse_list raw-data)