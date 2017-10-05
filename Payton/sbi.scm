#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

;; ============
;; Setting up hastables.
(define *symbol-table* (make-hash))
(define (symbol-get key)
		(hash-ref *symbol-table* key)
)
(define (symbol-put! key value)
		(hash-set! *symbol-table* key value)
)

(define *function-table* (make-hash))
(define (func-get key)
		(hash-ref *function-table* key)
)
(define (func-put! key value)
		(hash-set! *function-table* key value)
)
(define (func-has? key)
		(hash-has-key? *function-table* key)
)

;; Init hashtables.
(for-each
		(lambda (pair)
						(symbol-put! (car pair) (cadr pair))
		)
		`(
				(log10_2 0.301029995663981195213738894724493026768189881)
				(sqrt_2  1.414213562373095048801688724209698078569671875)
				(e       2.718281828459045235360287471352662497757247093)
				(pi      3.141592653589793238462643383279502884197169399)
				(div     ,(lambda (x y) (floor (/ x y))))
				(log10   ,(lambda (x) (/ (log x) (log 10.0))))
				(mod     ,(lambda (x y) (- x (* (div x y) y))))
				(quot    ,(lambda (x y) (truncate (/ x y))))
				(rem     ,(lambda (x y) (- x (* (quot x y) y))))
				(<>      ,(lambda (x y) (not (= x y))))
				(+       ,+)
				(-       ,-)
				(*       ,*)
				(/       ,/)
				(=       ,=)
				(<       ,<)
				(<=      ,<=)
				(>       ,>)
				(>=      ,>=)
				(^       ,expt)
				(ceil    ,ceiling)
				(exp     ,exp)
				(floor   ,floor)
				(log     ,log)
				(sqrt    ,sqrt)
				(abs     ,abs)
				(sin     ,sin)
				(cos     ,cos)
				(tan     ,tan)
				(asin    ,asin)
				(acos    ,acos)
				(atan    ,atan)
				(round   ,round)
		 )
)

(for-each
	(lambda (pair)
		(func-put! (car pair) (cadr pair))
	)
	`(
			(dim		,func-dim)
			(let 		,func-let)
			(goto 	,func-goto)
			(if 		,func-if)
			(print 	,func-print)
			(input 	,func-input)
	 )
)

;; ===============
;; Provided funcs
(define *stderr* (current-error-port))

; run-file = name of THIS file.
(define *run-file*
		(let-values
				(((dirpath basepath root?)
						;; split-path returns 3 variables, which are bound to
						;; the 3 vars by the let-values procedure.
						;;  base, name, must-be-dir?
						(split-path (find-system-path 'run-file))))
				(path->string basepath))
)

(define (die list)
		(for-each (lambda (item) (display item *stderr*)) list)
		(newline *stderr*)
		(exit 1)
)

(define (usage-exit)
		(die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
		(let ((inputfile (open-input-file filename)))
				 (if (not (input-port? inputfile))
						 (die `(,*run-file* ": " ,filename ": open failed"))
						 (let ((program (read inputfile)))
									(close-input-port inputfile)
												 program))))

(define (write-program-by-line filename program)
		(printf "==================================================~n")
		(printf "~a: ~s~n" *run-file* filename)
		(printf "==================================================~n")
		(printf "(~n")
		(map (lambda (line) (printf "~s~n" line)) program)
		(printf ")~n")
)


;;=============
;; Utils

;; Used to give us the actual command portion of a line.
(define (trim line)
				;; If it's a 3 element line, return 3rd element (list)
	(cond ((= (length line) 3) (cddr line))
				;; If its a 2 element line && 2nd element (node) is list, return 2nd element (list)
				((and (= (length line) 2) (list? (cadr line))) (cdr line))
				;; Else, return first element (node)
				(else (car line))
	)
)


(define (print-list program)
		(let ((element (car program)))
			(display element)
			(newline)
		)
		(cond ((not (null? (cdr program)))
						(print-list (cdr program)))
		)
		
)

;;===============

;; Init executing the prog.
(define (begin-exec program line-num)
	(when (> (length program) line-num)
		(let ((line (list-ref program line-num)))
			(let ((cmd (trim line)))
				(cond 
					; Skip line if it just contains a number.
					((number? cmd) (begin-exec program (+ line-num 1)))
					; If it's a list (valid command) process the line.
					;((list? cmd) (exec-line program (car cmd) line-num))
				)
			)
		)
	)
)

;(define (exec-line program cmd line-num)

;)

;(define (create-labels program line-num)

;)


(define (main arglist)
	;; If there's no args, or more than 1 arg, print usage.
	(if (or (null? arglist) (not (null? (cdr arglist))))
		(usage-exit)
		(let* ((sbprogfile (car arglist))
					 (program (readlist-from-inputfile sbprogfile))
					)
					;;(write-program-by-line sbprogfile program)
					;(create-labels program)
					(begin-exec program 0)
		)
	)
)


;; BEGIN
(main (vector->list (current-command-line-arguments)))

