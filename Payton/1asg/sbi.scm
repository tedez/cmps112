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
;; Setting up hastables.  *foo* means 'foo' is a global var.
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

(define *label-table* (make-hash))
(define (label-get key)
	(hash-ref *label-table* key)
)
(define (label-put! key value)
	(hash-set! *label-table* key value)
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


; Gets handled in exec-line
(define (p-print printable)
	(printf "Should not be able to reach func-print here.~n")
)

(define (p-goto lbl)
	(printf "Should not be able to reach func-goto here.~n")
)

(define (p-if reel)
	(printf "Should not be able to reach func-if here.~n")
)

(define (p-let mem-ex)
	;(display mem-ex)
	;(newline)
	(when (pair? mem-ex)
		(let ((size (interp (cadr mem-ex)))
					(mem (car mem-ex))
				 )
			;(printf "car: ~a~n" mem)
			;(printf "cadr: ~a~n: " expr)
			(symbol-put! mem (make-vector (+ size 1)))
		)
	)
)

;; arrays are created w/: (dim (arry 10)))
;; and set with (let (arry i) 9))
(define (p-dim arry)
	; Vectors are as close to arrays as we can get,
	; can access vectors with indices
	(display arry)
	(newline)
	(when (pair? (car arry))
		(let* ((var-ex (car arry))
					 (expr (interp (cadr var-ex)))
					 (var (car var-ex))
				  )
			(symbol-put! var expr)
		)
	)
)

;; These functions were way too long to just be represented
;; by lambdas.
(for-each
	(lambda (pair)
		(func-put! (car pair) (cadr pair))
	)
	`(
		(dim		,p-dim)
		(let 		,p-let)
		;(goto 	,p-goto)
		(if 		,p-if)
		(print 	,p-print)
		;(input 	,func-input)
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

;; 
;; What category of object is this?
;;

(define (what-kind value)
    (cond ((real? value) 'real)
          ((vector? value) 'vector)
          ((procedure? value) 'procedure)
          (else 'other)))

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
					((list? cmd) (exec-line program (car cmd) line-num))
				)
			)
		)
	)
)

(define (exec-line program cmd line-num)
	(cond 
		((eq? (car cmd) 'print) 
			(when (not (null? (cdr cmd)))
				; Interpret each element to display.
				(for-each (lambda (x) (display (interp x))) (cdr cmd))
				(newline)
			)
			; Start over on the next line.
			(begin-exec program (+ line-num 1))
		)
		((eq? (car cmd) 'if)
			; I don't think I can do if before I do let.
			(print-list (cdr cmd))
		)
	  (else 
	  	;(printf "(cdr cmd): ~a~n" (cdr cmd))
	  	((func-get (car cmd)) (cdr cmd))
	  	(begin-exec program (+ line-num 1))
	  )   
  )
)

;; Does the proper lookups and formatting for each element
;; of the command.  This is done recursively so the element
;; is is checked from terminal -> nonterminal
(define (interp element)
	;(printf "element: ~a~n" element)
	(cond
		; Mackey explicitly said to make all numbers floats. 
		((number? element) (+ element 0.0))
		; Strings are only used in print statements.
		((string? element) element)
		; Is the element in our symbol-table?
		((hash-has-key? *symbol-table* element) (symbol-get element))
		; Using list? is discouraged.
		((pair? element) 
			(when (hash-has-key? *symbol-table* (car element))
				(let ((op (symbol-get(car element))))
					(cond 
						((procedure? op)
							;; map returns a list of the interpetted elements
							(apply op (map (lambda(e) (interp e)) (cdr element)))
						)
					)
				)
			)
		)
	)
)

(define (create-labels program)
	(for-each (lambda (line)
							(when (and (>= (length line) 2) (symbol? (cadr line)))
								; Add label to label-table.
								(let ((line-num (- (car line) 1)))
									(label-put! (cadr line) line-num)
								)
								;(printf "(cadr line): ~a~n" (cadr line))
							)
						)
						program
	)
)


(define (main arglist)
	;; If there's no args, or more than 1 arg, print usage.
	(if (or (null? arglist) (not (null? (cdr arglist))))
		(usage-exit)
		(let* ((sbprogfile (car arglist))
					 (program (readlist-from-inputfile sbprogfile))
					)
					;;(write-program-by-line sbprogfile program)
					(create-labels program)
					;(display *label-table*)
					;(newline)
					;(print-list program)
					(begin-exec program 0)
		)
	)
)


;; BEGIN
(main (vector->list (current-command-line-arguments)))

