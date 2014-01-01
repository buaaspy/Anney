#lang eopl

;; env ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty-env
	(lambda () (list 'empty-env)))

(define extend-env
	(lambda (var val env)
 		(list 'extend-env var val env)))

(define apply-env
	(lambda (env search-var)
		(cond 
			[(eq? (car env) 'empty-env) (report-no-bindings-found 'x)]
			[(eq? (car env) 'extend-env) 
				(let ([saved-var (cadr env)]
				      [saved-val (caddr env)]
				      [saved-env (cadddr env)]) 
					(if (eq? search-var saved-var)
						saved-val
						(apply-env saved-env search-var)))]
			[else (report-invalid-env env)])))

(define report-no-bindings-found
	(lambda (search-var)
		(write "no binding found")))

(define report-invalid-env
	(lambda (env)
		(write "bad environment")))

;; scan&parse ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define scanner-let
	'((white-sp   (whitespace)                       skip)
	  (comment    ("%" (arbno (not #\newline)))      skip)
          (identifier (letter (arbno (or letter digit))) symbol)
          (number     (digit (arbno digit))              number)))

(define grammar-let
  '((program
     (expression)
     a-program)
    (expression
     (number)
     const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    (expression
     (identifier)
     var-exp)
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)))

(sllgen:make-define-datatypes scanner-let grammar-let)

(define list-the-datatypes
	(lambda ()
		(sllgen:list-define-datatypes scanner-let grammar-let)))

(define just-scan
	(sllgen:make-string-scanner scanner-let grammar-let))

(define scan&parse
	(sllgen:make-string-parser scanner-let grammar-let))

(define show-the-datatypes
  (lambda () (sllgen:show-define-datatypes scanner-let grammar-let)))

;; scarfolld  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?)))

(define expval->num
  (lambda (val)
    (case (expval val) 
      [(num-val num) num]
      [else (write ">> expected num ! <<")])))

(define expval-bool
  (lambda (val)
    (case (expval val)
      [(bool-val bool) bool]
      [else (write ">> expected bool <<")])))

;; interpreter;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define run 
  (lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
    (case (program pgm)
      [(a-program exp1) (value-of exp1 (init-env))])))

(define value-of
  (lambda (exp env)
    (case (expression exp)
      [(const-exp num) (num-val num)]
      [(var-exp var) (apply-env env var)]
      [(diff-exp exp1 exp2) 
       (let ([val1 (value-of exp1 env)]
             [val2 (value-of exp2 env)])
         (num-val (- num1 num2)))]
      [(zero?-exp exp1)
       (let ([val1 (value-of exp1 env)]) 
         (let ([num1 (expval->num val1)]) 
           (if (zero? num1)
               (bool-val #t)
               (bool-val $f))))]
      [(if-exp exp1 exp2 exp3) 
       (let ([val1 (value-of exp1 env)]) 
         (if (expval->bool val1) 
             (value-of exp2 env) 
             (value-of exp3 env)))]
      [(let-exp var exp1 body) 
       (let ([val1 (value-of exp1 env)]) 
         (value-of body (extend-env var val1 env)))])))

(write "-- Hello Anney ! --")

;; -- test lex -----------------------------------------------------------
(define lexer (sllgen:make-string-scanner scanner-let '()))
;(lexer "foo bar 10 % this is a comment")

;; -- test grammer -------------------------------------------------------
(define stmt1 "begin 
                   foo, bar := 1, 2; 
                   x := foo; 
                   while x do 
                     x := (x + 10)
               end")
;(scan&parse stmt1)