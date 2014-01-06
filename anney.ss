#lang eopl

;; env ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (saved-sym saved-val saved-env)
    (cond
      ((not (symbol? saved-sym))
       (write ">> expected symbol <<"))
      ((not (expval? saved-val))
       (write ">> expected value <<"))
      ((not (environment? saved-env))
       (write ">> expected environment <<"))
      (else
       (cons (list saved-sym saved-val) saved-env)))))

(define environment?
  (lambda (x)
    (or (null? x)
        (and (pair? x)
             (list? (car x))
             (= 2 (length (car x)))
             (symbol? (car (car x)))
             (expval? (car (cdr (car x))))
             (environment? (cdr x))))))

(define apply-env
  (lambda (env sym)
    (cond 
      ((null? env)
       (write ">> expected symbol <<"))
      ((eq? sym (car (car env)))
       (car (cdr (car env))))
      (else
       (apply-env (cdr env) sym)))))

(define init-env
  (lambda ()
    (extend-env
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

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
     ("+" "(" (separated-list expression ",")  ")")
     plus-exp)
    (expression
     ("-" "(" (separated-list expression ",") ")")
     diff-exp)    
    (expression
     ("*" "(" (separated-list expression ",") ")")
     times-exp)
    (expression
     ("/" "(" (separated-list expression ",") ")")
     divide-exp)
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
     let-exp)
    (expression
     ("proc" "(" identifier ")" expression)
     proc-exp)
    (expression
     ("(" expression expression ")")
     call-exp)))

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

;; proc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-datatype proc proc?
  (procedure
   (var symbol?)
   (exp expression?)
   (env environment?)))

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (extend-env var val saved-env))))))

;; scaffold  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?)))

(define expval->num
  (lambda (val)
    (cases expval val 
      (num-val (num) num)
      (else (write ">> expected num ! <<")))))

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (write ">> expected bool <<")))))

(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (p) p)
      (else (write ">> expected proc <<")))))

(define list-plus
  (lambda (lst env)
    (if (null? lst)
        (num-val 0)
        (num-val (+ (expval->num (value-of (car lst) env)) (expval->num (list-plus (cdr lst) env)))))))

(define list-diff
  (lambda (lst env)
    (if (null? lst)
        (num-val 0)
        (let* ([fst (car lst)]
               [fst-num (expval->num (value-of fst env))])
          (num-val (- fst-num (expval->num (list-plus (cdr lst) env))))))))

(define list-times
  (lambda (lst env)
    (if (null? lst)
        (num-val 1)
        (num-val (* (expval->num (value-of (car lst) env)) (expval->num (list-times (cdr lst) env)))))))

(define list-divide
  (lambda (lst env)
    (if (null? lst)
        (num-val 1)
        (let* ([fst (car lst)]
               [fst-num (expval->num (value-of fst env))])
          (num-val (/ fst-num (expval->num (list-times (cdr lst) env))))))))

;; interpreter;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define run 
  (lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1) (value-of exp1 (init-env))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      ;; exp := num
      (const-exp (num) (num-val num))

      ;; exp := identifier
      (var-exp (var) (apply-env env var))

      ;; exp := + (exp {, exp})
      (plus-exp (arg-list)                
       (list-plus arg-list env))      
      
      ;; exp := - (exp {, exp})      
      (diff-exp (arg-list)
       (if (null? arg-list)
           (write ">> empty diff-exp <<")
           (list-diff arg-list env)))
      
      ;; exp := * (exp {, exp})      
      (times-exp (arg-list)
       (list-times arg-list env))
      
      ;; exp := / (exp {, exp})      
      (divide-exp (arg-list)
       (if (null? arg-list)
           (write ">> empty divide-exp <<")
           (list-divide arg-list env)))
      
      ;; exp := zero? exp
      (zero?-exp (exp1)
       (let ([val1 (value-of exp1 env)]) 
         (let ([num1 (expval->num val1)]) 
           (if (zero? num1)
               (bool-val #t)
               (bool-val #f)))))
      
      ;; exp := if exp then else exp
      (if-exp (exp1 exp2 exp3) 
       (let ([val1 (value-of exp1 env)]) 
         (if (expval->bool val1) 
             (value-of exp2 env) 
             (value-of exp3 env))))
      
      ;; exp := let identifier = exp in body
      (let-exp (var exp1 body) 
       (let ([val1 (value-of exp1 env)]) 
         (value-of body (extend-env var val1 env))))
      
      ;; exp := proc ( identifier ) exp
      (proc-exp (var exp) 
                (proc-val (procedure var exp env)))
      
      ;; exp := ( proc para )
      (call-exp (rator rand)
                (let ([p (expval->proc (value-of rator env))])
                  (apply-procedure p (value-of rand env)))))))

(write "-- Hello Anney ! --")

;; -- test lex -----------------------------------------------------------
(define lexer (sllgen:make-string-scanner scanner-let grammar-let))
;(lexer "foo bar 10 % this is a comment")

;; -- test ---------------------------------------------------------------
(define stmt0 "let x = 9
               in let y = 1
                 in +(x, y)")

(define stmt1 "let x = 7
               in let y = 2
                 in let y = let x = -(x, 1) in -(x, y)
                   in -(-(x, 8), y)")

(define stmt2 "- (1, 2, +(3, 4, *(5, 6, 7)))")

(define stmt3 "- (1, 2)")

(define stmt4 "- (1, 2, 3, 4)")

(define stmt5 "+ (1, 2, +(3, 4, *(5, 6, 7), /(12, 2, 3)))")

(define stmt6 "let x = 1
               in let y = 2
                 in let z = 3
                   in let w = 4
                     in +(x, * (y, z, w))")
(define stmt7 "/(12, 2, 3)")
(define stmt8 "/()")
(define stmt9 "*()")
(define stmt10 "+()")
(define stmt11 "-()")
(define stmt12 "let x = 1
                in let y = 1
                  in if zero? (-(x, y)) then +(x, y) else -(x, y)")

(define stmt13 "let x = 200
                in let f = proc (z) -(z,x)
                  in let x = 100
                    in let g = proc (z) -(z, x)
                      in -((f 1), (g 1))")