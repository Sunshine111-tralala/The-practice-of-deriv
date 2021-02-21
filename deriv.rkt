#lang typed/racket
(define-type Math-Expression (U Variable Real (Pair Operator (Listof Math-Expression))))
(define-new-subtype Variable (make-variable Symbol))
(define-type Operator (U '+ '- '* '/))

(define-predicate operator? Operator)

(: variable [-> Symbol Variable])
(define variable
  (lambda(sym)
    (if (variable? sym)
        (make-variable sym)
        (raise-argument-error 'variable "variable?" sym))))

(: math-expression? [-> Any Boolean])
(define math-expression?
  (lambda(arg)
    (or (real? arg)
        (variable? arg)
        (and (list? arg)
             (not (null? arg))
             (operator? (car arg))
             (andmap math-expression? (cdr arg))))))


(: variable? [-> Any Boolean])
(define variable?
  (lambda (arg)
    (and (symbol? arg)
         (not (operator? arg)))))


(: deriv [-> Math-Expression Variable Math-Expression])
(define (deriv foo var)
  (match foo
    [(? (lambda (exp) (constant? exp var))) 0]
    [(? variable? ) 1]
    [`(+ ,(? math-expression? #{exp-1 : Math-Expression}) 
         ,(? math-expression? #{exp-2 : Math-Expression}))
      ;; 求导加法
      (make-sum (deriv exp-1 var) (deriv exp-2 var))
      ]
    [`(* ,(? math-expression? #{exp-1 : Math-Expression}) 
         ,(? math-expression? #{exp-2 : Math-Expression}))
      ;; 求导乘法
      (make-sum (make-product (deriv exp-1 var) exp-2)
                (make-product exp-1 (deriv exp-2 var)))
      ]
    [_ (raise-arguments-error 'match-error 
                              "the arguments delivered in deriv are wrong" 
                              "foo" foo
                              "var" var)]
    ))


(: constant? [-> Math-Expression Variable Boolean])
(define (constant? expression variable)
  (and (atom? expression)
       (not (eq? expression variable))))


(: make-sum [-> Math-Expression Math-Expression Math-Expression])
(define (make-sum math-exp1 math-exp2)
  (list '+ math-exp1 math-exp2))

(: make-product [-> Math-Expression Math-Expression Math-Expression])
(define (make-product math-exp1 math-exp2)
  (list '* math-exp1 math-exp2))


(: atom? [-> Any Boolean])
(define (atom? a)
  (and (not (pair? a)) (not (null? a))))


(: make-math-expression [-> Any Math-Expression])
(define make-math-expression
  (λ (arg)
    (match arg
      [(and (? symbol? var) (? variable? var)) (variable var)]
      [(? real? num) num]
      [`(,(? operator? op) ,(? math-expression? exps) ...)
        `(,op ,@(map make-math-expression exps))]
      [_ (raise-argument-error 'make-math-expression "math-expression?" arg)])))

(make-math-expression '(+ 1 2 3 4 5))
