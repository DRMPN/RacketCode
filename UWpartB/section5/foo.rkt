#lang racket
(provide (all-defined-out)) ; make all def public

;defined variable s
(define s "Hello") 

(define square
  (lambda (x)
         (* x x)))

(define cube
  (lambda (x)
    (* x x x)))

;syntactic sugar for functions
(define (quart x) (* x x x))

(define (pow1 x y) ; x to the yth power
  (if (= y 0)
      1
      (* x (pow1 x (- y 1)))))

;currying
(define pow2
  (lambda (x)
    (lambda (y)
      (pow1 x y))))

;sum all numbers in the list
(define (sum_all xs)
  (if (null? xs)
    0
    (+ (car xs) (sum_all (cdr xs)))))

;append
(define (my_append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my_append (cdr xs) ys))))

;map
(define (my_map f xs)
  (if (null? xs)
      null
      (cons (f (car xs )) (my_map f (cdr xs)))))

;;arbitary sum_all
(define (arb_sum xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (arb_sum (cdr xs)))
          (+ (arb_sum (car xs)) (arb_sum (cdr xs))))))

;; what if element in the list is not a number
(define (arb_sum_all xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (arb_sum_all (cdr xs)))
          (if (list? (car xs))
              (+ (arb_sum_all (car xs)) (arb_sum_all (cdr xs)))
              (arb_sum_all (cdr xs))))))

;; better style with cond
(define (arb_sum_better xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (arb_sum_better (cdr xs)))]
        [#t (+ (arb_sum_better (car xs)) (arb_sum_better (cdr xs)))]))

(define (arb_sum_all_b xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (arb_sum_all_b (cdr xs)))]
        [(list? (car xs)) (+ (arb_sum_all_b (car xs)) (arb_sum_all_b (cdr xs)))]
        [#t (arb_sum_all_b (cdr xs))]))

;; let definition and error
(define (test xs)
  (cond [(null? xs) (error "test error")]
        [#t
         (let ([dummy null]) ;binding
           (cons 0 dummy))   ;body
         ]))

;; let let* letrec define are all let bindings
(define (silly_double1 x)
  (let ([x (+ x 2)]
        [y (+ x 3)])
    (+ x y -5)))

(define (silly_double2 x)
  (let* ([x (+ x 2)]
         [y (+ x 3)])
    (+ x y -7)))

;; lazy evaluations or thunks (functions with no arguments)
(define (my-mult x y-thunk)
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult (- x 1) y-thunk))]))

;; delay and force evaluations
(define (my-delay th)
  (mcons #f th)) ;; make a mutable pair with bool and thunk

(define (my-force p)
  (if (mcar p)
      (mcdr p)
      (begin (set-mcar! p #t)         ; we mutate it only once
             (set-mcdr! p ((mcdr p))) ; evaluate p and place it to thunk
             (mcdr p))))              ; evaluate thunk

;; creating streams
; produces 1
(define ones (lambda () (cons 1 ones)))

; takes x and produces x + 1
(define nat-from (lambda (x) (cons x (lambda () (nat-from (+ x 1))))))
(define from-zero (lambda () (nat-from 0)))

; power of 2
(define to-power (lambda (x) (cons x (lambda () (to-power (* x 2))))))
(define power-of-two (lambda () (to-power 2)))

; takes function and argument
; produces stream
(define (stream-maker fn arg) (cons arg (lambda () (stream-maker fn (fn arg)))))

; examples
(define ones-2 (lambda () (stream-maker (lambda (x) x) 1)))
(define power-of-two-2 (lambda () (stream-maker (lambda (x) (* x x)) 2)))