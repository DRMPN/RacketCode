#lang racket

;; struct is a feature that adds primitive data

(struct foo (bar baz quux) #:transparent)
; make foo -> (foo e1 e2 e3)
; (foo? e) -> Bool
; (foo-bar e), (foo-baz e), (foo-quux e) -> e
; #: transparent -> printable

(struct const (int) #:transparent)
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)

;; eval-exp :: exp -> exp
(define (eval-exp e)
  (cond [(const? e) e]
        [(negate? e) (const (- (const-int (eval-exp (negate-e e)))))]
        [(add? e) (let ([v1 (const-int (eval-exp (add-e1 e)))]
                        [v2 (const-int (eval-exp (add-e2 e)))])
                    (const (+ v1 v2)))]
        [(multiply e) (let ([v1 (const-int (eval-exp (multiply-e1 e)))]
                            [v2 (const-int (eval-exp (multiply-e2 e)))])
                        (const (* v1 v2)))]))