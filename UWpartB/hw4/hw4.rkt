;; Homework assignment 4 | UWpartA | by SID 2020
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; >>>--- Problem 1 ---<<<
;; assume s > 0
(define (sequence l h s)
  (cond [(= l h) (list l)] 
        [(> (+ l s) h) null] 
        [#t (cons l (sequence (+ l s) h s))]))

;; >>>--- Problem 2 ---<<<
(define (string-append-map xs str)
  (map (lambda (x) (string-append x str)) xs))

;; >>>--- Problem 3 ---<<<
(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;; >>>--- Problem 4 ---<<<
(define (stream-for-n-steps s n)
  (cond [(>= 0 n) null]
        (#t (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1))))))

;; >>>--- Problem 5 ---<<<
(define funny-number-stream
  (lambda ()
    (letrec ([nat-stream (lambda (n)
                           (if (zero? (modulo n 5))
                               (cons (- n) (lambda () (nat-stream (+ n 1))))
                               (cons n (lambda () (nat-stream (+ n 1))))))])
      (nat-stream 1))))

;; >>>--- Problem 6 ---<<<
(define dan-than-dog
  (lambda ()
    (letrec ([dan (lambda () (cons "dan.jpg" (lambda () (dog) )))]
             [dog (lambda () (cons "dog.jpg" (lambda () (dan) )))])
      (dan))))

;; >>>--- Problem 7 ---<<<
(define stream-add-zero
  (lambda (s)
    (lambda () (cons (cons 0 (car (s))) (cdr (s))))))

;; >>>--- Problem 8 ---<<<
;; TODO: reforge this
(define (cycle-lists xs ys)
  (lambda ()
    (letrec ([aux
              (lambda (xs ys)
                (cons (cons (car xs) (car ys))
                      (lambda ()
                        (aux (append (cdr xs) (list (car xs))) (append (cdr ys) (list (car ys)))))))])
      (aux xs ys))))

;; >>>--- Problem 9 ---<<<
;; >>>--- Problem 10 ---<<<