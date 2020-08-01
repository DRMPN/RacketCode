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
(define dan-then-dog
    (letrec ([dan (lambda () (cons "dan.jpg" dog ))]
             [dog (lambda () (cons "dog.jpg" dan ))])
      dan))

;; >>>--- Problem 7 ---<<<
(define (stream-add-zero s)
  (lambda ()
      (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

;; >>>--- Problem 8 ---<<<
(define (cycle-lists xs ys)
  (lambda ()                                       ;; make stream
    (letrec ([aux (lambda (xs ys)                  ;; helper
                    (cons (cons (car xs) (car ys)) ;; first pair
                          (lambda ()               ;; rest stream
                            (aux (append (cdr xs) (list (car xs))) 
                                 (append (cdr ys) (list (car ys)))))))])
      (aux xs ys))))


;; >>>--- Problem 9 ---<<<
(define (vector-assoc val vec)
  (letrec
      ([veclen (vector-length vec)] ;; saved length to avoid rep. comp.
       [aux (lambda (n)
              (letrec ([vecref (lambda () (vector-ref vec n))] ;; lazy evaluation otherwise error
                       [notpair? (lambda () (or (not (pair? (vecref))) (list? (vecref))))])
                (cond
                  [(<= veclen n) #f]                     ;; empty? -> false 
                  [(notpair?) (aux (+ n 1))]             ;; not pair? -> skip
                  [(equal? val (car (vecref))) (vecref)] ;; element? -> result
                  [#t (aux (+ n 1))])))])                ;; _ -> go again
    (aux 0)))                                            ;; start of a helper fun

;; >>>--- Problem 10 ---<<<
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]             ;; produces empty cache
           [value null]                           ;; mutates only once
           [f (lambda (v)                         ;; adds accumulators
                (begin (set! value v) (g xs 0)))] ;; ! mutation
           [g (lambda (xs acc)
                (cond [(null? xs) #f]                                ;; empty case
                      [#t (let ([ans (vector-assoc (caar xs) cache)] ;; find value in cache
                                ;; checks and changes bound of the cache [0,n-1]
                                [inrange? (lambda (x) (if (>= x (- n 1)) 0 (+ x 1)))])
                            (cond
                              ;; in the cache -> change cache's last element, go again
                              [ans (begin (vector-set! cache acc ans) (g (cdr xs) acc))]
                              ;; matches? -> add value to cache, produce an answer
                              [(= value (caar xs)) (begin (vector-set! cache acc (car xs)) (car xs))]
                              ;; notfound? -> add value to cache, change limit, go again
                              [#t (begin (vector-set! cache acc (car xs))
                                         (g (cdr xs) (inrange? acc)))]))]))])
    f))