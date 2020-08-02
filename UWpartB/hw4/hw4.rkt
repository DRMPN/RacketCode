;; Homework assignment 4 | UWpartB | by SID 2020
;; ver. 1.3 (final)
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; >>>--- Problem 1 ---<<<
;; assume s > 0
(define (sequence l h s)
  (if (> l h) null 
      (cons l (sequence (+ l s) h s))))

;; >>>--- Problem 2 ---<<<
(define (string-append-map xs str)
  (map (lambda (x) (string-append x str)) xs))

;; >>>--- Problem 3 ---<<<
(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;; >>>--- Problem 4 ---<<<
(define (stream-for-n-steps stream n)
  (let ([s (stream)])
    (if (>= 0 n) null
        (cons (car s) (stream-for-n-steps (cdr s) (- n 1))))))

;; >>>--- Problem 5 ---<<<
(define funny-number-stream
    (letrec ([nat-stream (lambda (n)
                           (cons (if (zero? (modulo n 5)) (- n) n)
                                 (lambda () (nat-stream (+ n 1)))))])
     (lambda () (nat-stream 1))))

;; >>>--- Problem 6 ---<<<
(define dan-then-dog
    (letrec ([dan (lambda () (cons "dan.jpg" dog))]
             [dog (lambda () (cons "dog.jpg" dan))])
      dan))

;; >>>--- Problem 7 ---<<<
(define (stream-add-zero stream)
  (let ([s (stream)])
    (lambda ()
      (cons (cons 0 (car s)) (stream-add-zero (cdr s))))))

;; >>>--- Problem 8 ---<<<
(define (cycle-lists xs ys)
  (letrec ([aux (lambda (xs ys)                                       ;; helper
                  (cons (cons (car xs) (car ys))                      ;; first pair
                        (lambda ()                                    ;; rest stream
                          (aux (append (cdr xs) (list (car xs)))      ;; [a b c] -> [b c a]
                               (append (cdr ys) (list (car ys)))))))])
     (lambda () (aux xs ys))))

;; >>>--- Problem 9 ---<<<
;; takes int as value and any vector
;; produces false if value is not in the list, otherwise pair:(value, _)
(define (vector-assoc val vec)
  (letrec
      ([vec-len (vector-length vec)]
       [aux (lambda (n)
              (if (<= vec-len n)
                  #f                     
                  (let ([vec-ref (vector-ref vec n)])
                    (if (and (pair? vec-ref) (equal? (car vec-ref) val))
                        vec-ref            
                        (aux (+ n 1))))))])              
            (aux 0)))

;; >>>--- Problem 10 ---<<<
;; takes any list, int for cache size
;; produces function: takes int as value -> 
;;                    #f if value is not in the list, otherwise pair:(value, _)
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]                       ;; produces empty cache
           [value null]                                     ;; mutates only once
           [f (lambda (v) (begin (set! value v) (g xs 0)))] ;; mutation -> call g with accumulators     
           [g (lambda (xs acc)                              ;; tail rec. imp. of assoc using cache
                (if (null? xs)
                    #f                                                                                ;; empty case
                    (let ([ans (vector-assoc (caar xs) cache)]                                        ;; find value in cache
                          [inrange? (lambda (x) (if (>= x (- n 1)) 0 (+ x 1)))])                      ;; checks and changes acc to be [0,n-1]
                      (cond
                        [ans (begin (vector-set! cache acc ans) (g (cdr xs) acc))]                    ;; in the cache -> change cache's last element, go again
                        [(= value (caar xs)) (begin (vector-set! cache acc (car xs)) (car xs))]       ;; matches? -> add value to cache, produce an answer   
                        [#t (begin (vector-set! cache acc (car xs)) (g (cdr xs) (inrange? acc)))]     ;; notfound? -> add value to cache, change limit, go again
                        ))))]) 
    f))