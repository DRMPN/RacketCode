;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname student-card) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ==================
;; Data definitions:

(define-struct student (name id))
;; Student is (make-student String Natural)
;; interp. a student with name and student id
(define S1 (make-student "Eva" 3124))
(define S2 (make-student "John" 7893))
#;
(define (fn-for-student s)
  (... (student-name s)
       (student-id s)))
;; ListOfStudent is on of:
;; - empty
;; - (cons Student ListOfStudent)
;; interp. a list of students
(define LOS1 empty)
(define LOS2 (cons S1 empty))
(define LOS3 (cons S1 (cons S2 empty)))
#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (fn-for-student (first los))
              (fn-for-los (rest los)))]))
;; ==================
;; Functions:

;; ListOfStudent -> ListOfStudent
;; produces a list of student cards
(check-expect (cardList empty) empty)
(check-expect (cardList (cons S2 (cons S1 empty))) (cons "John 7893" (cons "Eva 3124" empty)))
; (define (cardList los) empty) ;stub

(define (cardList los)
  (cond [(empty? los) empty]
        [else
         (cons (makeCard (first los))
              (cardList (rest los)))]))

;; Student -> String
;; produces a student card
(check-expect (makeCard S1) (string-append (student-name S1) " " (number->string (student-id S1))))
;(define (makeCard s) "") ;stub
(define (makeCard s)
  (string-append (student-name s) " " (number->string (student-id s))))