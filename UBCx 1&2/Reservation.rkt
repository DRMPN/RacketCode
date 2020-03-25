;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Reservation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Reservation is one of:
;;  - Natural[1, 100]
;;  - "standby"
;; interp.
;;    Natural[1, 100] means a guaranteed seat for dinner where the number 
;;		      corresponds to which reservation (not which seat).
;;    "standby"       means a standby spot, if all the reservations show 
;;		      up this person will not be seated.
(define RS1 70)
(define RS2 "standby")
#;
(define (reserv rs)
  (cond [(number? rs) (... rs)]
        [else "standby" (...)])
  )       

;; Template rules used:
;; - one of 2 cases:
;; - Atomic Non-Distinct: Natural[1, 100]
;; - Atomic Distinct Value: "standby"

;; Number, String -> Number, String
;; produce number of seat, or place in standy list
#;
(define (reserv rs)
  (cond [(number? rs) (... rs)]
        [else "standby" (...)])
  )

(check-expect (reserv 70) 70)
(check-expect (reserv "standby") "standby")


(define (reserv rs)
  (cond [(and (number? rs) (<= 1 rs) (<= rs 100))
         rs]
        [else
         "standby"])
  )