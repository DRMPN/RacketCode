;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Next of the season|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Season is one of:
;; - "spring"
;; - "summer"
;; - "fall"
;; - "winter"
;; interp. the four seasons of the year
;; <examples are redundant for enumerations>
#;
(define (fn-for-season s)
  (cond [(string=? "spring" s) (...)]
        [(string=? "summer" s) (...)]
        [(string=? "fall" s) (...)]
        [(string=? "winter" s) (...)]))

;; Template Rules Used:
;; - one of: 4 cases
;; - atomic distinct: "spring"
;; - atomic distinct: "summer"
;; - atomic distinct: "fall"
;; - atomic distinct: "winter"

;; Season->Season
;; takes the given season and produces the season that follows
#;
(define (nextSeason s)
  (...s))
(check-expect (nextSeason "spring") "summer")
(check-expect (nextSeason "summer") "fall")
(check-expect (nextSeason "fall") "winter")
(check-expect (nextSeason "winter") "spring")

(define (nextSeason s)
  (cond [(string=? "spring" s) "summer"]
        [(string=? "summer" s) "fall"]
        [(string=? "fall" s) "winter"]
        [(string=? "winter" s) "spring"])
  )
