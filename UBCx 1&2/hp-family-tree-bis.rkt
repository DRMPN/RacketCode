;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname hp-family-tree-bis) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))

;; hp-family-tree-starter.rkt
;;====================================================================================================
;; Data definitions:
;;====================================================================================================
(define-struct wiz (name wand patronus kids))
;; Wizard is (make-wiz String String String ListOfWizard)
;; interp. a wizard in a descendant family tree
;;         name is the first name
;;         wand is the wood their primary wand is made of ("" if unknown)
;;         patronus is a string  ("" if unknown)
;;         kids is their immediate children
#;
(define (fn-for-wizard w)
  (... (wiz-name w)
       (wiz-wand w)
       (wiz-patronus w)
       (fn-for-low (wiz-kids w))))
;;
;; ListOfWizard is one of:
;;  - empty
;;  - (cons Wizard ListOfWizard)
;; interp. a list of wizards
#;
(define (fn-for-low low)
  (cond [(empty? low) (...)]
        [else
         (... (fn-for-wizard (first low))
              (fn-for-low (rest low)))]))

(define ARTHUR
  (make-wiz "Arthur" "" "Weasel"
            (list (make-wiz "Bill" "" "" (list (make-wiz "Victoire"  "" "" empty)      
                                               (make-wiz "Dominique" "" "" empty)   
                                               (make-wiz "Louis"     "" "" empty)))   
                  (make-wiz "Charlie" "ash" "" empty)
                  (make-wiz "Percy" "" "" (list (make-wiz "Molly" "" "" empty)          
                                                (make-wiz "Lucy"  "" "" empty)))
                  (make-wiz "Fred"    ""    "" empty)
                  (make-wiz "George"  ""    "" (list (make-wiz "Fred" "" "" empty)     
                                                     (make-wiz "Roxanne"  "" "" empty)))
                  (make-wiz "Ron"     "ash" "Jack Russell Terrier"
                            (list (make-wiz "Rose" "" "" empty)  
                                  (make-wiz "Hugo" "" "" empty)))
                  (make-wiz "Ginny"   ""    "Horse" 
                            (list (make-wiz "James" "" "" empty)
                                  (make-wiz "Albus" "" "" empty))))))

;; ListOfPair is one of:
;;  - empty
;;  - (cons (list String String) ListOfPair)
;; interp. used to represent an arbitrary number of pairs of strings
(define LOP1 empty)
(define LOP2 (list (list "Harry" "stag") (list "Hermione" "otter")))
#;
(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else
         (... (first (first lop))
              (second (first lop)) ;(first (rest (first lop)))
              (fn-for-lop (rest lop)))]))


;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings
(define LOS1 empty)
(define LOS2 (list "a" "b"))
#;
(define (fn-for-los los)
  (cond [(empty? los) ...]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

;;====================================================================================================
;; Functions:
;;====================================================================================================

;; Wizard -> ListOfPair
;; produces a pair list of every person in the tree and his or her patronus
(check-expect (ListOfPatron--low empty) empty)
(check-expect (ListOfPatron--w ARTHUR) (list (list "Arthur" "Weasel")
                                             (list "Ron" "Jack Russell Terrier")
                                             (list "Ginny" "Horse")))
;(define (ListOfPatron--w w) empty) ;stub

(define (ListOfPatron--w w)
  (if (string=? (wiz-patronus w) "")
      (ListOfPatron--low (wiz-kids w))
      (cons (list (wiz-name w) (wiz-patronus w)) (ListOfPatron--low (wiz-kids w)))))

(define (ListOfPatron--low low)
  (cond [(empty? low) empty]
        [else
         (append (ListOfPatron--w (first low))
                 (ListOfPatron--low (rest low)))]))

;; Wizard -> ListOfString
;; produces the names of every person in a given tree
;;           whose wands are made of a given material
(check-expect (ListOfWand--low empty "wood") empty)
(check-expect (ListOfWand--w ARTHUR "ash")
              (list "Charlie" "Ron"))
(check-expect (ListOfWand--w ARTHUR "wood")
              empty)

;(define (ListOfWand--w w m) empty) ;stub
#;
(define (ListOfWand--w w m)
  (if (string=? (wiz-wand w) "")
      (ListOfWand--low (wiz-kids w) m)
      (if (string=? (wiz-wand w) m)
       (cons (wiz-name w) (ListOfWand--low (wiz-kids w) m))
       (ListOfWand--low (wiz-kids w) m))))

(define (ListOfWand--w w m)
  (cond
    [(string=? (wiz-wand w) "") (ListOfWand--low (wiz-kids w) m)]
    [(string=? (wiz-wand w) m) (cons (wiz-name w) (ListOfWand--low (wiz-kids w) m))]
    [else
     (ListOfWand--low (wiz-kids w) m)]))

(define (ListOfWand--low low m)
  (cond [(empty? low) empty]
        [else
         (append (ListOfWand--w (first low) m)
                 (ListOfWand--low (rest low) m))]))

