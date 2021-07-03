;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |a. double-starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;Number -> Number
;;produce 2 times the given number
(check-expect (double 3) 6)
(check-expect (double 4.2) (* 2 4.2))
(check-expect (double -2) -4)
;(define (double n) 0) ; this is the stub
;(define (double n) This is the tmplate
;  (...n))
(define (double n)
  (* 2 n))

;Problem: Design a function that pluralizes a given word.
;(Pluralize means to convert the word to its plural form.)
;For simplicity you may assume that just adding s is enough to pluralize a word.

;;String -> String
;;purpose pluralize a word by adding s
(check-expect (pluralize "ab") "abs")
(check-expect (pluralize "hoing") "hoings")
(check-expect (pluralize "fuck") "fucks")

;(define (pluralize s) "a") ; this is the stub

;(define (pluralize s) ; this is the template 
;  (...s))

(define (pluralize s)
  (string-append s "s")) 