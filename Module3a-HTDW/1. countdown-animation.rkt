;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |d. countdown-animation-starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; countdown-animation starter.rkt
;; a countdown that flashes down from zero per tick
;; ==========

;; Constants:

(define HEIGHT 400)
(define WIDTH 400)
(define CTR-Y (/ HEIGHT 2))
(define CTR-X (/ WIDTH 2))
(define SPEED 1)
(define MTS (empty-scene WIDTH HEIGHT))
(define TEXT-SIZE 42)
(define TEXT-COLOR "black")

;; =========
;; Data Definitions (from changing domain analysis)

;; Countdown is Number[0, 10] - interval
;; interp. Countdown is number of seconds until 0
#; examples
(define cd1 1)
(define cd2 10)
(define cd3 5)

(define (fn-for-countdown cd)
  (... cd))

;; template rules used: atomic-non-disctinct interval

;; =========
;; Functions:

;; Countdown -> Image
;; start the world with (main 10)
;;

(define (main cd)
  (big-bang cd
            (on-tick  countdown 1)           ; Countdown -> Countdown
            (to-draw  display-count-down)    ; Countdown -> Img
            (on-key   reset-countdown)))     ; Countdown KeyEvent -> Countdown

;; Countdown -> Countdown
;; produce the next coutndown by counting down by one
(check-expect (countdown 3) 2)
(check-expect (countdown 10) 9)
(check-expect (countdown 1) 0)
(check-expect (countdown 0) 0)

;(define (countdown t) 2) ;stub

(define (countdown t)
  (if (= t 0) 0
   (- t 1)))

;; template used: atomic-nondistinct

;; Countdown -> Img
;; produce an image of the countdown
(check-expect (display-count-down 4) (place-image (text (number->string 4) TEXT-SIZE TEXT-COLOR) CTR-X CTR-Y MTS))
(check-expect (display-count-down 2) (place-image (text (number->string 2) TEXT-SIZE TEXT-COLOR) CTR-X CTR-Y MTS))
(check-expect (display-count-down 0) (place-image (text (number->string 0) TEXT-SIZE TEXT-COLOR) CTR-X CTR-Y MTS))


;(define (display-count-down t) MTS) stub

(define (display-count-down t)
  (place-image (text (number->string t) TEXT-SIZE TEXT-COLOR) CTR-X CTR-Y MTS))

;;template used: atomic non-distinct: Countdown 

;; Countdown KeyEvent -> Countdown
;; Reset Countdown to 10 once spacebar is pressed
;; !!!
(check-expect (reset-countdown 10 " ") 10)
(check-expect (reset-countdown 0 " ") 10)
(check-expect (reset-countdown 0 "a") 0)
(check-expect (reset-countdown 10 "a") 10)
              
;(define (reset-countdown t ke) 10) ;stub

(define (reset-countdown t ke)
  (cond [(key=? ke " ") 10]
        [else 
         t]))

;; template format used: on-key

