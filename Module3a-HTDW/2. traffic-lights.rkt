;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |e. traffic-light-starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; traffic-light-starter.rkt
;; Traffic light that changes color every second

;; Constants

(define CIRCLE-RADIUS 30)     ;radius of each circle 
(define SPACING 10)           ;space between each light

(define WIDTH (+ (* 2 SPACING) (* 2 CIRCLE-RADIUS)))
(define LENGTH (+ (* 4 SPACING) (* 6 CIRCLE-RADIUS)))
(define BACKGROUND (rectangle WIDTH LENGTH "solid" "black"))

(define SPACE (rectangle WIDTH SPACING "solid" "black"))

(define RON
  (overlay (above SPACE
                  (circle CIRCLE-RADIUS "solid" "red")
                  SPACE
                  (circle CIRCLE-RADIUS "outline" "yellow")
                  SPACE
                  (circle CIRCLE-RADIUS "outline" "green")
                  SPACE)
           BACKGROUND))

(define YON
  (overlay (above SPACE
                  (circle CIRCLE-RADIUS "outline" "red")
                  SPACE
                  (circle CIRCLE-RADIUS "solid" "yellow")
                  SPACE
                  (circle CIRCLE-RADIUS "outline" "green")
                  SPACE)
           BACKGROUND))

(define GON
  (overlay (above SPACE
                  (circle CIRCLE-RADIUS "outline" "red")
                  SPACE
                  (circle CIRCLE-RADIUS "outline" "yellow")
                  SPACE
                  (circle CIRCLE-RADIUS "solid" "green")
                  SPACE)
           BACKGROUND))

;; =========
;; Data Definition (from changing domain analysis)

;; TrafficLight is string one of:
;; - "red"
;; - "yellow"
;; - "green"
;; interp. current color of the traffic light 

;; examples are redundant for enumerations
#;
(define (fn-for-trafficlight t1)
  (cond [(string=? t1 "red") (...)]
        [(string=? t1 "yellow")(...)]
        [(string=? t1 "green")(...)]))

;; template used one of:
;; - atomic distinct: Red, yellow, green

;; =========
;; FUNCTIONS


;; TrafficLight -> Image
;; start with the world main "red"

(define (main tl)
  (big-bang tl
            (on-tick change-light 1)  ; TrafficLight -> TrafficLight 
            (to-draw render-light)))  ; TrafficLight -> Image

;; TrafficLight -> TrafficLight
;; purpose. take current traffic light and change to next trafficlight
(check-expect (change-light "red") "green")
(check-expect (change-light "yellow") "red")
(check-expect (change-light "green") "yellow")

;(define (change-light tl) "red") stub

(define (change-light t1)
  (cond [(string=? t1 "red") "green"]
        [(string=? t1 "yellow") "red" ]
        [(string=? t1 "green") "yellow"]))

; template from data definition

;; TrafficLight -> Image
;; purpose. take current traffic light and display it as an image
(check-expect (render-light "red") RON)
(check-expect (render-light "yellow") YON)
(check-expect (render-light "green") GON)
;(define (render-light t1) RON) stub

(define (render-light t1)
  (cond [(string=? t1 "red") RON]
        [(string=? t1 "yellow") YON]
        [(string=? t1 "green") GON]))

; template taken from data definition 