;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |d. making-rain-filtered-starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; making-rain-filtered-starter.rkt

;; Make it rain where we want it to.

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 300)

(define SPEED 1)

(define DROP (ellipse 4 8 "solid" "blue"))

(define MTS (rectangle WIDTH HEIGHT "solid" "light blue"))

;; =================
;; Data definitions:

(define-struct drop (x y))
;; Drop is (make-drop Integer Integer)
;; interp. A raindrop on the screen, with x and y coordinates.

(define D1 (make-drop 10 30))

#;
(define (fn-for-drop d)
  (... (drop-x d) 
       (drop-y d)))

;; Template Rules used:
;; - compound: 2 fields


;; ListOfDrop is one of:
;;  - empty
;;  - (cons Drop ListOfDrop)
;; interp. a list of drops

(define LOD1 empty)
(define LOD2 (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-drop (first lod))
              (fn-for-lod (rest lod)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Drop ListOfDrop)
;; - reference: (first lod) is Drop
;; - self reference: (rest lod) is ListOfDrop

;; =================
;; Functions:

;; ListOfDrop -> ListOfDrop
;; start rain program by evaluating (main empty)
(define (main lod)
  (big-bang lod
            (on-mouse handle-mouse)   ; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
            (on-tick  next-drops)     ; ListOfDrop -> ListOfDrop
            (to-draw  render-drops))) ; ListOfDrop -> Image


;; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
;; if mevt is "button-down" add a new drop at that position

(check-expect (handle-mouse empty 10 10 "button-down") (cons (make-drop 10 10) empty))
(check-expect (handle-mouse (cons (make-drop 10 10) empty) 10 10 "button-up") (cons (make-drop 10 10) empty))
(check-expect (handle-mouse (cons (make-drop 10 10) (cons (make-drop 20 10) empty)) 30 20 "button-down")
              (cons (make-drop 30 20) (cons (make-drop 10 10) (cons (make-drop 20 10) empty))))

;(define (handle-mouse lod x y mevt) empty) ; stub

(define (handle-mouse lod x y me)
  (cond [(mouse=? me "button-down") (cons (make-drop x y) lod)]
        [else
         lod]))              

;; ListOfDrop -> ListOfDrop
;; produce filtered and ticked list of drops

(check-expect (next-drops (cons (make-drop 10 10) empty)) (cons (make-drop (+ 10 SPEED) (+ 10 SPEED)) empty))
(check-expect (next-drops (cons (make-drop 20 20) (cons (make-drop 10 15) empty)))
              (cons (make-drop (+ 20 SPEED) (+ 20 SPEED)) (cons (make-drop (+ 10 SPEED) (+ 15 SPEED)) empty)))
;(define (next-drops lod)empty) ; stub

(define (next-drops lod)
  (on-screen (tick-drop lod)))

;;tick-drop
;; lod -> lod
;; move each drop in lod to next drop

(check-expect (tick-drop (cons (make-drop 10 10) empty)) (cons (make-drop (+ 10 SPEED) (+ 10 SPEED)) empty))

;(define (tick-drop lod) empty)

(define (tick-drop lod)
  (cond [(empty? lod) empty]
        [else
         (cons (next-drop (first lod))
              (tick-drop (rest lod)))]))

;; next-drop
;; drop -> drop
;; move drop one tick by SPEED
;; ASSUMES ITS ON SCREEN
(check-expect (next-drop (make-drop 10 10)) (make-drop 11 11))
;(define (tick-drop drop) drop) ;stub

(define (next-drop d)
  (make-drop (+ (drop-x d) SPEED) (+ (drop-y d) SPEED)))

;; on-screen
;; ListOfDrop -> ListOfDrop
;; Take list of drop and returns list that is only list of drops that are onscreen

(check-expect (on-screen (cons (make-drop 10 10) empty)) (cons (make-drop 10 10) empty))
(check-expect (on-screen (cons (make-drop (+ HEIGHT 1) 10) empty)) empty)
(check-expect (on-screen (cons (make-drop 10 10) (cons (make-drop 10 (+ WIDTH 1)) empty))) (cons (make-drop 10 10) empty))

;(define (on-screen lod) lod) ;stub

(define (on-screen lod)
  (cond [(empty? lod) empty]
        [else
         (if (in-screen (first lod))
             (cons (first lod) (on-screen (rest lod)))
             (on-screen (rest lod)))]))

;; in-screen
;; drop -> boolean
;; produce true if the drop is in screen false if not

(check-expect (in-screen (make-drop (+ HEIGHT 1) 10)) false)
(check-expect (in-screen (make-drop 10 (+ WIDTH 1))) false)
(check-expect (in-screen (make-drop 10 10)) true)

;(define (in-screen drop) true) ;stub

(define (in-screen d)
  (and (< (drop-x d) WIDTH)
       (< (drop-y d) HEIGHT)))


;; ListOfDrop -> Image
;; Render the drops onto MTS
(check-expect (render-drops empty) MTS)
(check-expect (render-drops (cons (make-drop 10 10) empty)) (place-image DROP 10 10 MTS))
(check-expect (render-drops (cons (make-drop 3 7) (cons (make-drop 12 30) empty)))
              (place-image DROP 3 7 (place-image DROP 12 30 MTS)))
              

;(define (render-drops lod) MTS) ; stub

(define (render-drops lod)
  (cond [(empty? lod) MTS]
        [else
         (place-drop (first lod)
         (render-drops (rest lod)))]))

;; template from ListOfDrop

;; place-drop
;; drop -> image
;; !!!

(define (place-drop d MTS)
  (place-image DROP (drop-x d) (drop-y d) MTS))