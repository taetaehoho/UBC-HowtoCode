;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |g. render-bst-w-lines-starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; render-bst-w-lines-starter.rkt

(require 2htdp/image)


;; Constants

(define TEXT-SIZE  14)
(define TEXT-COLOR "BLACK")
(define HEIGHT 20)
(define KEY-VAL-SEPARATOR ":")

(define MTTREE (rectangle 20 10 "solid" "white"))



;; Data definitions:

(define-struct node (key val l r))
;; A BST (Binary Search Tree) is one of:
;;  - false
;;  - (make-node Integer String BST BST)
;; interp. false means no BST, or empty BST
;;         key is the node key
;;         val is the node val
;;         l and r are left and right subtrees
;; INVARIANT: for a given node:
;;     key is > all keys in its l(eft)  child
;;     key is < all keys in its r(ight) child
;;     the same key never appears twice in the tree
(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST7 (make-node 7 "ruf" false false)) 
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42 
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             (make-node 50 "dug" false false)))
(define BST10
  (make-node 10 "why" BST3 BST42))
(define BST100 
  (make-node 100 "large" BST10 false))
#;
(define (fn-for-bst t)
  (cond [(false? t) (...)]
        [else
         (... (node-key t)    ;Integer
              (node-val t)    ;String
              (fn-for-bst (node-l t))
              (fn-for-bst (node-r t)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic-distinct: false
;;  - compound: (make-node Integer String BST BST)
;;  - self reference: (node-l t) has type BST
;;  - self reference: (node-r t) has type BST

;; Functions:

;; BST -> image
;; produce simple rendering of the tree
;; use previous tests as true as next tests
(check-expect (render-bst false) MTTREE)
(check-expect (render-bst BST1)
              (above (render-key-val 1 "abc") 
                     (lines (image-width (render-bst false))
                            (image-width (render-bst false)))
                     (beside (render-bst false)
                             (render-bst false))))
                            
(define (render-bst t)
  (cond [(false? t) MTTREE]
        [else
         (above (render-key-val (node-key t) (node-val t))
                (lines (image-width (render-bst (node-l t)))
                       (image-width (render-bst (node-r t))))
                (beside (render-bst (node-l t))
                        (render-bst (node-r t))))]))

;; function def
;; render-key-val
;; number string -> image (atomic distinct, atomic distinct -> image)
(check-expect (render-key-val 99 "foo") 
              (text (string-append "99" KEY-VAL-SEPARATOR "foo") TEXT-SIZE TEXT-COLOR))

(define (render-key-val key val)
  (text (string-append (number->string key) KEY-VAL-SEPARATOR val)
                    TEXT-SIZE
                    TEXT-COLOR))

;; function def lines
;; image -> image
;; interp. takes box and adds lines
(check-expect (lines 100 50) (add-line (add-line (rectangle (+ 100 50) (/ (+ 100 50) 4) "solid" "white")
                                       (/ (+ 100 50) 2) 0
                                       (/ 100 2) (/ (+ 100 50) 4) "black")
                                    (/ (+ 100 50) 2) 0
                                    (+ (/ 50 2) 100) (/ (+ 100 50) 4) "black"))
             
;(define (lines lw rw) MTTREE)

(define (lines lw rw)
  (add-line (add-line (rectangle (+ lw rw) (/ (+ lw rw) 4) "solid" "white")
                                 (/ (+ lw rw) 2) 0
                                 (/ lw 2) (/ (+ lw rw) 4) "black")
                      (/ (+ lw rw) 2) 0
                      (+ (/ rw 2) lw) (/ (+ lw rw) 4) "black"))
              
