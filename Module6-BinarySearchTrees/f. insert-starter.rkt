;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |f. insert-starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; insert-starter.rkt

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
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42 
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             false))
(define BST10 (make-node 10 "why" BST3 BST42))

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

;; Integer String BST -> BST
;; interp. Integer is Key String is Value. We are inserting into the BST
(check-expect (insert-bst 0 "hoing" BST0) (make-node 0 "hoing" false false))
(check-expect (insert-bst 0 "hoing" BST1) (make-node 1 "abc" (make-node 0 "hoing" false false) false))
(check-expect (insert-bst 5 "hoing" BST3) (make-node 3 "ilk" BST1
                                                     (make-node 4 "dcj" false (make-node 7 "ruf"
                                                                              (make-node 5 "hoing" false false) false))))
;(define (insert-bst int string bst) false)

(define (insert-bst i s b)
  (cond [(false? b) (make-node i s false false)]
        [else
         (if (< (node-key b) i)
             (make-node
              (node-key b) (node-val b) (node-l b) (insert-bst i s (node-r b)))
             (make-node
              (node-key b) (node-val b) (insert-bst i s (node-l b)) (node-r b)))]))
