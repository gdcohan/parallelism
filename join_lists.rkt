#|
Partners: Gregory Cohan and Patrick Gunawan
Project: Parallelism - Part 1
|#

#lang planet cs019/cs019:1:9

(require "join-lists-support.rkt")

(define JoinList$ (Sig: join-list?))

;Tests lists
(define testlist1
  (list->join-list (list 2 6 2 79 2 4 7)))

(define testlist2
  (list->join-list (list 3 12 3 5 0 9)))
(define testlist3
  (list->join-list (list 39 5 382 2 -1)))



;j-max: JoinList$ (Number$ Number$ -> Boolean$) -> Number$
;given a Joinlist$ and a number comperator, 
;outputs the max number in the list based on
;the comperator. If the comparator returns true, 
;the first argument to the comparator is 'greater' than the second argument.
(define: (j-max [jl : JoinList$] [comp : (Number$ Number$ -> Boolean$)]) -> Number$
  (cond
    [(empty? jl) (error "List is empty")]
    [(one? jl) (get jl)]
    [(join? jl)
     (split jl (lambda (jl1 jl2)
                 (let ([1max (j-max jl1 comp)]
                       [2max (j-max jl2 comp)])
                    (if (comp 1max 2max) 1max 2max))))]))

;j-max tests:
(check-expect (j-max testlist1 >) 79)
(check-expect (j-max testlist3 >) 382)
(check-expect (j-max testlist3 <) -1) ;change the comperator to the opposite to show it will still work
(check-error (j-max empty <))



;j-length: JoinList$ -> Number$
;given a JoinList$, outputs the length of the list
(define: (j-length [jl : JoinList$]) -> Number$
  (cond
    [(empty? jl) 0]
    [(one? jl) 1]
    [(join? jl) (split jl (lambda (jl1 jl2)
                            (+ (j-length jl1) (j-length jl2))))]))

;j-length tests:
(check-expect (j-length testlist1) 7)
(check-expect (j-length testlist2) 6)
(check-expect (j-length testlist3) 5)
(check-expect (j-length empty) 0)



;j-first : JoinList$ -> Number$
;given a JoinList$, outputs the first number in the list
(define: (j-first [jl : JoinList$]) -> Number$
  (cond
    [(empty? jl) (error "List is empty")]
    [(one? jl) (get jl)]
    [else 
     (split jl (λ (jl1 jl2)
                 (cond
                   [(one? jl1) (get jl1)]
                   [(join? jl1) (j-first jl1)])))]))

;j-first tests:
(check-expect (j-first testlist1) 2)
(check-expect (j-first testlist2) 3)
(check-expect (j-first testlist3) 39)
(check-error (j-first empty))


;j-rest: JoinList$ -> JoinList$
;given a JoinList$, outputs a JoinLest$ containing all elements but the first 
(define: (j-rest [jl : JoinList$]) -> JoinList$
  (cond
    [(empty? jl) (error "List is empty")]
    [(one? jl) empty]
    [(join? jl)
     (split jl (lambda (jl1 jl2)
              (cond
                [(one? jl1) jl2]
                [(join? jl1) (join (j-rest jl1) jl2)])))]))

;j-rest tests:
(check-expect (join-list->list (j-rest testlist1)) (list 6 2 79 2 4 7))
(check-expect (join-list->list (j-rest testlist2)) (list 12 3 5 0 9))
(check-expect (join-list->list (j-rest testlist3)) (list 5 382 2 -1))
(check-error (j-rest empty))




;j-nth: JoinList$ Number$ -> Number$
;given a JoinList$ and a number index, outputs the element number at the index
(define: (j-nth [jl : JoinList$] [index : Number$]) -> Number$
  (cond
    [(>= index (j-length jl)) (error "Sorry, invalid index")]
    [(< index 0) (error "Sorry, invalid index")]
    [(= index 0) (j-first jl)]
    [else
     (split jl (lambda (jl1 jl2)
                  (let ([len1 (j-length jl1)])
                  (cond
                    [(< index len1) (j-nth jl1 index)]
                    [else (j-nth jl2 (- index len1))]))))]))

;j-nth test:
(check-expect (j-nth testlist1 0) 2)
(check-expect (j-nth testlist1 5) 4)
(check-error (j-nth testlist1 100))
(check-expect (j-nth testlist1 3) 79)



;j-map: (Number$ -> Number$) JoinList$ -> JoinList$
;given an operator and a JoinList$, outputs a list of values having applied the operator
;to each element
(define: (j-map [op : (Number$ -> Number$)] [jl : JoinList$]) -> JoinList$
  (cond
    [(empty? jl) (error "List is empty")]
    [(one? jl) (one (op (get jl)))]
    [else 
     (split jl (lambda (jl1 jl2)
                 (join (j-map op jl1) (j-map op jl2))))]))

;j-map tests:
(check-expect (join-list->list (j-map (λ (x) (* 2 x)) testlist1)) (list 4 12 4 158 4 8 14))
(check-expect (join-list->list (j-map (λ (x) (+ 4 x)) testlist2)) (list 7 16 7 9 4 13))
(check-expect (join-list->list (j-map (λ (x) (/ x 2)) testlist3)) (list 39/2 5/2 191 1 -1/2))



;j-filter: (Number$ -> Boolean$) JoinList$ -> JoinList$
;given a predicate and a JoinList$, outputs a JoinList$ for which the predicate is true
(define (j-filter op jl)
  (cond
    [(empty? jl) (error "List is empty")]
    [(one? jl) (if (op (get jl)) jl empty)]
    [else
     (split jl (lambda (jl1 jl2)
                 (join (j-filter op jl1) (j-filter op jl2))))]))

;j-filter tests:
(check-expect (join-list->list (j-filter even? testlist1)) (list 2 6 2 2 4 ))
(check-expect (join-list->list (j-filter odd? testlist2)) (list 3 3 5 9))
(check-expect (join-list->list (j-filter negative? testlist3)) (list -1))



;j-sort: JoinList$ (Number$ Number$ -> Boolean$) -> JoinList$
;given a JoinList$ and a comparator (that takes two numbers and outputs a Boolean),
;outputs a sorted JoinList according to the comparator
;If the comparator returns true, then the first argument to the comparator should come before 
;the second argument in the outputted list.
;EX: for increasing order, use comparator <
(define: (j-sort [jl : JoinList$] [comp : (Number$ Number$ -> Boolean$)]) -> JoinList$
  (cond
    [(one? jl) jl]
    [else
     (split jl (lambda (jl1 jl2)
                 (cond
                   [(and (one? jl1) (one? jl2)) 
                    (if (comp (get jl1) (get jl2)) (join jl1 jl2) (join jl2 jl1))]
                   [else (merge (j-sort jl1 comp) (j-sort jl2 comp) comp)])))]))

;j-sort tests:
(check-expect (join-list->list (j-sort testlist1 <)) (list 2 2 2 4 6 7 79))
(check-expect (join-list->list (j-sort testlist2 <)) (list 0 3 3 5 9 12))
(check-expect (join-list->list (j-sort testlist3 <)) (list -1 2 5 39 382))
(check-expect (join-list->list (j-sort testlist3 >)) (list 382 39 5 2 -1)) ; to show it can be sorted decreasingly



;merge: JoinList$ JoinList$ (Number$ Number$ -> Boolean$) -> JoinList$
;given two already sorted JoinLists that were sorted with the comparator,
;outputs a combined sorted JoinList$ of the two according to the comparator.
;If the comparator returns true, then the first argument to the comparator should come before 
;the second argument in the outputted list.
(define: (merge [jl1 : JoinList$] [jl2 : JoinList$] [comp : (Number$ Number$ -> Boolean$)]) -> JoinList$
    (cond
      [(and (empty? jl1) (empty? jl2))
       empty]
      [(empty? jl1) jl2]
      [(empty? jl2) jl1]
      [else
       (let ([1first (j-first jl1)]
             [2first  (j-first jl2)])
         (cond
           [(comp 1first 2first)
            (join (one 1first) (merge (j-rest jl1) jl2 comp))]
           [(comp 2first 1first) 
            (join (one 2first) (merge jl1 (j-rest jl2) comp))]
           [else ;;this is if there is a duplicate, then it doesn't matter which goes first
            (join (one 1first) (merge (j-rest jl1) jl2 comp))]))])) 

;merge tests:
;merge tests list:
(define pt1list (list->join-list (list 2 5 7 11 12)))
(define pt2list (list->join-list (list -1 6 100)))
(define pt3list (list->join-list (list 50 61 100)))
(define pt4list (list->join-list (list 8 10 11)))

(check-expect (join-list->list (merge pt1list pt2list <))
              (list -1 2 5 6 7 11 12 100))
(check-expect (join-list->list (merge pt1list pt3list <))
              (list 2 5 7 11 12 50 61 100))
(check-expect (join-list->list (merge pt1list empty <))
              (list 2 5 7 11 12))
(check-expect (join-list->list (merge empty empty <))
              empty)



;j-reduce: (Number$ Number$ -> Number$) JoinList$ -> Number$
;given an operator (that takes 2 arguments and returns a number) and a JoinList,
;outputs a Number that is the product of distributing the operator across a non-empty list
(define: (j-reduce [op : (Number$ Number$ -> Number$)] [jl : JoinList$]) -> Number$
  (cond
    [(empty? jl) (error "List is empty")]
    [(one? jl) (get jl)] 
    [(one? (j-rest jl)) (op (j-first jl) (j-first (j-rest jl)))]
    [else
     (split jl (lambda (jl1 jl2)
                 (op (j-reduce op jl1) (j-reduce op jl2))))]))

;j-reduce tests:
(check-expect (j-reduce + testlist1) 102)
(check-expect (j-reduce max testlist1) 79)
(check-expect (j-reduce min testlist1) 2)







