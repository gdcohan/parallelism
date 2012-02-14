#lang racket

(define-struct one (elt))
(define-struct join-list (left right size) #:mutable)

;; is-join-list?: any/c -> boolean
;; takes a datum and returns true if it is a join list
;; false otherwise
(define (is-join-list? dat)
  (or (empty? dat)
      (one? dat)
      (join? dat)))

;; new-cons: any/c list -> list
;; redefines cons to throw an error when used with append lists
;; to be exported as "cons"
(define (new-cons elt lst)
  (cond
    [(or (one? lst) (join? lst))
     (error 'cons "do not use cons with join lists! given ~a and ~a" elt lst)]
    [(list? lst) (cons elt lst)]
    [else (error 'cons "second argument must be of type <list>, given ~a and ~a" elt lst)]))

;; join?: any/c -> boolean
;; takes a datum and returns true if it is a join list with multiple elements
;;    (a join-list struct), returns false otherwise
(define (join? lst)
  (join-list? lst))

;; size: join-list -> number
;; returns the size of the join-list
(define (size lst)
  (cond
    [(empty? lst) 0]
    [(one? lst) 1]
    [(join? lst) (join-list-size lst)]))

;; get: one -> any/c
;; consumes a singleton join list and returns the data contained within
(define (get lst)
  (if (one? lst)
      (one-elt lst)
      (error 'get "expected one-element join list, found ~a" lst)))

;; join: join-list join-list -> join-list
;; consumes two join lists and joins them together into a single list
(define (join lst1 lst2)
  (cond
    [(not (is-join-list? lst1))
     (error 'join
            "expected first argument of type <join-list>, found ~a. other argument: ~a"
            lst1
            lst2)]
    [(not (is-join-list? lst2))
     (error 'join
            "expected second argument of type <join-list>, found ~a. other argument: ~a"
            lst2
            lst1)]
    [(empty? lst1) lst2]
    [(empty? lst2) lst1]
    [else (make-join-list lst1 lst2 (+ (size lst1) (size lst2)))]))

;; get-moved: join-list join-list number number -> join-list join-list
;; consumes two join lists, a maximum number of elements to move, and a direction to move them
;; produces two join lists, the first is the left half after moving the specified number of
;;    elements in the appropriate direction
;;    the second is the right half
(define (get-moved left right num the-dir)
  (local [;; get-sub-lists: join-list dir -> join-list join-list
          ;; consumes a join-list and a direction
          ;; returns two join-lists where the first either a prefix (the-dir = 0) or
	  ;;    a suffix (the-dir = 1) of the inputted list with length no greater than num
          ;;    the second is the other part of the inputted list
          (define (get-sub-lists a-list dir)
            (if (<= (size a-list) num)
                (values a-list empty)
                (let-values ([(sub-list new-side) (get-sub-lists (dir a-list) dir)])
                  (values sub-list
                          (if (= the-dir 0)
                              (join new-side (join-list-right a-list))
                              (join (join-list-left a-list) new-side))))))]
    (let-values ([(moved stayed) (if (= the-dir 0)
                                     (get-sub-lists right join-list-left)
                                     (get-sub-lists left join-list-right))])
      (if (= the-dir 0)
          (values (join left moved) stayed)
          (values stayed (join moved right))))))

;; split: join-list (join-list join-list -> a) -> a
;; consumes a join-list with multiple elements and a handler
;; the handler takes two halves of a list and does a computation
(define (split lst proc)
  (if (not (join? lst))
      (error 'split "expected first argument of type <join-list> with multiple elements, found ~a. other argument: ~a" lst proc)
      (let* ([left (join-list-left lst)]
             [right (join-list-right lst)]
             [left-size (size left)]
             [right-size (size right)])
        (let-values
            ([(new-left new-right)
              (cond
                [(= (size lst) 2) (values left right)] ;; do nothing
                [(= left-size right-size) (get-moved left right 1 (random 2))]
                [(> left-size right-size) (get-moved left
                                                     right
                                                     (+ (random (- left-size right-size)) 1)
                                                     1)]
                [(< left-size right-size) (get-moved left
                                                     right
                                                     (+ (random (- right-size left-size)) 1)
                                                     0)])])
          (set-join-list-left! lst new-left)
          (set-join-list-right! lst new-right)
          (proc (join-list-left lst) (join-list-right lst))))))

;; list->join-list: (listof any/c) -> join-list
;; consumes a scheme list and outputs a mostly-balanced join list representation
;;    of that same list
(define (list->join-list lst)
  (when (not (list? lst)) (error 'list->join-list
                                 "expected argument of type <list>, found ~a"
                                 lst))
  (local
    [(define (list->join-list-help a-list len)
       (cond
         [(= len 0) (list empty a-list)]
         [(= len 1) (list (make-one (first a-list)) (rest a-list))]
         [(= len 2) (list (join (make-one (first a-list))
                                (make-one (second a-list)))
                          (rest (rest a-list)))]
         [else (let* ([range (inexact->exact (max 3 (floor (* 2 (log len)))))]
                      [rand (- (random range) (quotient range 2))]
                      [left-output (list->join-list-help a-list
                                                         (- (floor (/ len 2)) rand))]
                      [right-output (list->join-list-help (second left-output)
                                                          (+ (ceiling (/ len 2)) rand))])
                 (list (join (first left-output) (first right-output))
                       (second right-output)))]))
     (define constructed (list->join-list-help lst (length lst)))]
    (if (not (empty? (second constructed)))
        (error 'list->join-list "something went wrong with join-list construction")
        (first constructed))))

;; join-list->list: join-list -> list
;; consumes a join list and converts it into a scheme list representation
;; note: this does not call split
(define (join-list->list lst)
  (if (is-join-list? lst)
      (local [(define (join-list->list-help a-list acc)
                (cond
                  [(empty? a-list) acc]
                  [(one? a-list) (cons (get a-list) acc)]
                  [else (join-list->list-help (join-list-left a-list)
                                              (join-list->list-help (join-list-right a-list)
                                                                    acc))]))]
        (join-list->list-help lst empty))
      (error 'join-list->list "expected argument of type <join-list>, found ~a" lst)))

;; join-list=?: join-list join-list -> boolean
;; consumes two join-lists and returns true if they have the same elements in the same order
(define (join-list=? lst1 lst2)
  (cond
    [(not (is-join-list? lst1))
     (error 'join-list=?
            "expected first argument of type <join-list>, found ~a. other argument: ~a"
            lst1
            lst2)]
    [(not (is-join-list? lst2))
     (error 'join-list=?
            "expected second argument of type <join-list>, found ~a. other argument: ~a"
            lst2
            lst1)]
    [else (equal? (join-list->list lst1) (join-list->list lst2))]))

;; provide statement
(provide (rename-out (new-cons cons))
         (rename-out (make-one one))
         one?
         get
         join
         join?
         split
         (rename-out (is-join-list? join-list?))
         join-list=?
         list->join-list
         join-list->list)
