#lang typed/racket

; 2010

(provide (struct-out tv)
         wc-map
         wc-reduce)

(define-struct: (a b) tv ([tag : a]  [value : b]) #:transparent)

(: wc-map ((tv String String) -> (Listof (tv String Integer))))
(define (wc-map file)
    (map (lambda: ((word : String))
           (make-tv word 1))
         (parse-words (tv-value file))))

(: wc-reduce ((tv String (Listof Integer)) -> (tv String Integer)))
(define (wc-reduce word-counts)
  (make-tv (tv-tag word-counts)
           (length (tv-value word-counts))))

(: parse-words (String -> (Listof String)))
;words can be divided by any number of spaces
(define (parse-words text)
  (cond
    [(string=? "" text) empty]
    [else (local [(define word (take-til " " text))]
            (cons word
                (parse-words (rmv-spaces
                              (substring text (string-length word))))))]))

(: rmv-spaces (String -> String))
;removes spaces from front until a letter is hit
(define (rmv-spaces str)
  (cond
    [(string=? "" str) ""]
    [(string=? (substring str 0 1) " ") (rmv-spaces (substring str 1))]
    [else str]))

(: take-til (String String -> String))
;takes from the front of a string until it hits c
(define (take-til c str)
  (cond
    [(string=? "" str) ""]
    [(string=? c (substring str 0 1)) ""]
    [else (string-append (substring str 0 1)
                         (take-til c (substring str 1)))]))