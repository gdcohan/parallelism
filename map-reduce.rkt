#lang planet cs019/cs019

;Authors: Patrick Gunawan and Gregory Cohan

;map-reduce is a function that enables certain 
;tasks to be subdivided,processed on multiple
;machines, and then recombined. The beginning
;of this program develops a map-reduce 
;template that can then be used
;to solve various problems
(require "map-reduce-support.rkt")

;example data

;example tvs: (make-tv "filename" "filecontent")
(define tv1 (make-tv "file1" "one two three four five"))
(define tv2 (make-tv "file2" "one two three four"))
(define tv3 (make-tv "file3" "one two three"))

;input example (listof: (make-tv "filename" "filecontent"))
(define input1 (list tv1 tv2 tv3))

;example output of the wc-map function available from the 
;support code
(define map1 (wc-map tv1))
(define map2 (wc-map tv2))

;example input into wc-reduce
(define tv1a (make-tv (tv-tag (first map1)) (list (tv-value (first map1)))))
(define tv1b (make-tv (tv-tag (first map1)) (list 6 5 2)))

;Signatures
(define tv$ (Sig: tv?))

;map-reduce takes an input (Listof: (make-tv a$ b$)), 
;a map function: (make-tv $a $b) -> (make-tv $m $n),
;and a reduce function: (make-tv $m (Listof: n$)).
;It returns a (listof: (make-tv $m $o).
;It does this by:
;1) taking an input that is a list of tvs 
;2)processing each tv in that list with the map function 
;3)turning each of the values of the resulting tvs into lists and
;4) applying the reduce function to the each
;new tv in the list.
(define: (map-reduce [input : (Listof: tv$)] [mapper : (tv$ -> (Listof: tv$))] [reducer : (tv$ -> tv$)]) -> (Listof: tv$)
  (reduce-list 
   (combiner
    (list-maker
     (map-reduce-helper input mapper))
    empty)
   reducer))

(check-expect (map-reduce input1 wc-map wc-reduce)
              (list
               (make-tv "three" 3)
               (make-tv "two" 3)
               (make-tv "one" 3)
               (make-tv "four" 2)
               (make-tv "five" 1)))

(check-expect (map-reduce 
               (list (make-tv "hello" "goodbye no goodbye") (make-tv "yes" "no goodbye"))
               wc-map wc-reduce)
              (list
               (make-tv "goodbye" 3)
               (make-tv "no" 2)))

(check-expect (map-reduce
               (list
                (make-tv "file2" "acb    d efg")
                (make-tv "one" "x")
                (make-tv "two" ""))
               wc-map
               wc-reduce)
              (list
               (make-tv "x" 1)
               (make-tv "efg" 1)
               (make-tv "d" 1)
               (make-tv "acb" 1)))

;map-reduce-helper takes an input
;(listof: Tv$) and a map function
;and applies the map function to each item in the input
(define: (map-reduce-helper [input : (Listof: tv$)] [mapper : (tv$ -> (Listof: tv$))]) -> (Listof: tv$)
  (cond
    [(empty? input) empty]
    [(cons? input)
     (append (mapper (first input))
           (map-reduce-helper (rest input) mapper))]))

(check-expect (map-reduce-helper 
               (list (make-tv "file1" "hello goodbye   hello goodbye")
                     (make-tv "file2" "yes goodbye      yes")) wc-map)
              (list
               (make-tv "hello" 1)
               (make-tv "goodbye" 1)
               (make-tv "hello" 1)
               (make-tv "goodbye" 1)
               (make-tv "yes" 1)
               (make-tv "goodbye" 1)
               (make-tv "yes" 1)))

(check-expect (map-reduce-helper empty wc-map)
              empty)

(check-expect (map-reduce-helper 
               (list (make-tv "" "")
                     (make-tv "file1" "hello")) wc-map)
              (list
               (make-tv "hello" 1)))

;list-maker takes a (listof: (make-tv String Number))
;and returns a list of tvs with the same tag and 
;a value that is the previous value but in 
;list form.
(define: (list-maker [lotvs : (Listof: tv$)]) -> (Listof: tv$)
  (cond
    [(empty? lotvs) empty]
    [(cons? lotvs)
     (cons (make-tv (tv-tag (first lotvs)) (list (tv-value (first lotvs))))
           (list-maker (rest lotvs)))]))

(check-expect (list-maker 
               (list
                (make-tv "hello" 1)
                (make-tv 3 "hello")))
              (list
               (make-tv "hello" (list 1))
               (make-tv 3 (list "hello"))))

(check-expect (list-maker
               (list
                (make-tv "hello" 1)
                (make-tv "hello" 1)))
              (list
               (make-tv "hello" (list 1))
               (make-tv "hello" (list 1))))

;combiner takes a (Listof: (make-tv String (Listof: Number))
;and concatenates the value of any tvs
;with the same tag. It does this by keeping 
;a running list of tvs already seen
;and comparing the tag of the current
;tv to the tag of the tvs in the list.
(define: (combiner [lontvs : (Listof: tv$)] [accumulated : (Listof: tv$)]) -> (Listof: tv$)
  (cond
    [(empty? lontvs) accumulated]
    [(cons? lontvs) 
      (add-or-increment (first lontvs) (combiner (rest lontvs) accumulated))]))

(check-expect (combiner 
               (list
                (make-tv "hello" (list 1))
                (make-tv "fred" (list 1 1))
                (make-tv "hello" (list 1 1))) empty)
              (list
               (make-tv "hello" (list 1 1 1))
               (make-tv "fred" (list 1 1)))) 

(check-expect (combiner
               (list
                (make-tv "hello" (list 1))
                (make-tv "fred" (list 1 1)))
               (list
                (make-tv "hello" (list 1 1))))
              (list
               (make-tv "hello" (list 1 1 1))
               (make-tv "fred" (list 1 1))))
              

;add-or-increment takes a 
;(list of (make-tv String (listof: Number))
;and concatenates the values of tvs
;with the same tag. It does this by keeping a 
;running list of tvs that have already
;been processed and checking the current
;tv (and its tag) against the tags of the
;tvs on the list. If it finds a tv on the
;list with the same tag, it appends the value
;to the value of the tv on the list. If it doesn't
;find a tv in the list with the same tag,
;it adds the tv to the list. 
(define: (add-or-increment [ntv : tv$] [lontvs : (Listof: tv$)]) -> (Listof: tv$)
  (cond
    [(empty? lontvs) (list ntv)]
    [(cons? lontvs)
     (cond
       [(equal? (tv-tag ntv) (tv-tag (first lontvs)))
        (cons 
         (make-tv (tv-tag ntv) (append (tv-value ntv) (tv-value (first lontvs))))
         (rest lontvs))]
       [else
        (cons
         (first lontvs)
         (add-or-increment ntv (rest lontvs)))])]))

(check-expect (add-or-increment
               (make-tv "hello" (list 1 1))
               empty)
              (list
               (make-tv "hello" (list 1 1))))

(check-expect (add-or-increment
               (make-tv "hello" (list 1))
               (list
                (make-tv "greg" (list 2 2))
                (make-tv "fred" (list 2 3))))
              (list
               (make-tv "greg" (list 2 2))
               (make-tv "fred" (list 2 3))
               (make-tv "hello" (list 1))))

(check-expect (add-or-increment
               (make-tv "hello" (list 2 3))
               (list 
                (make-tv "greg" (list 1 2))
                (make-tv "hello" (list 4 2))))
              (list
               (make-tv "greg" (list 1 2))
               (make-tv "hello" (list 2 3 4 2))))

;reduce-list takes a 
;(listof: (make-tv String (listof: Number))
;and returns a (listof: (make-tv String Number)
;where the value of the tvs in the output 
;are the length of the lists in the input.
;it does this applying the reduce 
;function to each tv in the list.
(define: (reduce-list [lontvs : (Listof: tv$)] [reducer : (tv$ -> tv$)]) -> (Listof: tv$)
  (cond
    [(empty? lontvs) empty]
    [(cons? lontvs) 
     (cons (reducer (first lontvs))
           (reduce-list (rest lontvs) reducer))]))

(check-expect (reduce-list 
               (list
                (make-tv "hello" (list 1 2))) 
               wc-reduce)
              (list
               (make-tv "hello" 2)))

(check-expect (reduce-list
               (list
                (make-tv "hello" (list 1 2))
                (make-tv "goodbye" (list 3 2)))
               wc-reduce)
              (list
               (make-tv "hello" 2)
               (make-tv "goodbye" 2)))

;;;;

;Anagrams

;anagram-map and anagram-reduce are
;designed such that if map reduce is given
;a (listof: (make-tv String String)), 
;the output will be a 
;(listof: (make-tv String String)) 
;where the tag is a group of characters 
;and the value is all of the words that appeared
;in any of the inputs that are anagrams
;of that group of letters. 

;example data
(define anagramtv1 (make-tv "file1" "cat act dog god frog tac"))
(define anagramtv2 (make-tv "file2" "log tac god apple cat"))

(define anagramtestinput (list anagramtv1 anagramtv2))

;anagram-map takes (make-tv String String) and returns a 
;(listof: (make-tv String String) where the tag
;is the alphabetized characters of each word in 
;the filecontents and the value is the original word that
;was in the file. It works by first applying the 
;wc-map function from the support file to the tv. It
;then breaks the value of the resulting tv into a 
;list of each of the characters in that value. It then 
;alphabetizes that string and sets it as the tag,
;while it sets the original word as the value.
(define: (anagram-map [tv : tv$]) -> (Listof: tv$)
  (aas (wc-map tv)))

(check-expect 
 (anagram-map 
  (make-tv "one" "one two three"))
 (list
  (make-tv "eno" "one")
  (make-tv "otw" "two")
  (make-tv "eehrt" "three")))

(check-expect
 (anagram-map
  (make-tv "one" ""))
 empty)

(check-expect
 (map-reduce anagramtestinput anagram-map anagram-reduce)
 (list
  (make-tv "act" (list "cat" "act" "tac"))
  (make-tv "aelpp" (list "apple"))
  (make-tv "dgo" (list "dog" "god"))
  (make-tv "glo" (list "log"))
  (make-tv "fgor" (list "frog"))))
 

;aas (alphabetize and sort) takes a
;(listof: (make-tv String String))
;and processes them so that it returns
;a list of tvs where the tag is 
;all the letters in the word in alphabetical
;order and the value is the orginal word
(define: (aas [lotvs : (Listof: tv$)]) -> (Listof: tv$)
  (cond
    [(empty? lotvs) empty]
    [(cons? lotvs) 
     (cons
      (convert (first lotvs))
      (aas (rest lotvs)))]))

(check-expect
 (aas 
  (list
   (make-tv "hello" "goodbye")
   (make-tv "one" "two")))
 (list
  (make-tv "ehllo" "hello")
  (make-tv "eno" "one")))

(check-expect 
 (aas
  (list 
   (make-tv "" "")))
 (list
  (make-tv "" "")))

;convert takes a (make-tv String Number) and returns a tv
;where the tag is the string in alphabetical order 
;and the value is the original string
(define: (convert [tv : tv$]) -> tv$
  (make-tv 
   (alphabetize (tv-tag tv))
   (tv-tag tv)))

(check-expect
 (convert (make-tv "hello" (list 1)))
          (make-tv "ehllo" "hello"))

(check-expect
 (convert
  (make-tv "" (list 1)))
  (make-tv "" ""))

;alphabetize takes a string and returns a string
;with the letters of the original string
;in alphabetical order
(define: (alphabetize [str : String$]) -> String$
  (foldr string-append "" (sort (stringparser str) string<=?)))

(check-expect
 (alphabetize "hello")
 "ehllo")
(check-expect
 (alphabetize "")
 "")
(check-expect 
 (alphabetize "abaab")
 "aaabb")

;stringparser takes a string and returns a
;list of all of the letters in the string.
;It does so by adding the first letter
;of the string onto the recursion on the
;rest of the string.
(define: (stringparser [string : String$]) -> (Listof: String$)
  (cond
    [(string=? string "") empty]
    [else 
     (cons (substring string 0 1) (stringparser (substring string 1 (string-length string))))]))

(check-expect
 (stringparser "hello")
 (list "h" "e" "l" "l" "o"))

(check-expect 
 (stringparser "")
 empty)

(check-expect
 (stringparser "abcab")
 (list "a" "b" "c" "a" "b"))

;anagram-reduce takes (make-tv String (listof: String)) 
;and returns a tv with the same tag and 
;a new list of strings that has removed 
;the duplicates from the original list
(define: (anagram-reduce [tv : tv$]) -> tv$
  (make-tv
   (tv-tag tv)
   (remove-dupes-list (tv-value tv))))

(check-expect
 (anagram-reduce
  (make-tv "hello" (list "hello" "goodbye" "hello")))
 (make-tv "hello" (list "hello" "goodbye")))

(check-expect
 (anagram-reduce
  (make-tv "" (list "" "goodbye" "")))
 (make-tv "" (list "" "goodbye")))

;remove-dupes-list takes a list of strings
;and returns a list of strings
;without any duplicates on it. 
;It does so by adding the first item of
;the list onto recurring on the rest of
;the list, only with the rest of the list
;filtered to remove the first element so 
;there is only one of the first element
;in the list. 
(define: (remove-dupes-list [los : (Listof: String$)]) -> (Listof: String$)
  (cond
    [(empty? los) empty]
    [(cons? los)
     (cons (first los) (remove-dupes-list (filter (λ (x) 
                                                    (not (string=? x (first los))))
                                                    (rest los))))]))

(check-expect
 (remove-dupes-list (list "hello" "goodbye" "hello" ""))
 (list "hello" "goodbye" ""))

(check-expect
 (remove-dupes-list (list ""))
 (list ""))
(check-expect 
 (remove-dupes-list empty)
 empty)


;;;
;;Nile 2.0
;;;;

;Test TV's

(define nilelist1 "JK Rowling, Harry Potter\nLewis Carroll, Alice in Wonderland\nSuzanne Collins, The Hunger Games\nDavid Wallace, Infinite Jest\n")
(define nilelist2 "Lewis Carroll, Alice in Wonderland\nKurt Vonnegut, Slaughterhouse\nGGM, OHYOS\nJK Rowling, Harry Potter\n")
(define nilelist3 "Suzanne Collins, The Hunger Games\nJK Rowling, Harry Potter\nLewis Carroll, Alice in Wonderland\n")

(define niletv1 (make-tv "file1" nilelist1))
(define niletv2 (make-tv "file2" nilelist2))
(define niletv3 (make-tv "file3" nilelist3))

(define nileinput (list niletv1 niletv2 niletv3))

;popular-pairs implementations:

;popular-pairs:
;(Listof (tv String String)) -> (tv Integer (Listof String))
;takes a list of (make-tv "filename" "filecontents")
;and returns a tv where the tag is the frequency
;of the most popular pairing and the value is the 
;list of most popular pairings
(define: (popular-pairs [lotvs : (Listof: tv$)]) -> tv$
  (process-maxes
   (find-max-tag
    (map-reduce lotvs nile-map-pair nile-reduce-pairs)
    empty)
   empty))

;popular-pairs tests:
(check-expect (popular-pairs nileinput)
              (make-tv 3 
                       (list
                        "JK Rowling, Harry Potter+Lewis Carroll, Alice in Wonderland")))
(check-error (popular-pairs empty)) ; checks if it errors out if input is empty


;nile-map-pair: tv$ -> (Listof: tv$)
;takes a tv with the filename as the tag
;and the filecontent as the value
;and returns a list of tvs where the tag
;of each tv is a pair of book names
;(put in alphabetical order) that appear together 
;in the filecontents, and the value is just 1,
;used for a marker just like in word count example.

;The function works by first parsing the filecontent,
;then sorting it alphabetically, then pairing
;every book that appears in a common list, then
;making a tv from those pairings
(define: (nile-map-pair [tv : tv$]) -> (Listof: tv$)
  (make-paired-tags-list (pair-list (sort (parse-words (tv-value tv)) string<=?) empty)))

;nile-map-pair tests:
(check-expect (nile-map-pair niletv1)
              (list
 (make-tv "Lewis Carroll, Alice in Wonderland+Suzanne Collins, The Hunger Games" 1)
 (make-tv "JK Rowling, Harry Potter+Lewis Carroll, Alice in Wonderland" 1)
 (make-tv "JK Rowling, Harry Potter+Suzanne Collins, The Hunger Games" 1)
 (make-tv "David Wallace, Infinite Jest+JK Rowling, Harry Potter" 1)
 (make-tv "David Wallace, Infinite Jest+Lewis Carroll, Alice in Wonderland" 1)
 (make-tv "David Wallace, Infinite Jest+Suzanne Collins, The Hunger Games" 1)))
(check-expect (nile-map-pair niletv2)
              (list
 (make-tv "Kurt Vonnegut, Slaughterhouse+Lewis Carroll, Alice in Wonderland" 1)
 (make-tv "JK Rowling, Harry Potter+Kurt Vonnegut, Slaughterhouse" 1)
 (make-tv "JK Rowling, Harry Potter+Lewis Carroll, Alice in Wonderland" 1)
 (make-tv "GGM, OHYOS+JK Rowling, Harry Potter" 1)
 (make-tv "GGM, OHYOS+Kurt Vonnegut, Slaughterhouse" 1)
 (make-tv "GGM, OHYOS+Lewis Carroll, Alice in Wonderland" 1)))
(check-expect (nile-map-pair niletv3) 
              (list
 (make-tv "Lewis Carroll, Alice in Wonderland+Suzanne Collins, The Hunger Games" 1)
 (make-tv "JK Rowling, Harry Potter+Lewis Carroll, Alice in Wonderland" 1)
 (make-tv "JK Rowling, Harry Potter+Suzanne Collins, The Hunger Games" 1)))



;parse-words: String$ -> (Listof: String$)
;takes a string with "\n" as the delimiter
;between book names and returns a list of
;strings where the strings are the authors
;and titles. This function borrows heavily
;from the map-reduce support code.
(define: (parse-words [text : String$]) -> (Listof: String$)
  (cond
    [(string=? "" text) empty]
    [else (local [(define word (take-til "\n" text))]
            (cons word
                (parse-words (rmv-newlines
                              (substring text (string-length word))))))]))

;parse-words tests:
(check-expect (parse-words nilelist1)
(list
 "JK Rowling, Harry Potter"
 "Lewis Carroll, Alice in Wonderland"
 "Suzanne Collins, The Hunger Games"
 "David Wallace, Infinite Jest"))
(check-expect (parse-words nilelist2)
(list
 "Lewis Carroll, Alice in Wonderland"
 "Kurt Vonnegut, Slaughterhouse"
 "GGM, OHYOS"
 "JK Rowling, Harry Potter"))
(check-expect (parse-words nilelist3)
(list
 "Suzanne Collins, The Hunger Games"
 "JK Rowling, Harry Potter"
 "Lewis Carroll, Alice in Wonderland"))
(check-expect (parse-words "") empty)



;take-til: String$ String$ -> String$
;takes a delimiter String and another string
;and returns the string prior to 
;the delimiter. This function is the same as
;the function written in the map-reduce
;support code.
(define: (take-til [c : String$] [str : String$]) -> String$
  (cond
    [(string=? "" str) ""]
    [(string=? c (substring str 0 1)) ""]
    [else (string-append (substring str 0 1)  
                         (take-til c (substring str 1)))]))

;take-til tests:
(check-expect
 (take-til "." "Hi my name is Pat.What is your name?") 
 "Hi my name is Pat")
(check-expect
 (take-til "n" "Hi my name is Pat.What is your name?")
 "Hi my ")
(check-expect
 (take-til "!" "Hi my name is Pat.What is your name?")
 "Hi my name is Pat.What is your name?")

;rmv-newlines: String$ -> String$
;takes a string and outputs a String
;having removed "\n" if it is in the string.
;this too borrows heavily from the support code.
(define: (rmv-newlines [str : String$]) -> String$
  (cond
    [(string=? "\n" str) ""]
    [(string=? (substring str 0 1) "\n") (rmv-newlines (substring str 1))] 
    [else str]))                                                           

;rmv-newlines tests:
(check-expect
 (rmv-newlines "\nHello there")
 "Hello there")
(check-expect
 (rmv-newlines "Hello there")
 "Hello there")
(check-expect 
 (rmv-newlines "\n\nHello there")
 "Hello there")


;pair-list: (Listof: String$) (Listof: tv$) -> (Listof: String$)
;given a list of books and a list of books
;(it starts empty), it returns a list of tvs
;where the tag is the name of one book and 
;the value is the name of another book 
;that appears in the list. The function pairs
;every book with every other book on the list.
(define: (pair-list [lobs : (Listof: String$)] [accum : (Listof: tv$)]) -> (Listof: tv$)
  (cond
    [(empty? lobs) accum]
    [else
     (pair-list (rest lobs)
      (pair-book-to-list (first lobs) (rest lobs) accum))]))

;pair-list tests:
(check-expect (pair-list (list "a" "b" "c") empty) 
              (list (make-tv "b" "c") (make-tv "a" "b") (make-tv "a" "c")))
(check-expect (pair-list (list "a" "b" "c" "d" "e")  empty)
              (list
               (make-tv "d" "e")
               (make-tv "c" "d")
               (make-tv "c" "e")
               (make-tv "b" "c")
               (make-tv "b" "d")
               (make-tv "b" "e")
               (make-tv "a" "b")
               (make-tv "a" "c")
               (make-tv "a" "d")
               (make-tv "a" "e")))
(check-expect (pair-list empty empty) empty) 


;pair-book-to-list: String$ (Listof: String$) (Listof: tv$) -> (Listof: tv$) 
;takes a book string, a list of book strings,
;and an accumulated list of tvs (starts empty)
;and returns the accumulated list with
;new tvs in it. These new tvs consist of the book as 
;the tag and each of the listofbooks as a value.
;For example, if pair-book-to-list were passed
;"harry potter" (list "alice in wonderland" "infinite jest")
;empty, it would return: 
;(list (make-tv "harry potter" "alice in wonderland") (make-tv "harry potter" "infinite jest")).
(define: (pair-book-to-list [book : String$] [lobs : (Listof: String$)] [accum : (Listof: tv$)]) -> (Listof: tv$)
  (cond
    [(empty? lobs) accum]
    [(cons? lobs)
     (cons
      (make-tv book (first lobs))
      (pair-book-to-list book (rest lobs) accum))]))

;pair-book-to-list tests:
(check-expect
 (pair-book-to-list
  "harry potter" 
  (list "alice in wonderland" "infinite jest")
  empty)
 (list 
  (make-tv "harry potter" "alice in wonderland") 
  (make-tv "harry potter" "infinite jest")))
(check-expect 
 (pair-book-to-list 
  "a"
  (list "b" "c" "d" "e")
  empty)
 (list
  (make-tv "a" "b") 
  (make-tv "a" "c")
  (make-tv "a" "d")
  (make-tv "a" "e")))
(check-expect
 (pair-book-to-list
  "a"
  empty
  empty) empty)



;make-paired-tags-list: (Listof: tv) -> (Listof: tv)
;given a list of tvs where the tag is one
;book name and the value is another book
;name with which it is paired,
;outputs a list of tvs in the form of
;bookname+bookname as the tag and 1
;as the value for each tv
(define: (make-paired-tags-list [lotvs : (Listof: tv$)]) -> (Listof: tv$)
  (local [(define (make-pair-tag tv) ; creates the new tag from the old tv tag and value 
            (make-tv (string-append (tv-tag tv) "+" (tv-value tv)) 1))]
  (cond
    [(empty? lotvs) empty]
    [(cons? lotvs)
     (cons
      (make-pair-tag (first lotvs))
      (make-paired-tags-list (rest lotvs)))])))

;make-paired-tags-list tests:
(check-expect
 (make-paired-tags-list
  (list
   (make-tv "b" "c")
   (make-tv "a" "b")
   (make-tv "a" "c")))
 (list
  (make-tv "b+c" 1)
  (make-tv "a+b" 1)
  (make-tv "a+c" 1)))
(check-expect 
 (make-paired-tags-list 
  (list
   (make-tv "q" "d")
   (make-tv "l" "w")
   (make-tv "q" "d")
   (make-tv "q" "d")))
 (list
  (make-tv "q+d" 1)
  (make-tv "l+w" 1) 
  (make-tv "q+d" 1)
  (make-tv "q+d" 1)))
(check-expect 
 (make-paired-tags-list empty) 
 empty)
                                           


;find-max-tag: (Listof: tv$) (Listof: tv$) -> (Listof: tv$)
;takes a list of tvs where the tag is the
;frequency of that book/bookpair
;and an accumulated list (starts as empty)
;and returns a list of tvs that
;are only the tvs in the original
;list with the highest tag. This function 
;also will return an error if it is given
;an empty list, which becomes relevant
;if someone uses the recommend function
;with a book that is not in any of
;the files
(define: (find-max-tag [lotvs : (Listof: tv$)] [accum : (Listof: tv$)]) -> (Listof: tv$)
  (cond
    [(empty? lotvs) (error "Book Not Found")]
    [(empty? (rest lotvs)) (cons (first lotvs) accum)]
    [else
     (cond
       [(> (tv-tag (first lotvs)) (tv-tag (first (rest lotvs))))
        (find-max-tag 
         (append (list (first lotvs)) (rest (rest lotvs)))
         accum)]
       [(= (tv-tag (first lotvs)) (tv-tag (first (rest lotvs))))
        (find-max-tag
         (rest lotvs) (cons (first lotvs) accum))]
       [else
        (find-max-tag (rest lotvs) empty)])]))

;find-max-tag tests:
(check-expect
 (find-max-tag 
  (list
   (make-tv 3 "Harry+paul")
   (make-tv 2 "greg+bob")
   (make-tv 2 "pat+em")) 
  empty)
 (list
  (make-tv 3 "Harry+paul")))
(check-expect
 (find-max-tag
  (list
   (make-tv 3 "Harry+paul")
   (make-tv 2 "greg+bob")
   (make-tv 2 "pat+em") 
   (make-tv 3 "joe+christina"))
  empty)
 (list
  (make-tv 3 "joe+christina") 
  (make-tv 3 "Harry+paul")))
(check-error (find-max-tag empty))



;process-maxes: (Listof: tv$) (Listof: String$) -> tv$
;given a non-empty list of tvs
;(with all the same tag) and an accumulated
;list of bookpair Strings, 
;outputs a tv where the tag
;is the same as the tags from the original list
;and the value is the accumulated list 
;of bookpair strings of all the tv-values
;found in the input list of tvs
(define: (process-maxes [lotvs : (Listof: tv$)] [accum : (Listof: String$)]) -> tv$
  (cond
    [(empty? lotvs) accum]
    [(empty? (rest lotvs))
     (make-tv 
      (tv-tag (first lotvs))
      (cons (tv-value (first lotvs)) accum))]
    [else
     (process-maxes (rest lotvs) (cons (tv-value (first lotvs)) accum))]))

;process-maxes tests:
(check-expect
 (process-maxes
  (list
   (make-tv 3 "Harry+paul")
   (make-tv 3 "greg+bob")
   (make-tv 3 "pat+em"))
  empty)
 (make-tv 3 (list "pat+em" "greg+bob" "Harry+paul")))
(check-expect
 (process-maxes 
  (list 
   (make-tv 2 "a+b")
   (make-tv 2 "c+d")
   (make-tv 2 "v+w")
   (make-tv 2 "me+you"))
  empty)
 (make-tv 2 (list "me+you" "v+w" "c+d" "a+b")))



;nile-reduce-pairs: tv$ -> tv$
;takes a tv with a string tag and
;a (listof: String$)) value and returns a 
;tv with the length of the list of
;strings as the tag and the original
;tag as the new value
(define: (nile-reduce-pairs [tv : tv$]) -> tv$
  (make-tv
   (length (tv-value tv))
   (tv-tag tv)))

;nile-reduce-pairs tests:
(check-expect (nile-reduce-pairs (make-tv "Alice+Harry Potter" (list 1 1)))
              (make-tv 2 "Alice+Harry Potter"))
(check-expect (nile-reduce-pairs (make-tv "Alice+Harry Potter" (list 1 1 1 1 1)))
              (make-tv 5 "Alice+Harry Potter"))
(check-expect (nile-reduce-pairs (make-tv "Alice+Harry Potter" empty))
              (make-tv 0 "Alice+Harry Potter"))



;;Recommend implementations:

;recommend: String$ (Listof: tv$) -> tv$
;takes a book string and a list of 
;tvs(filename filecontents) and
;returns a tv(frequency (listof booknames))
;where the booknames are the books
;that have been most frequently paired
;with the book that is passed to the function
(define: (recommend [book : String$] [lotvs : (Listof: tv$)]) -> tv$
  ;the following local mapper function that is created
  ;will take a tv(filename filecontents)
  ;and will return a list of tvs(bookname 1)
  ;where the booknames are those that have
  ;been paired with the book the original 
  ;create-mapper function is passed. 
  [local [(define (create-mapper book) ; creates a mapper function depending on the passed in book
            (lambda (x)
              (make-val-new-tag ;makes a new tv with the tag now the complementary book to the input book
               (filter ; filters only keeping pair tvs with the book in them
                     (lambda (x) (or (string=? (tv-tag x) book) (string=? (tv-value x) book)))
                     (pair-list (sort (parse-words (tv-value x)) string<=?) empty)) book)))] ;uses the same parsing/pairing
    (process-maxes                                                                           ;as popular-pairs
     (find-max-tag (map-reduce lotvs (create-mapper book) nile-reduce-recommend) empty) empty)])

;recommend tests:
(check-expect 
 (recommend "JK Rowling, Harry Potter" nileinput)
 (make-tv 3 (list "Lewis Carroll, Alice in Wonderland")))
(check-expect 
 (recommend "Lewis Carroll, Alice in Wonderland" nileinput)
 (make-tv 3 (list "JK Rowling, Harry Potter")))
(check-expect
 (recommend "Kurt Vonnegut, Slaughterhouse" nileinput)
 (make-tv 1 (list "GGM, OHYOS" "JK Rowling, Harry Potter" "Lewis Carroll, Alice in Wonderland")))
(check-error (recommend "non-existent book" nileinput))



;make-val-new-tag: (Listof: tv$) String$ -> (Listof: tv$)
;takes a list of tvs(bookname bookname) and 
;a bookname string that is within all the tvs
;and returns a list of tvs where the new tag
;was the complementary bookname string to the input book 
;and the value is simply 1 for counting
(define: (make-val-new-tag [lotvs : (Listof: tv$)] [book : String$]) -> (Listof: tv$)
  (cond
    [(empty? lotvs) empty]
    [(cons? lotvs)
     (cond
       [(string=? (tv-tag (first lotvs)) book)
        (cons 
         (make-tv (tv-value (first lotvs)) 1)
         (make-val-new-tag (rest lotvs) book))]
       [else
        (cons
         (make-tv (tv-tag (first lotvs)) 1)
         (make-val-new-tag (rest lotvs) book))])]))

;make-val-new-tag tests:
(check-expect 
 (make-val-new-tag
  (list
   (make-tv "hello" "goodbye")
   (make-tv "goodbye" "fred")
   (make-tv "greg" "goodbye"))
  "goodbye")
 (list 
  (make-tv "hello" 1)
  (make-tv "fred" 1)
  (make-tv "greg" 1)))     
(check-expect
 (make-val-new-tag
  (list 
   (make-tv "hi" "steve")
   (make-tv "mr" "hi")
   (make-tv "lizza" "hi"))
  "hi")
 (list
  (make-tv "steve" 1)
  (make-tv "mr" 1) 
  (make-tv "lizza" 1)))
(check-expect
 (make-val-new-tag empty "Hi")
 empty)


;nile-reduce-recommend: tv$ -> tv$
;takes a tv where the tag is the bookname
;and the value are a list of 1's,
;outputs a tv where the tag is the length
;of the original value list and the old
;tag is now the new value.
(define: (nile-reduce-recommend [tv : tv$]) -> tv$
  (make-tv
   (length (tv-value tv))
   (tv-tag tv)))

;nile-reduce-recommend tests:
(check-expect
 (nile-reduce-recommend
  (make-tv "Alice" (list 1 1 1 1 1)))
 (make-tv 5 "Alice"))
(check-expect 
 (nile-reduce-recommend
  (make-tv "Alice" (list 1 1)))
 (make-tv 2 "Alice"))
(check-expect
 (nile-reduce-recommend
  (make-tv "Alice" empty))
 (make-tv 0 "Alice"))
