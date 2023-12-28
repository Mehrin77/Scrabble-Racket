#lang racket
(provide subbag? best-word score-letter helper-map score-word add-list valid-list valid-words best-helper best-word  );Scrabble

(define (subbag? S B) ; takes 2 list S(small) and B(big) and checks of if contains all elements of S 
  (cond [(empty? S) #t] ;base case
        [(member (first S) B) ; checks first elecment of S in B
         (subbag? (rest S) (remove (first S) B))] ;recurces and and checks consecutive 
        [else #f])) 


(define scrabble-tile-bag ;given
  '((#\a 1 9) (#\b 3 2) (#\c 3 2) (#\d 2 4) (#\e 1 12)
    (#\f 4 2) (#\g 2 3) (#\h 4 2) (#\i 1 9) (#\j 8 1)
    (#\k 5 1) (#\l 1 4) (#\m 3 2) (#\n 1 6) (#\o 1 8)
    (#\p 3 2) (#\q 10 1) (#\r 1 6) (#\s 1 4) (#\t 1 6)
    (#\u 1 4) (#\v 4 2) (#\w 4 2) (#\x 8 1) (#\y 4 2)
    (#\z 10 1) (#\_ 0 2)) )
;; end define scrabble-tile-bag
;; The underscore is used to represent a blank tile, which is a wild-card

(define (score-letter x) ;returns sore for each letter
  (first (rest (assoc x scrabble-tile-bag)))) ;looks up dictionary ;gets 2nd element

(define (helper-map func L) ;helps map a func (function) to an entire list
  (if (empty? L) L ; base case
      (cons (func (first L)) ; Apply teh function to first element andmake list
            (helper-map func (rest L))))) ; Recurse the restof the list

(define (score-word w) ; applies the scoreword on everyletter of the word
  (add-list (helper-map score-letter ;applies scoreletter on every element then adds them up
                 (string->list w)))) ; turns te strings into list of chaarcters

(define (add-list L) ;helper fun. to add elements in alist
  (if (empty? L) 0 ;base case
      (+ (first L) (add-list (rest L))))) ;recurxes and add the rest

(define (valid-list WL rack) ;
  (cond
    [(empty? WL) WL] ; base case
    [(subbag? (first WL) rack) ;checks if first word is possible using rack
     (cons (first WL) (valid-list (rest WL) rack))] ;recurses and stores possible words
    [else (valid-list (rest WL) rack)])) ;skips invalid words and recurses

(define (valid-words WL rack) ;helper func. to run valid list and return list of words
  (helper-map list->string ; converts lst of lst to lst of string 
       (valid-list (helper-map string->list WL) (string->list rack)))) ;converts string to list and calls helper

(define (best-helper L best top) ;ranks the best words 
  (cond [(empty? L) (list best top)] ; base case 
        [(> (score-word (first L)) top) ;if score> highest score 
            (best-helper (rest L) (first L) (score-word (first L)))] ; recurces with new high score
        [else (best-helper (rest L) best top)])) ; recurses with old high scoere

(define (best-word rack WL) ;scrabble func. 
  (best-helper (valid-words WL rack) "" 0)) ;calls helper

  