#lang racket
(require racket/match)

(define/contract (full-justify words maxWidth)
  (-> (listof string?) exact-integer? (listof string?))
  (cond
   [(null? words) null]
   [else null])
  )

(define (pad words maxWidth)
  (let ((spaces (- maxWidth (foldl + 0 (map string-length words))))
	(words-to-pad (length (cdr words))))
    (cond
     [(null? words) null]
     [(= 1 (length words)) (string-append (car words) (make-string (- maxWidth (string-length (car words))) #\space))]
     [else (string-append (car words) (string-join (cdr words) (make-string (quotient spaces words-to-pad) #\space) #:before-first (make-string (round (/ spaces words-to-pad)) #\space)))])))

(define (justify words maxWidth)
  (match words
    ['() '()]
    [(list a) (pad a)]
    [(list a b ... c ...)
     #:when (<= (+ (string-length a) (foldl + 0 (map (compose add1 string-length) b))) maxWidth)
     (list a b c)]))

(justify '("a" "b" "c" "d" "dude" "asdlk;fja" "x") 16)

(round (/ 3 2))

(pad '("dude" "bro") 16)
