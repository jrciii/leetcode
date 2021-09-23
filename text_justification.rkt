#lang racket
(require racket/match)

(define/contract (full-justify words maxWidth)
  (-> (listof string?) exact-integer? (listof string?))
  (cond
   [(null? words) null]
   [else null])
  )

(define (pad words maxWidth)
  (let ((spaces (- maxWidth ())))))

(define (justify words maxWidth)
  (match words
    ['() '()]
    [(list a) (pad a)]
    [(list a b ... c ...)
   #:when (<= (+ (string-length a) (foldl + 0 (map (compose add1 string-length) b))) maxWidth)
   (list a b c)]))

