(define/contract (reverse x)
  (-> exact-integer? exact-integer?)
  (if
   (negative? x)
   (* -1 (my-reverse (* -1 x) 0 0 #t))
   (my-reverse x)))

(define (my-reverse x (rev 0) (rem 0) (neg #f))
  (cond
   [(and neg (> rev 2147483648)) 0]
   [(and (not neg) (> rev 2147483647)) 0]
   [(zero? x) rev]
   [else
    (let ((remainder (modulo x 10)))
      (my-reverse (quotient x 10) (+ remainder (* rev 10)) remainder neg))]))
