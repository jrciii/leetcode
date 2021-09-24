(define/contract (number-of-combinations num)
  (-> string? exact-integer?)
  (go num (substring num 1))
  )

(define (go num rest)
  (cond
   [(eq? "" rest) 1]
   [(eq? "0" num) 0]
   [(eq? #\0 (string-ref rest 0)) 0]
   [(> (string->number num) (string->number rest)) 0]
   [else
    (let ((a (string->number num))
	  (b (string->number rest)))
      (+ (if (<= a b) 1 0)
	 (go )))]))

(define (take-until-ge a b)
  (for/last ([i (stop-after
	    (in-range (string-length b))
	    (lambda (x) (>= (string->number (substring b 0 (add1 x))) a)))])
    (substring b 0 (add1 i))))

(take-until-ge 33 "333")

