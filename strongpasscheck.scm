;; A password is considered strong if the below conditions are all met:

;; It has at least 6 characters and at most 20 characters.
;; It contains at least one lowercase letter, at least one uppercase letter, and at least one digit.
;; It does not contain three repeating characters in a row (i.e., "...aaa..." is weak, but "...aa...a..." is strong, assuming other conditions are met).
;; Given a string password, return the minimum number of steps required to make password strong. if password is already strong, return 0.

;; In one step, you can:

;; Insert one character to password,
;; Delete one character from password, or
;; Replace one character of password with another character.
(define has-lower #f)
(define has-upper #f)
(define has-digit #f)

(define/contract (strong-password-checker password)
  (-> string? exact-integer?)
  (set! has-lower #f)
  (set! has-upper #f)
  (set! has-digit #f)
  (let ((clist (string->list password)))
    (for-each (lambda (c)
		(set! has-lower (or has-lower (char-lower-case? c)))
		(set! has-upper (or has-upper (char-upper-case? c)))
		(set! has-digit (or has-digit (char-numeric? c))))
	      clist)
    (strengthen 0 0 clist)))

(define (btoi b)
  (if b 1 0))

(define (strengthen steps pos remaining)
  (let ((charsLeft (max 0 (- 6 pos)))
	(curr-length (+ pos (length remaining)))
	(spc-chr-left (- 3 (+ (btoi has-lower) (btoi has-upper) (btoi has-digit)))))
    (displayln `(,steps ,curr-length ,pos ,charsLeft ,has-lower ,has-upper ,has-digit ,remaining))
    (cond
     [(and has-lower
	   has-upper
	   has-digit
	   (null? remaining)
	   (>= 6 pos)
	   (<= 20 pos))
      steps]
     [(and (= 20 pos)
	   (> (length remaining) 0))
      (let ((r (car remaining)))
	(strengthen
	 (add1 steps)
	 pos
	 (cdr remaining)))]
     [(null? remaining)
      (+ steps (max charsLeft spc-chr-left))]
     [(and (< pos 18) (>= (length remaining) 3) (= 1 (length (remove-duplicates (take remaining 3)))))
      (cond [(and (>= curr-length 6) (<= curr-length 20))
	     (set! has-digit (or has-upper has-digit))
	     (set! has-upper (or has-lower has-upper))
	     (set! has-lower #t)
	     (strengthen (add1 steps) (+ 3 pos) (drop remaining 3))]
	    [(and (< curr-length 20) (<= (+ pos charsLeft) 6))
	     (strengthen (add1 steps) (+ 2 pos) (cdr remaining))]
	    [else (strengthen (add1 steps) pos (cdr remaining))])]
     [else (strengthen steps (add1 pos) (cdr remaining))])))

;(strong-password-checker "aaa111")
;(strong-password-checker "aaaB1")
;(strong-password-checker "1111111111")
; Gives 9 should give 8
(strong-password-checker "bbaaaaaaaaaaaaaaacccccc")
