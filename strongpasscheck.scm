;; A password is considered strong if the below conditions are all met:

;; It has at least 6 characters and at most 20 characters.
;; It contains at least one lowercase letter, at least one uppercase letter, and at least one digit.
;; It does not contain three repeating characters in a row (i.e., "...aaa..." is weak, but "...aa...a..." is strong, assuming other conditions are met).
;; Given a string password, return the minimum number of steps required to make password strong. if password is already strong, return 0.

;; In one step, you can:

;; Insert one character to password,
;; Delete one character from password, or
;; Replace one character of password with another character.
;; (define has-lower #f)
;; (define has-upper #f)
;; (define has-digit #f)
(require rnrs/arithmetic/bitwise-6)
(define/contract (strong-password-checker password)
  (-> string? exact-integer?)
  ;; (set! has-lower #f)
  ;; (set! has-upper #f)
  ;; (set! has-digit #f)
  ;; (let ((clist (string->list password)))
  ;;   (for-each (lambda (c)
  ;; 		(set! has-lower (or has-lower (char-lower-case? c)))
  ;; 		(set! has-upper (or has-upper (char-upper-case? c)))
  ;; 		(set! has-digit (or has-digit (char-numeric? c))))
  ;; 	      clist)
  ;;   (strengthen 0 0 clist))
  (strengthen #:remaining (string->list password))
  )

(define (btoi b)
  (if b 1 0))

(define HAS_LOWER 1)
(define HAS_UPPER 2)
(define HAS_DIGIT 4)
(define MAX_FLAGS (+ HAS_LOWER HAS_UPPER HAS_DIGIT))

(define (set-flag bitflag flag)
  (bitwise-ior flags flag))

(define (add-next-flag flags)
  (bitwise-and MAX_FLAGS (bitwise-ior flags (bitwise-arithmetic-shift 1 (bitwise-first-bit-set (bitwise-not flags))))))

(define (maybe-add-flag flags char)
  (bitwise-ior
   flags
   (cond
    [(char-lower-case? char) HAS_LOWER]
    [(char-upper-case? char) HAS_UPPER]
    [(char-numeric? char) HAS_DIGIT]
    [else 0])))

(define (calc-rep rep a b)
  (if (eq? a b) (add1 rep) 1))

(define (strengthen
	 #:steps (steps 0)
	 #:pos (pos 0)
	 #:remaining (remaining '())
	 #:last-char (last-char '())
	 #:rep (rep 0)
	 #:flags (flags 0))
  (let ((chars-left (max 0 (- 6 pos)))
	(curr-length (+ pos (length remaining)))
	(spc-chr-left (- 3 (bitwise-bit-count flags))))
    (displayln `(strengthen
		 #:steps ,steps
		 #:pos ,pos
		 #:remaining ,remaining
		 #:last-char ,last-char 
		 #:rep ,rep
		 #:flags ,flags
		 curr-length ,curr-length))
    (cond
     [(null? remaining)
      (+ steps (max chars-left spc-chr-left) (if (= 3 rep) 1 0))]
     [(= 3 rep)
      (cond
       [(< curr-length 6)
	(strengthen
	 #:steps (add1 steps)
	 #:pos (add1 pos)
	 #:remaining remaining
	 #:last-char '()
	 #:rep 0
	 #:flags (add-next-flag flags))]
       [(or (> spc-chr-left 0) (eq? last-char (car remaining)))
	(strengthen
	 #:steps (add1 steps)
	 #:pos pos
	 #:remaining remaining
	 #:last-char '()
	 #:rep 0
	 #:flags (add-next-flag flags))]
       [else
	(strengthen
	 #:steps (add1 steps)
	 #:pos pos
	 #:remaining (cdr remaining)
	 #:last-char (car remaining)
	 #:rep 1
	 #:flags flags)])]
     [(and (= pos 20))
      (strengthen
       #:steps (add1 steps)
       #:pos pos
       #:remaining (cdr remaining)
       #:last-char (car remaining)
       #:rep rep
       #:flags (maybe-add-flag flags (car remaining)))]
     [else
      (strengthen
       #:steps steps
       #:pos (add1 pos)
       #:remaining (cdr remaining)
       #:last-char (car remaining)
       #:rep (calc-rep rep last-char (car remaining))
       #:flags (maybe-add-flag flags (car remaining)))])))
  










;; (define (strengthen steps pos remaining)
;;   (let ((charsLeft (max 0 (- 6 pos)))
;; 	(curr-length (+ pos (length remaining)))
;; 	(spc-chr-left (- 3 (+ (btoi has-lower) (btoi has-upper) (btoi has-digit)))))
;;     (displayln `(,steps ,curr-length ,pos ,charsLeft ,has-lower ,has-upper ,has-digit ,remaining))
;;     (cond
;;      [(and has-lower
;; 	   has-upper
;; 	   has-digit
;; 	   (null? remaining)
;; 	   (>= 6 pos)
;; 	   (<= 20 pos))
;;       steps]
;;      [(and (= 20 pos)
;; 	   (> (length remaining) 0))
;;       (let ((r (car remaining)))
;; 	(strengthen
;; 	 (add1 steps)
;; 	 pos
;; 	 (cdr remaining)))]
;;      [(null? remaining)
;;       (+ steps (max charsLeft spc-chr-left))]
;;      [(and (>= (length remaining) 3) (= 1 (length (remove-duplicates (take remaining 3)))))
;;       (cond [(and
;; 	      (>= curr-length 6)
;; 	      (or
;; 	       (> spc-chr-left 0)
;; 	       (= (length remaining) 3)
;; 	       (and (<= curr-length 20) eq? (third remaining) (fourth remaining))))
;; 	     (set! has-digit (or has-upper has-digit))
;; 	     (set! has-upper (or has-lower has-upper))
;; 	     (set! has-lower #t)
;; 	     (strengthen (add1 steps) (+ 3 pos) (drop remaining 3))]
;; 	    [(< curr-length 6)
;; 	     (strengthen (add1 steps) (+ 2 pos) (cdr remaining))]
;; 	    [(> curr-length 20)
;; 	     (strengthen (add1 steps) pos (cdr remaining))]
;; 	    [else (strengthen (add1 steps) pos (cdr remaining))])]
;;      [else (strengthen steps (add1 pos) (cdr remaining))])))

(map strong-password-checker '("a"
			       "aaa111"
			       "aaaB1"
			       "1111111111"
			       "bbaaaaaaaaaaaaaaacccccc"
			       "aaa123"
			       "aaaabbbbccccddeeddeeddeedd"
			       "1337c0DE"
			       "aaaaAAAAAA000000123456"
			       "A1234567890aaabbbbccccc"))
;; 5
;; 2
;; 1
;; 3
;; 8
;; 1
;; 8
;; 0
;; 5
;; 4

