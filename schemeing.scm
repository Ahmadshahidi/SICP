(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
	product
	(iter (* counter product) (+ counter 1))))
  (iter 1 1))

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fibIt n)
  (define (fib-iter a b counter)
    (if (= counter 0)
	b
	(fib-iter (+ a b) a (- counter 1))))
  (fib-iter 1 0 n))


(define (count-change amount)
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
	  ((= kinds-of-coins 2) 5)
	  ((= kinds-of-coins 3) 10)
	  ((= kinds-of-coins 4) 25)
	  ((= kinds-of-coins 5) 50)))
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
	  ((or (< amount 0) (= kinds-of-coins 0)) 0)
	  (else (+ (cc amount (- kinds-of-coins 1))
		   (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))
  (cc amount 5))


(define (f-of-n n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	((= n 2) 2)
	(else (+ (f-of-n (- n 1)) (* 2 (f-of-n (- n 2))) (* 3 (f-of-n (- n 3)))
		 ))))

;; Iterating f(n) = f(n-1)+2*f(n-2)+3*f(n-3)

(define (f-of-n-iter n)
  (define (new x y z)
    (+ (* 3 x) (* 2 y) z))
  (define (itering a b c count countup)
    (cond ((and (= 2 (modulo n 3)) (= count 2)) c)
	  ((and (= 1 (modulo n 3)) (= count 2)) b)
	  ((and (= 0 (modulo n 3)) (= count 2)) a)
	  ((= count 1) 1)
	  ((= count 0) 0)
	  ((= 0 (modulo countup 3))
	   (itering (new a b c) b c (- count 1) (+ countup 1)))
	  ((= 1 (modulo countup 3))
	   (itering a (new b c a) c (- count 1) (+ countup 1)))
	  ((= 2 (modulo countup 3))
	   (itering a b (new c a b) (- count 1) (+ countup 1)))
	  ))
	  
	;;  (else (itering (new a b c) (new b c a) (new c a b) (- count 1)))))
  (itering 0 1 2 n 0))

;;Exponentiation

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;;Exponen.Iter

(define (expt-it b n)
  (define (expt-iter b count product)
    (if (= count 0)
	product
	(expt-iter b (- count 1) (* b product))))
  (expt-iter b n 1))

(define (fast-expt b n)
  (define (square x)
    (* x x))
  (define (even? n)
    (= (remainder n 2) 0))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
  (else (* b (fast-expt b (- n 1)))))) 

;; fast-expt-iterating

(define (fast-expt-it b n)
  (define (fe-iter b count product)
    (cond ((= 0 count) product)
	  ((even? n) (fe-iter b (- count 2) (* (* b b) product)))
	  (else (* b (fe-iter b (- count 1) product)))))
  (fe-iter b n 1))

;;
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;;
(define (smallest-divisor n)
  (define (find-divisor n m)
    (cond ((> (squar m ) n) n)
	  ((divids? m n) m)
	  ((find-divisor n (+ m 1)))))
  (find-divisor n 2))

(define (divids? a b)
  (= (remainder b a) 0))

;;;Higher order producers

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))
;;cube
(define (cube x) (* x x x))
;;next
(define (next x) (+ x 1))
;;sum-of-cubed

(define (sum-of-cubed a b)
  (sum cube a next b))

(define (pi-term x)
  (/ 1.0 (* x (+ x 2))))

(define (pi-next x)
  (+ x 4))

(define (pi-sum a b)
  (sum pi-term a pi-next b))

  

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

;;;;;;;;;;;;;;;;;
(define (Simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (simp-term1 x)
    (* 2 (f x)))
  (define (simp-term2 x)
    (* 4 (f x)))
  (define (next-simp-term x)
    (+ x h))
  (define (simp-sum a b state)
    (cond ((> a b) 0)
          ((= state 0) (+ (f a) (simp-sum (next-simp-term a) b (+ state 1))))
	  ((odd? state) (+ (simp-term2 a) (simp-sum (next-simp-term a) b (+ state 1))))
	  ((even? state) (+ (simp-term1 a) (simp-sum (next-simp-term a) b (+ state 1))))
	  ((= state n) (+ (f b) (simp-sum (next-simp-term a) b (+ state 1))))
	  ))
  (* (/ h 3.0) (simp-sum a b 0)))

(define (even? n)
  (and (> n 0) (= (remainder n 2) 0)))

(define (odd? n)
  (and (> n 0) (not (even? n))))


;;;;;;;;;;Midpoint method;;;;;;;;;;;;;;;;
;;;
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-poit pos-point)
	midpoint
	(let ((test-sign (f midpoint)))
	  (cond ((positive? test-sign) (search f neg-point midpoint))
		((negative? test-sign) (search f midpoint pos-point))
		(else midpoint))))))

;;;;;;;;;;;;;;;;;;;Fixed-Point;;;;;;;;;;;;;;;;;;;
;;;
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next-val (f guess)))
      (if (close-enough? next-val guess)
	  next-val
	  (try next-val))))
  (try first-guess))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (cont-frac first second k)
  (let ((n (first k))
	(d (second k)))
    (if (= k 1)
	n
	(/ n (+ d (cont-frac first second (- k 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;2.27;;;;;;;;;;;;;;;;;;;;;;;;
(define (reverse items)
  (if (null? items)
      '()
      (append (reverse (cdr items)) (list (car items)))))

(define (deep-reverse items)
  (cond ((null? items)
	 '())
	((= (count-leaves items) 2)
	 (list (cdr items) (car items)))
        (else (list  (deep-reverse (cdr items)) (deep-reverse (car items))))))

(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))
	 
	 
	       
