;;;;;2.2;;;;;;;;;;;;;;

(define (make-segment a b)
  (cons a b))

(define (start-segment x) (car x))
(define (end-segment x) (cdr x))

;;;;;;;;;;;;;Point;;;;;;;
(define (make-point x y)
  (cons x y))

(define (x-point x) (car x))
(define (y-point x) (cdr x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;midpoint-segment;;;;;;;;;;;;

(define (midpoint-segment x)
  (let ((x-cord (average (x-point (start-segment x)) (x-point (end-segment x))))
	(y-cord (average (y-point (start-segment x)) (y-point (end-segment x)))))
    (make-point x-cord y-cord)))

;;;;;;;;;;;;;;;;;;;;printing points;;;;;;;;;;;;;;;
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

;;;;;;;;;;;;;;;;;;;2.3-pge90;;;;;;;;;;;;;;;;;;;;;
(define (make-rectangle a b c d)
  (define ab (make-segment a b))
  (define ac (make-segment a c))
  (define ad (make-segment a d))
  (define bc (make-segment b c))
  (define bd (make-segment b d))
  (define cd (make-segment c d))
  (cond ((and (is-rect? a b c d) (perpend? ab ac))
	 (cons (cons a b) (cons d c)))
	((and (is-rect? a b c d) (perpend? ab ad))
	 (cons (cons a d) (cons c b)))
	((and (is-rect? a b c d) (perpend? ac ad))
	 (cons (cons a d) (cons b c)))
	(else (display "cannot make rectangle"))))
  


(define (is-rect? a b c d)
  (define ab (make-segment a b))
  (define ac (make-segment a c))
  (define ad (make-segment a d))
  (define bc (make-segment b c))
  (define bd (make-segment b d))
  (define cd (make-segment c d))
  (cond ((and (not (perpend? ab ac)) (not (perpend? ab ad)) (not (perpend? ac ad)))
	 #f)
	((and (perpend? ab ac) (not (pair-eq? (midpoint-segment ad) (midpoint-segmet bc))))
	 #f)
	((and (perpend? ab ad) (not (pair-eq? (midpoint-segment ac) (midpoint-segment bd))))
	 #f)
	((and (perpend? ac ad) (not (pair-eq? (midpoint-segment ab) (midpoint-segment cd))))
	 #f)
	(else #T)))


	
(define (perpend? x y)
  (cond ((and (= (x-point (start-segment x)) (x-point (end-segment x)))
	      (= (y-point (start-segment y)) (y-point (end-segment y))))
	 #T)
	((and (= (x-point (start-segment y)) (x-point (end-segment y)))
	      (= (y-point (start-segment x)) (y-point (end-segment x))))
	 #T)
	((= (* (slope x) (slope y)) -1)
	 #T)
	(else #F)))

(define (slope x)
  (/ (- (y-point (end-segment x)) (y-point (start-segment x))) (- (x-point (end-segment x)) (x-point (start-segment x)))))
    
  
(define (pair-eq? p q)
  (if (and (= (car p) (car q)) (= (cdr p) (cdr q)))
      #t
      #f))
(define (rect-l x)
  (make-segment (car (car x)) (car (cdr x))))

(define (rect-w x)
  (make-segment (car (car x)) (cdr (cdr x))))

(define (segment-size x)
  (define (squar x) (* x x))
  (sqrt (+ (squar (- (x-point (end-segment x)) (x-point (start-segment x))))
	   (squar (- (y-point (end-segment x)) (y-point (start-segment x))))
	   )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2.20 page 104;;;;;;;;;;;;;;;;;;;;;;;;;
(define (same-par x . z)
  (if (odd? x)
      (cons x (odd-par z))
      (cons x (even-par z))))


(define (odd-par z)
  (cond ((null? z) '())
	 ((odd? (car z))
	 (cons (car z)
	       (odd-par (cdr z))))
	(else (odd-par (cdr z)))))

(define (even-par z)
  (cond ((null? z) '())
	 ((even? (car z))
	 (cons (car z)
	       (even-par (cdr z))))
	(else (even-par (cdr z)))))
