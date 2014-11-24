

;;1-7 improve good-enough? procedure

(define (average x y)
  (/ (+ x y) 2))
(define (square x)
  (* x x))
(define (improve guess x)
  (average guess (/ x guess)))


(define (good-enough? prev-guess guess)
  (> 0.001 (abs (- prev-guess guess))))

(define (sqrt-iter guess x)
  (define improve-guess (improve guess x))
  (if (good-enough? guess improve-guess)
      improve-guess
      (sqrt-iter improve-guess x)))

(define (sqrt n) (sqrt-iter 1.0 n))


;;1-8 build cube-root procedure 
(define (cbrt n) (cbrt-iter 1.0 n))
(define (improve-cbrt guess x)
  (/ (+ (/ x (* guess guess)) (* guess 2)) 3))

(define (cbrt-iter guess x)
  (define cbrt-improve (improve-cbrt guess x))
  (if (good-enough? guess cbrt-improve)
      cbrt-improve
      (cbrt-iter cbrt-improve x)))

