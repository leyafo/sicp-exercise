#lang racket


(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

;for example:
;  f(n) = f(n-1)+f(n-2)+f(n-3)

(define (f-iter a b c n)
  (cond ((= n 2) a)
        ((= n 1) b)
        ((= n 0) c)
        (else (f-iter (+ a (* 2 b) (* 3 c)) a b (- n 1)))
   ))
(define (f-loop n)
  (f-iter 2 1 0 n))


(define (print l)
  (display l) (newline))

(define (pascal n)
  (define l (list 1))
  (if (= n 1) 
      l
      (let ((l (pascal (- n 1))))
        (if (or (> (length l) 2) (= (length l) 2))
            (set! l (append-row (gen l)))
            (set! l (append (list 1) l))) (print l) l)))

(define (append-row l)
      (append (append (list 1) l) (list 1)))

(define (gen l)
  (define len (length l))
  (define new-l (list))  
  (do ((i 1 (+ 1 i)))
    ((= i len))
    (set! new-l 
          (append new-l (list (+ (list-ref l (- i 1)) (list-ref l i))))))
  new-l)


(define (test-gen) (gen (list 1 2 3 4)))
(define (test) (pascal 10))
(define (test-1) (pascal 1))
(define (test-2) (pascal 2))
(define (test-3) (pascal 3))