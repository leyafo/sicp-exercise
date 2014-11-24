(define (max a b)
  (if (> a b) a b))

(define (max-sum a b c)
    (max (+ b c) (max (+ a b) (+ a c))))

(max-sum 6 5 8)
