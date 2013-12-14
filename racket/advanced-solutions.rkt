#lang racket

;; The nth element of the Fibonacci sequence. 0, 1, 1, 2, 3, 5...
(define (fibonacci n)
  (define (fib-helper n x1 x2)
    (cond
     ((zero? n) x1)
     ((= n 1) x2)
     (else
      (fib-helper (- n 1) x2 (+ x1 x2)))))
  (fib-helper n 0 1))

(define (qsort lst)
  (cond
   ((null? lst) '())
   (else
    (let* ((pivot (car lst))
           (less-than? (lambda (x) (<= x pivot)))
           (greater-than? (lambda (x) (> x pivot))))
      (append
       (qsort (filter less-than? (cdr lst)))
       (list pivot)
       (qsort (filter greater-than? (cdr lst))))))))

(define (reverse lst)
  (foldl cons '() lst))

(define (prime? n)
  (define (divides? x)
    (zero? (modulo n x)))
  (and (> n 1)
       (null? (filter divides? (range 2 n)))))

(define (maximum lst)
  (foldl (lambda (x y) (if (> x y) x y))
         (car lst)
         (cdr lst)))

(define (product-of-digits n)
  (cond
   ((< n 10) n)
   (else
    (* (modulo n 10) (product-of-digits (quotient n 10))))))

(define (greatest-prime-digit-product n)
  (maximum (map product-of-digits (filter prime? (range n)))))

(provide
 fibonacci
 qsort
 prime?
 maximum
 product-of-digits
 greatest-prime-digit-product)
