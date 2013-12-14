#lang racket

;;; This function the sum of all numbers up to and including n for 0 <= n.
(define (sum-to-n n)
  (cond
   ((zero? n) 0)
   (else
    (+ n (sum-to-n (- n 1))))))

;;; This function calculates n choose r, for 0 <= n, 0 <= r <= n
(define (combination n r)
  (cond
   ((zero? r) 1)
   ((= n r) 1)
   (else
    (+ (combination (- n 1) (- r 1))
       (combination (- n 1) r)))))

(define (sum lst)
  (cond
   ((null? lst) 0)
   (else
    (+ (car lst) (sum (cdr lst))))))

(define (member? a lst)
  (cond
   ((null? lst) #f)
   ((equal? a (car lst)) #t)
   (else
    (member? a (cdr lst)))))

(define (rember lst a)
  (cond
   ((null? lst) '())
   ((equal? a (car lst)) (cdr lst))
   (else
    (cons (car lst) (rember (cdr lst) a)))))

(define (member?* a lst)
  (cond
   ((null? lst) #f)
   ((equal? a (car lst)) #t)
   ((and (list? (car lst))
         (member?* a (car lst))) #t)
   (else (member?* a (cdr lst)))))

(define (intersect set1 set2)
  (cond
   ((null? set1) '())
   (else
    (let ((rest (intersect (cdr set1) set2)))
      (if (member? (car set1) set2)
          (cons (car set1) rest)
          rest)))))

(define (two-in-a-row? lst)
  (define (two-in-a-row-helper last lst)
    (cond
     ((null? lst) #f)
     ((equal? last (car lst)) #t)
     (else
      (two-in-a-row-helper (car lst) (cdr lst)))))
  (and (not (null? lst))
       (two-in-a-row-helper (car lst) (cdr lst))))

(define (nth lst n)
  (cond
   ((zero? n) (car lst))
   (else
    (nth (cdr lst) (- n 1)))))

(define (dedup lst)
  (cond
   ((null? lst) '())
   ((member? (car lst) (cdr lst)) (dedup (cdr lst)))
   (else
    (cons (car lst) (dedup (cdr lst))))))

(define (reverse lst)
  (define (reverse-helper lst rev)
    (cond
     ((null? lst) rev)
     (else
      (reverse-helper (cdr lst) (cons (car lst) rev)))))
  (reverse-helper lst '()))

(provide
 sum-to-n
 combination
 sum
 rember
 member?
 member?*
 intersect
 two-in-a-row?
 nth
 dedup
 reverse)
