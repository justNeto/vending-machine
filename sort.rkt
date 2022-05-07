#lang racket

(define sort '(4 2 3 1 6 7 7 4 12 54 1))

(define (quick-sort lts)
  (cond
    [(null? lts) '()]
    [else (append
	    (append
	      (quick-sort (filter (lambda (x) (< x (car lts))) lts))
	      (filter (lambda (x) (= x (car lts))) lts))
	    (quick-sort (filter (lambda (x) (> x (car lts))) lts))
	  )
    ]
  )
)

(define sort2 '((1 5) (2 54) (5 42)  (10 12)  (20 12)) )

(define (bubble L)
  (if (null? (cdr L))
      L
      (if (> (car (car L)) (car (cadr L)))
          (cons (car L)
                (bubble (cdr L)))
          (cons (cadr L)
                (bubble (cons (car L) (cddr L)))))))


(define (bubble-sort N L)
  (cond ((= N 1) (bubble L))
        (else
         (bubble-sort (- N 1) (bubble L)))))


(define (bubble-set-up L)
  (bubble-sort (length L) L))
