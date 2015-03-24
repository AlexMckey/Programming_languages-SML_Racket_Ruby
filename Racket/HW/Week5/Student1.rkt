
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))])
                  (car (list-tail xs i)))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([ans (s)])
        (cons (car ans) (stream-for-n-steps (cdr ans) (- n 1))))))

(define funny-number-stream 
  (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0) (- 0 x) x)
                                    (lambda () (f (+ x 1)))))])
     (lambda () (f 1))))

(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" (lambda () (dog))))]
           [dog (lambda () (cons "dog.jpg" (lambda () (dan))))])
    (lambda () (dan))))

(define (stream-add-zero s)
  (define (f x) (cons (cons 0 (car (x))) (lambda () (f (cdr (x))))))
  (lambda () (f s)))

(define (cycle-lists xs ys)
  (define (f n) (cons (cons
                       (list-nth-mod xs n)
                       (list-nth-mod ys n))
                      (lambda () (f (+ n 1)))))
  (lambda () (f 0)))

