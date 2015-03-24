
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
   (if (> low high)
       null
       (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append suffix s)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) ((error "list-nth-mod: negative number"))]
        [(null? xs) ((error "list-nth-mod: empty list"))]
        [#t (let ([i (remainder n (length xs))]) (car (list-tail xs i)))]))
                    
(define (stream-for-n-steps s n)
  (letrec ([f (lambda (s xs i)
                (if (= i n)
                    (append xs (list (car (s))))
                    (f (cdr (s)) (append xs (list (car (s)))) (+ i 1))))])
    (if (= n 0)
        s
        (f s (list) 1))))

(define funny-number-stream
  (letrec ([f (lambda (x) (if (= (remainder x 5) 0)
                              (cons (- x) (lambda () (f (+ x 1))))
                              (cons x (lambda () (f (+ x 1))))))])
           (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (s) (if (string=? s "dog")
                              (cons "dog.jpg" (lambda () (f "dan")))
                              (cons "dan.jpg" (lambda () (f "dog")))))])
    (lambda () (f "dan"))))

(define (stream-add-zero stream)
  (letrec ([f (lambda (s) (cons (cons 0 (car (s))) (lambda () (f (cdr (s))))))])
    (lambda () (f stream))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (as bs)
                (cond [(and (null? as) (not (null? bs))) (cons (cons (car xs) (car bs)) (lambda () (f (cdr xs) (cdr bs))))]
                      [(and (null? bs) (not (null? as))) (cons (cons (car as) (car ys)) (lambda () (f (cdr as) (cdr ys))))]
                      [(and (null? as) (null? bs)) (cons (cons (car xs) (car ys)) (lambda () (f (cdr xs) (cdr ys))))]
                      [#t (cons (cons (car as) (car bs)) (lambda () (f (cdr as) (cdr bs))))]))])
  (lambda () (f xs ys)))
  )

(define (vector-assoc v vec)
  (letrec ([f (lambda (n) 
             (cond [(>= n (vector-length vec)) #f]
                   [(and (pair? (vector-ref vec n)) (equal? (car (vector-ref vec n)) v)) (vector-ref vec n)]
                   [#t (f (+ n 1))]))])
    (f 0)))

(define (cached-assoc xs n)
  (let* ([cache (make-vector n #f)]
         [index 0])
    (lambda (v) (cond [(vector-assoc v cache) (vector-assoc v cache)]
                      [#t (let ([result (assoc v xs)])
                            (if result
                              (begin
                                (vector-set! cache index result)
                                (if (= index (- n 1))
                                    (set! index 0)
                                    (set! index (+ index 1)))
                                result)
                              #f))]))))