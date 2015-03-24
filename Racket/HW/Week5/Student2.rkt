
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (< high low)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs) )])
              (car (list-tail xs i )))]))
                  
                  
(define (stream-for-n-steps s n)
  (letrec ([f (lambda (i s) (if (> i n)
                              null
                              (cons (car (s)) (f (+ i 1) (cdr (s))))))])
    (f 1 s)))


(define funny-number-stream
  (letrec ([f (lambda (x) (if (= (remainder x 5) 0)
                              (cons (- 0 x) (lambda () (f (+ x 1))))
                              (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (s) 
                (cond [(equal? s "dan.jpg") (cons "dog.jpg" (lambda () (f "dog.jpg")))]
                      [(equal? s "dog.jpg") (cons "dan.jpg" (lambda () (f "dan.jpg")))]))])
    (lambda () (f "dog.jpg")))) 
                
(define (stream-add-zero s)
  (letrec ([f (lambda (s) (letrec ([g (lambda () (cons (cons 0 (car (s))) (f (cdr (s)))))])
                         g))])
    (f s)))                                

; Racket does "cycling" itself, in other words,
; if I have a list of 3 items, list-nth-mod ls 4 returns the first item.
(define (cycle-lists xs ys)                     
   (letrec ([f (lambda (i xs ys) 
                 (letrec ([g (lambda () 
                               (cons (cons (list-nth-mod xs i)(list-nth-mod ys i))
                                     (f (+ i 1) xs ys)))])
                   g))])
     (f 0 xs ys)))
                                    

(define (vector-assoc v vec)
  (letrec ([n (vector-length vec)]
           [f (lambda (i) (if (> i n)
                                #f
                                (let ([val (vector-ref vec i)])
                                  (if (pair? val)
                                      (if (equal? v (car val))
                                          val
                                          (f (+ i 1)))
                                      (f (+ i 1))))))])
    (f 0)))
                                    

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [ind 0] ; to keep track of which cache slot will be replaced next
           [lookup (lambda (v i) (if (or (< i n) (< i (- n ind)))
                                     #f
                                     (let ([item (vector-ref cache (remainder i n))])
                                       (if (equal? (car item) v)
                                           item
                                           (lookup v (+ i 1))))))]                           
           [update (lambda (val) (begin
                                   (vector-set! cache (remainder ind n) val)
                                   (if (= ind n)
                                       (set! ind 0)
                                       (set! ind (+ ind 1)))
                                   val))])
    (lambda (v) (cond ((lookup v ind))
                      ((vector-assoc v xs) => update)
                      (else #f)))))
  