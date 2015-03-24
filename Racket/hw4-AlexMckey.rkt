
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

(define (sequence low high strade)
  (if (> low high)
      null
      (cons low (sequence (+ low strade) high strade))))

(define (string-append-map xs suffix)
  (map (λ (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond
    [(< n 0) (error "list-nth-mod: negative number")]
    [(= (length xs) 0) (error "list-nth-mod: empty list")]
    [(= n 0) (car xs)]
    [(>= n (length xs)) (car (list-tail xs (remainder n (length xs))))]
    [#t (car (list-tail xs n))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s))
            (stream-for-n-steps (cdr (s)) (- n 1)))))

(define (funny-number-stream)
  (letrec ([f (λ (x)
                (cons (if (= 0 (remainder x 5))
                          (- x)
                          x)
                      (λ () (f (+ x 1)))))])
    (f 1)))

(define dan-then-dog
  (letrec (
           [dan (λ () (cons "dan.jpg" dog))]
           [dog (λ () (cons "dog.jpg" dan))])
    dan))

(define (stream-add-zero s)
  (letrec ([next (s)]
           [ans (cons 0 (car next))]
           [stream (cdr next)])
    (λ () (cons ans (stream-add-zero stream)))))

(define (cycle-lists xs ys)
  (letrec ([f (λ (x)
                (cons (cons (list-nth-mod xs x)
                            (list-nth-mod ys x))
                      (λ () (f (+ x 1)))))])
    (λ () (f 0))))

(define (vector-assoc v vec)
  (letrec ([fun (λ (i)
                  (if (= i (vector-length vec))
                      #f
                      (let ([val (vector-ref vec i)])
                        (cond
                          [(not (pair? val)) (fun (+ i 1))]
                          [(equal? v (car val)) val]
                          [#t (fun (+ i 1))]))))])
    (fun 0)))

(define (cached-assoc xs n)
  (letrec([memo (make-vector n)]
          [pos 0]
          [f (λ (v)
               (let ([ans (vector-assoc v memo)])
                 (if ans
                     ans
                     (let ([new-ans (assoc v xs)])
                       (begin
                         [vector-set! memo pos new-ans]
                         [set! pos (if (= pos (- n 1)) 0 (+ pos 1))]
                         new-ans)))))])
    f))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([x e1]
              [loop (λ ()
                      (let ([y e2])
                        (if (>= y x)
                            #t
                            (begin y (loop)))))])
         (loop))]))