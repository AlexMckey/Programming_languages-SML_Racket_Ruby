#lang racket
;; Programming Languages Homework4 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and change HOMEWORK_FILE to the name of your homework file.
;;

(require "hw4-AlexMckey.rkt")
(require rackunit)

;; Helper functions
(define (ones)(cons 1 ones))
(define nats
  (letrec ([f (lambda (x) 
                (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))
(define a 2)

(define tests
  (test-suite
   "Sample tests for Assignment 4"
   
   ; sequence test
   (check-equal? (sequence 0 5 1) (list 0 1 2 3 4 5) "Sequence test1")
   (check-equal? (sequence 0 4 1) (list 0 1 2 3 4) "Sequence test2")
   (check-equal? (sequence 0 5 2) (list 0 2 4) "Sequence test3")
   (check-equal? (sequence 1 5 2) (list 1 3 5) "Sequence test4")
   (check-equal? (sequence 3 11 2) (list 3 5 7 9 11) "Sequence test5")
   (check-equal? (sequence 3 8 3) (list 3 6) "Sequence test6")
   (check-equal? (sequence 3 2 1) null "Sequence test7")
   (check-equal? (sequence -3 2 1) (list -3 -2 -1 0 1 2) "Sequence test8")
   
   ; string-append-map test
   (check-equal? (string-append-map 
                  (list "dan" "dog" "curry" "dog2") 
                  ".jpg") '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg")
    "string-append-map test1")
   (check-equal? 
    (string-append-map (list "XPEH" "OPEX" "CO6AKA" "6apmarJIoT") ".suff")
    '("XPEH.suff" "OPEX.suff" "CO6AKA.suff" "6apmarJIoT.suff")
    "string-append-map test2")
   (check-equal? 
    (string-append-map null ".suff")
    null
    "string-append-map test3")
   (check-equal? 
    (string-append-map (list "") ".suff")
    '(".suff")
    "string-append-map test4")
   
   ; list-nth-mod test
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 0) 0 "list-nth-mod test1")
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 1) 1 "list-nth-mod test2")
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 2) 2 "list-nth-mod test3")
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 3) 3 "list-nth-mod test4")
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 4) 4 "list-nth-mod test5")
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 5) 0 "list-nth-mod test6")
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 6) 1 "list-nth-mod test7")
   (check-exn
    exn:fail?
    (lambda () (list-nth-mod null 5)) "list-nth-mod test8")
   (check-exn
    exn:fail?
    (lambda () (list-nth-mod (list 0 1 2 3 4 5) -1)) "list-nth-mod test9")
   
   ; stream-for-n-steps test
   (check-equal? (stream-for-n-steps ones 1) (list 1) "stream-for-n-steps test1")
   (check-equal? (stream-for-n-steps (lambda () (cons 1 ones)) 2) (list 1 1) "stream-for-n-steps test2")
   (check-equal? (stream-for-n-steps (lambda () (cons 1 ones)) 3) (list 1 1 1) "stream-for-n-steps test3")
   (check-equal? (stream-for-n-steps (lambda () (cons 1 ones)) 0) null "stream-for-n-steps test4")
   (check-equal? (stream-for-n-steps nats 5) (list 1 2 3 4 5) "stream-for-n-steps test5")
   
   ; funny-number-stream test
   (check-equal? (stream-for-n-steps funny-number-stream 16) (list 1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15 16) "funny-number-stream test")
   
   ; dan-then-dog test
   (check-equal? (stream-for-n-steps dan-then-dog 1) (list "dan.jpg") "dan-then-dog test1")
   (check-equal? (stream-for-n-steps dan-then-dog 2) (list "dan.jpg" "dog.jpg") "dan-then-dog test2")
   (check-equal? (stream-for-n-steps dan-then-dog 5) (list "dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg" "dan.jpg") "dan-then-dog test3")
   (check-equal? (stream-for-n-steps dan-then-dog 0) null "dan-then-dog test4")
   
   ; stream-add-zero test
   (check-equal? (stream-for-n-steps (stream-add-zero ones) 1) (list (cons 0 1)) "stream-add-zero test1")
   (check-equal? (stream-for-n-steps (stream-add-zero ones) 2) '((0 . 1) (0 . 1)) "stream-add-zero test2")
   (check-equal? (stream-for-n-steps (stream-add-zero nats) 3) '((0 . 1) (0 . 2) (0 . 3)) "stream-add-zero test3")
   (check-equal? (stream-for-n-steps (stream-add-zero dan-then-dog) 3) '((0 . "dan.jpg") (0 . "dog.jpg") (0 . "dan.jpg")) "stream-add-zero test4")
   (check-equal? (stream-for-n-steps (stream-add-zero (stream-add-zero nats)) 3) '((0 0 . 1) (0 0 . 2) (0 0 . 3)) "stream-add-zero test5")
   
   ; cycle-lists test
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3) (list (cons 1 "a") (cons 2 "b") (cons 3 "a")) 
                 "cycle-lists test")
   
   ; vector-assoc test
   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) (cons 4 1) "vector-assoc test1")
   (check-equal? (vector-assoc 2 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) '(2 . 1) "vector-assoc test2")
   (check-equal? (vector-assoc 3 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) '(3 . 1) "vector-assoc test3")
   (check-equal? (vector-assoc 2 (vector (cons 2 4) (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) '(2 . 4) "vector-assoc test4")
   (check-equal? (vector-assoc 6 (vector (cons 2 1) (cons 2 4) (cons 3 1) (cons 4 1) (cons 5 1))) #f "vector-assoc test5")
   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 2 4) 1 (cons 4 1) (cons 5 1))) '(4 . 1) "vector-assoc test6")
   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 2 4) (list 4 2 3 1 5) (cons 4 1) (cons 5 1))) '(4 2 3 1 5) "vector-assoc test7")
   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 2 4) 4 (cons 4 1) (cons 5 1))) '(4 . 1) "vector-assoc test8")
   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 2 4) (vector 4 2 3 1 5) (cons 4 1) (cons 5 1))) '(4 . 1) "vector-assoc test9")
   
   ; cached-assoc tests
   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3) (cons 3 4) "cached-assoc test")
   
   ; while-less test
   (check-equal? (while-less 7 do (begin (set! a (+ a 1)) a)) #t "while-less test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
