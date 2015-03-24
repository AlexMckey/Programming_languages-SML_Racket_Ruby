#lang racket
;; Programming Languages Homework5 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and change HOMEWORK_FILE to the name of your homework file.

(require "hw5-AlexMckey.rkt")

(require rackunit)

(define tests
  (test-suite
   "Sample tests for Assignment 5"
   
   ;; check racketlist to mupllist with normal list
   (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit))) "racketlist->mupllist test")
   
   ;; check mupllist to racketlist with normal list
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit)))) (list (int 3) (int 4)) "racketlist->mupllist test")

   ;; eval-under-env test
   (check-equal? (eval-under-env (var "foo") (cons (cons "foo" (int 1)) '())) (int 1) "var test")
   (check-exn exn:fail? (λ () (eval-exp (var "s"))) "var error test")
   (check-equal? (eval-exp (int 5)) (int 5) "int test")
   (check-equal? (eval-exp (int "Hi")) (int "Hi") "int wrong test")
   (check-equal? (eval-exp (add (int 5) (int 6))) (int 11) "add test")
   (check-exn exn:fail? (λ() (eval-exp (add (int 5) (aunit)))) "add error test")
   (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) "ifgreater1 test")
   (check-equal? (eval-exp (ifgreater (int 4) (int 4) (int 3) (int 2))) (int 2) "ifgreater2 test")
   (check-equal? (eval-exp (ifgreater (int 5) (int 4) (int 3) (int 2))) (int 3) "ifgreater3 test")
   (check-equal? (eval-exp (ifgreater (int 5) (int 4) (int 3) (fst (int 2)))) (int 3) "ifgreater4 test")
   (check-exn exn:fail? (λ() (eval-exp (ifgreater (int 3) (int 4) (int 3) (fst (aunit))))) "ifgreater error test")
   ;; snd test
   (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2) "snd test")
   ;; aunit test
   (check-equal? (eval-exp (aunit)) (aunit) "aunit test")
   (check-exn exn:fail? (λ() (eval-exp aunit)) "aunit error test")
   ;; isaunit test
   (check-equal? (eval-exp (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test")
   (check-equal? (eval-exp (isaunit (aunit))) (int 1) "isaunit test")
   (check-equal? (eval-exp (isaunit (int 5))) (int 0) "isaunit wrong test")
   (check-equal? (eval-exp (isaunit (snd (apair (int 6) (aunit))))) (int 1) "isaunit test")
   (check-exn exn:fail? (λ() (eval-exp (isaunit aunit))) "aunit error test")
   ;; mlet test
   (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test")
   
   (check-equal? 
    (eval-exp
     (mlet "sum-to"
           (fun "sum-to" "n"
                (ifgreater (var "n")
                           (int 1)
                           (add (var "n")
                                (call (var "sum-to") (add (var "n") (int -1)))) 
                         (int 1))) 
         (call (var "sum-to") (int 100))))
    (int 5050)
    "call test")
   
   ;; call test
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1))) (int 8) "call test")
   (check-equal? (eval-exp (fun #f "x" (add (var "x") (int 7)))) (closure '() (fun #f "x" (add (var "x") (int 7)))))
   (check-equal? (eval-exp (call (fun #f "x" (add (var "x") (int 7))) (int 1))) (int 8) "call test")
     
   ;; ifaunit test
   (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test")
   
   ;; mlet* test
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test")

   ;; ifeq test
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")
   
   (check-equal? (eval-exp (call (fun #f "x"
                                      (add
                                       (call (fun #f "y" (var "x")) (var "x"))
                                       (int 7)))
                                 (int 1)))
                 (int 8)
                 "call test")
   
   ;; mupl-map test
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit)))) 
                 (apair (int 8) (aunit)) "mupl-map test")
   
   ;; problems 1, 2, and 4 combined test
   (check-equal? (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")
   
   (test-case "examples from the lecture"
    (check-equal? (fun-challenge-freevars
      (compute-free-vars (fun #f "_" (add (add (var "x") (var "y")) (var "z")))))
      (set "x" "y" "z"))
    (check-equal? (fun-challenge-freevars
      (compute-free-vars (fun #f "x" (add (add (var "x") (var "y")) (var "z")))))
      (set "y" "z"))
    (check-equal? (fun-challenge-freevars
      (compute-free-vars (fun #f "x" (ifgreater (var "x") (aunit) (var "y") (var "z")))))
      (set "y" "z"))
    (check-equal? (fun-challenge-freevars
      (compute-free-vars (fun #f "x"
        (mlet "y" (int 0) (add (add (var "x") (var "y")) (var "z"))))))
      (set "z"))
    (check-equal? (fun-challenge-freevars
      (compute-free-vars (fun #f "x" (fun #f "y" (fun #f "z"
        (add (add (var "x") (var "y")) (var "z")))))))
      (set))
    (check-equal? (fun-challenge-freevars
      (compute-free-vars (fun #f "x" 
        (add (var "y")
          (mlet "y" (var "z") (add (var "y") (var "y")))))))
      (set "y" "z")))
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
