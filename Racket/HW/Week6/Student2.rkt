;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 


;; Problem 1

;; CHANGE (put your solutions here)

;(a)
(define (racketlist->mupllist xs)
    (cond [(null? xs) (aunit)]
          [#t (apair (car xs) (racketlist->mupllist (cdr xs)))]))

;(b)
(define (mupllist->racketlist xs)
    (cond [(aunit? xs) null]
          [#t (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))]))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e)
         e]
        [(closure? e)
         e]
        [(aunit? e) 
         e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "ifgreater applied to non-integer")))]
        [(mlet? e)
         (let ([var (mlet-var e)]
               [e1 (eval-under-env (mlet-e e) env)])
           (if (string? var)
               (let ([extended-env (cons (cons var e1) env)])
                 (eval-under-env (mlet-body e) extended-env))
               (error "mlet given a non-string variable name")))]
        [(apair? e)
         (let ([e1 (eval-under-env (apair-e1 e) env)]
               [e2 (eval-under-env (apair-e2 e) env)])
           (apair e1 e2))]
        [(fst? e)
         (let ([ep (eval-under-env (fst-e e) env)])
           (if (apair? ep)
             (eval-under-env (apair-e1 ep) env)
             (error "fst applied to non-pair")))]
        [(snd? e)
         (let ([ep (eval-under-env (snd-e e) env)])
           (if (apair? ep)
             (eval-under-env (apair-e2 ep) env)
             (error "snd applied to non-pair")))]
        [(isaunit? e)
         (let ([e1 (eval-under-env (isaunit-e e) env)])
           (if (aunit? e1)
               (int 1)
               (int 0)))]
        [(fun? e) ;  (nameopt formal body)
         (let ([s1 (fun-nameopt e)]
               [s2 (fun-formal e)]
               [body (fun-body e)])
           (if (and (or (string? s1) (boolean? s1))
                    (string? s2))
               (closure env e)
               (error "one or more incorrect arguments to fun")))]
        [(call? e)
         (let ([e1 (eval-under-env (call-funexp e) env)]
               [e2 (eval-under-env (call-actual e) env)])
           (if (closure? e1)
               (letrec ([func (closure-fun e1)]
                        [s1 (fun-nameopt func)]
                        [s2 (fun-formal func)]
                        [body (fun-body func)]
                        [env-ext1 (cons (cons s2 e2) (closure-env e1))]
                        [env-ext2 (if (string? s1)
                                          (cons (cons s1 e1) env-ext1)
                                          env-ext1)])
                 (eval-under-env body env-ext2))
               (error "call first argument does not evaluate to a closure")))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3
;(define (ifaunit e1 e2 e3) (ifgreater (eval-exp (isaunit e1)) (int 0) e2 e3))
(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (cond [(null? lstlst) e2]
        [#t (mlet* (cdr lstlst) (mlet (car (car lstlst)) (cdr (car lstlst)) e2))]))

(define (ifeq e1 e2 e3 e4) 
  (mlet "_x" e1 (mlet "_y" e2 (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3)))))


;; Problem 4
(define mupl-map
  (fun "map" "fun-arg" (fun #f "xs" (ifaunit (var "xs")
                                             (aunit)
                                             (apair (call (var "fun-arg") (fst (var "xs"))) 
                                                    (call (call (var "map") (var "fun-arg")) (snd (var "xs"))))))))

(define mupl-addN
  (fun "addN" "N" (fun "ret-fun" "xs" (add (var "N") (var "xs")))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (mlet "addN" mupl-addN 
              (fun #f "N" (call (var "map") (call (var "addN") (var "N")))))))

;; Challenge Problem
(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))