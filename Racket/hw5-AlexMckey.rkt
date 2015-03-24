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

(define (racketlist->mupllist lst)
  (cond [(pair? lst) (apair (car lst)(racketlist->mupllist (cdr lst)))]
        [(null? lst) (aunit)]
        [#t (error "MUPL: not a list")]))

(define (mupllist->racketlist lst)
  (cond [(apair? lst) (cons (apair-e1 lst) (mupllist->racketlist (apair-e2 lst)))]
        [(aunit? lst) null]
        [#t (error "MUPL: not a MUPLlist")]))

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
        [(int? e)
         (int (int-num e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(apair? e)
         (let ([p1 (eval-under-env (apair-e1 e) env)]
               [p2 (eval-under-env (apair-e2 e) env)])
           (apair p1 p2))]
         ;;(apair (apair-e1 e) (apair-e2 e))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-pair")))]
        [(ifgreater? e)
         (let ([i1 (eval-under-env (ifgreater-e1 e) env)]
               [i2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? i1)
                    (int? i2))
               (if (> (int-num i1) (int-num i2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number in condition")))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(aunit? e) (aunit)]
        [(mlet? e)
         (let ([s (mlet-var e)]
               [v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons s v) env)))]
        [(fun? e)
         (let ([name (fun-nameopt e)]
               [param (fun-formal e)])
           (if (and (or (string? name)
                        (not name))
                    (string? param))
               (closure env (fun name param (fun-body e)))
               (error "MUPL fun name or param no a string")))]
        [(call? e)
         (let ([e1 (eval-under-env (call-funexp e) env)])
           (if (closure? e1)
               (let* ([e2 (eval-under-env (call-actual e) env)]
                      [s1 (fun-nameopt (closure-fun e1))]
                      [s2 (fun-formal (closure-fun e1))]
                      [all-env (cons (cons s2 e2)
                                     (cons (cons s1 e1)
                                           (closure-env e1)))])
                 (eval-under-env (fun-body (closure-fun e1)) all-env))
               (error "MUPL call fun that not closure")))]
        [(closure? e)
         (closure (closure-env e) (closure-fun e))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst))
            (cdr (car lstlst))
            (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet*
   (list (cons "_x" e1) (cons "_y" e2))
   (ifgreater (var "_x") (var "_y")
              e4
              (ifgreater (var "_y") (var "_x")
                         e4
                         e3))))

;; Problem 4

(define mupl-map 
  (fun "map" "f"
     (fun #f "lst"
          (ifaunit (var "lst")
                   (aunit)
                   (apair (call (var "f") (fst (var "lst")))
                          (call (call (var "map") (var "f")) (snd (var "lst"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i" (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))

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

