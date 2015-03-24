(define hanoi
    (mlet* (list
          (cons "mappend" 
                (fun "mappend" "lst"
                     (ifaunit (var "lst")
                              (aunit)
                              (mlet "hd" (fst (var "lst"))
                                    (ifaunit (var "hd")
                                             (call (var "mappend")
                                                   (snd (var "lst")))
                                             (apair (fst (var "hd"))                                      
                                                    (call (var "mappend")
                                                          (apair (snd (var "hd"))
                                                                 (snd (var "lst"))))))))))
          (cons "free-stick"
                (fun #f "pr"
                     (mlet "sum" (add (fst (var "pr")) (snd (var "pr")))
                           (ifeq (var "sum") (int 3)
                                 (int 3)
                                 (ifeq (var "sum") (int 4)
                                       (int 2)
                                       (int 1))))))
          (cons "move"
                (fun "move" "prob"
                     (mlet* (list
                             (cons "height" (fst (var "prob")))
                             (cons "from" (fst (snd (var "prob"))))
                             (cons "to" (snd (snd (var "prob"))))
                             (cons "free" (call (var "free-stick")
                                                (apair (var "from") (var "to")))))
                            (ifeq (var "height") (int 1)
                                  (racketlist->mupllist (list (apair (var "from") (var "to"))))
                                  (call (var "mappend")
                                        (racketlist->mupllist
                                         (list (call (var "move")
                                                     (apair (add (var "height") (int -1))
                                                            (apair (var "from")
                                                                   (var "free"))))
                                               (racketlist->mupllist (list (apair (var "from") (var "to"))))
                                               (call (var "move")
                                                     (apair (add (var "height") (int -1))
                                                            (apair (var "free")
                                                                   (var "to"))))))))))))
         (fun #f "height"
              (call (var "move")
                    (apair (var "height")
                           (apair (int 1)
                                  (int 3)))))))
  
;;(mupllist->racketlist (eval-exp (call hanoi (int 3))))
  
  (define mupl-filter  
  (fun "mupl-filter-curried" "filter-fun"  
       (fun "mupl-filter-inner" "filter-list"  
            (ifaunit (var "filter-list")  
                     (aunit)  
             (mlet "cur" (fst (var "filter-list"))  
               (ifeq (int 0)  
                 (call (var "filter-fun") (var "cur"))  
                 (call (var "mupl-filter-inner") (snd (var "filter-list")))  
                 (apair (var "cur") (call (var "mupl-filter-inner") (snd (var "filter-list"))))))))))  

(define mupl-prepend  
  (fun "mupl-prepend" "list1"  
       (fun "mupl-prepend-inner" "list2"  
        (ifaunit (var "list2")  
             (var "list1")  
             (apair (fst (var "list2")) (call (var "mupl-prepend-inner")  
                              (snd (var "list2"))))))))  

(define mupl-append ; Just for nicety  
  (fun "mupl-append" "list1"  
       (fun "mupl-append-inner" "list2"  
        (call (call mupl-prepend (var "list2")) (var "list1")))))  

(define mupl-hanoi (fun "hanoi" "discs"  
              (fun "hanoi-2" "from"  
               (fun "hanoi-3" "to"  
                (ifeq (int 1)  
                      (var "discs")  
                      (apair (apair (var "from") (var "to")) (aunit))  
                      (mlet "tempto"  
                        (fst (call (call mupl-filter  
                                 (fun #f "x"  
                                  (ifeq (var "x")  
                                    (var "from")  
                                    (int 0)  
                                    (ifeq (var "x")  
                                          (var "to")  
                                          (int 0)  
                                          (int 1)))))  
                               (racketlist->mupllist (list (int 1) (int 2) (int 3)))))  
                        (mlet "movefirst"  
                          (add (var "discs") (int -1))  
                          (call (call mupl-append  
                                  (call (call (call (var "hanoi")  
                                        (var "movefirst"))  
                                      (var "from"))  
                                    (var "tempto")))  
                            (apair (apair (var "from") (var "to"))  
                                   (call (call (call (var "hanoi")  
                                         (var "movefirst"))  
                                       (var "tempto"))  
                                     (var "to")))))))))))

;; (eval-exp (call (call (call mupl-hanoi (int 3)) (int 1)) (int 3)))