;; PL Project - Fall 2022
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file
;; definition of structures for NUMEX programs
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct num  (int)    #:transparent)  ;; a constant number, e.g., (num 17)
(struct plus  (e1 e2)  #:transparent)  ;; add two expressions

;; newly added structs 
(struct div  (e1 e2)  #:transparent)
(struct mult  (e1 e2)  #:transparent)
(struct minus  (e1 e2)  #:transparent)
(struct bool (b) #:transparent)
(struct ifleq  (e1 e2 e3 e4)  #:transparent)
(struct ifnzero  (e1 e2 e3)  #:transparent)
(struct iseq  (e1 e2)  #:transparent)
(struct cnd  (e1 e2 e3)  #:transparent)
(struct orelse  (e1 e2)  #:transparent)
(struct andalso  (e1 e2)  #:transparent)
(struct neg  (e1)  #:transparent)
(struct apair  (e1 e2) #:transparent)
(struct 1st  (e1) #:transparent)
(struct 2nd  (e1) #:transparent)
(struct with  (s e1 e2) #:transparent)

(struct lam  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct tlam  (nameopt formal arg-type body) #:transparent) ;; a typed argument, recursive(?) 1-argument function
(struct apply (funexp actual)       #:transparent) ;; function application



(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then true else false

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env f) #:transparent) 


(struct key  (s e) #:transparent) ;; key holds corresponding value of s which is e
(struct record (k r) #:transparent) ;; record holds several keys
(struct value (s r) #:transparent) ;; value returns corresponding value of s in r

(struct letrec (s1 e1 s2 e2 s3 e3 s4 e4 e5) #:transparent) ;; a letrec expression for recursive definitions

;; Type structures
;; Primitive types are: "int", "bool" and "null"
(struct collection (type) #:transparent) ;; collection of a certain type, e.g., (collection "int")
(struct function (input-type output-type) #:transparent) ;; e.g. (function ("int" int")) means fn f "int" -> "int"

;; Problem 1

(define (racketlist->numexlist xs)
  (cond ((equal? xs null) (munit))
        ((list? xs) (apair (car xs) (racketlist->numexlist (cdr xs))))
        (#t (error "Error in racketlist->numexlist: argument type is not valid"))
        
  ))
(define (numexlist->racketlist xs)
  (cond ((munit? xs) null)
        ((apair? xs) (cons (apair-e1 xs) (numexlist->racketlist (apair-e2 xs))))
        (#t (error "Error in numexlist->racketlist: argument type is not valid"))    
  )
  )

;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(list? env) (cond ((equal? str (car (car env))) (cdr (car env)))
                           (#t (envlookup (cdr env) str)) )]
        [#t (error "Error in envlookup: invalid argument type")]
		)
 )

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.


;; this function gets record and string and returns the value of string
(define (getVal rec str)
  (cond ((munit? rec) rec)
        ((equal? (key-s (record-k rec)) str) (key-e (record-k rec)))
        (#t  (getVal (record-r rec) str))
  ))

(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        
        [(num? e) (cond ((integer? (num-int e)) e) (#t (error "NUMEX num does not contain a numeral value")))]
        [(bool? e) (cond ((boolean? (bool-b e)) e) (#t (error "NUMEX bool does not contain a boolean value")))]
        
        [(munit? e) e]
        [(closure? e) e]
                
        
        [(plus? e) 
         (let ([v1 (eval-under-env (plus-e1 e) env)]
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]

        [(minus? e) 
         (let ([v1 (eval-under-env (minus-e1 e) env)]
               [v2 (eval-under-env (minus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (- (num-int v1) 
                       (num-int v2)))
               (error "NUMEX subtraction applied to non-number")))]

        [(mult? e) 
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1) 
                       (num-int v2)))
               (error "NUMEX multiplication applied to non-number")))]

        [(div? e) 
         (let ([v1 (eval-under-env (div-e1 e) env)]
               [v2 (eval-under-env (div-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (cond ((equal? (num-int v2) 0) (error "NUMEX division by zero"))
                      (#t (num (quotient (num-int v1) (num-int v2)))))
               (error "NUMEX division applied to non-number")))]

        [(andalso? e) 
         (let ([v1 (eval-under-env (andalso-e1 e) env)])
           (if (bool? v1)
               (cond ((equal? v1 (bool #f)) (bool #f))
                     (#t (let ([v2 (eval-under-env (andalso-e2 e) env)])
                       (cond ((bool? v2) (bool (and (bool-b v1) (bool-b v2))))
                             (#t error "NUMEX andalso applied to non-boolean")))))
               (error "NUMEX andalso applied to non-boolean")))]

        [(orelse? e) 
         (let ([v1 (eval-under-env (orelse-e1 e) env)])
           (if (bool? v1)
               (cond ((equal? v1 (bool #t)) (bool #t))
                     (#t (let ([v2 (eval-under-env (orelse-e2 e) env)])
                       (cond ((bool? v2) (bool (or (bool-b v1) (bool-b v2))))
                             (#t error "NUMEX orelse applied to non-boolean")))))
               (error "NUMEX orelse applied to non-boolean")))]

        

        [(neg? e) 
         (let ([v1 (eval-under-env (neg-e1 e) env)])
           (cond  ((bool? v1) (bool (not (bool-b v1))))
                  ((num? v1) (num (* -1 (num-int v1))))  
               (#t (error "NUMEX orelse applied to non-boolean"))))]

        [(cnd? e) 
         (let ([v1 (eval-under-env (cnd-e1 e) env)])
           (if (bool? v1)
               (cond ((equal? v1 (bool #t)) (eval-under-env (cnd-e2 e) env))
                     ((equal? v1 (bool #f)) (eval-under-env (cnd-e3 e) env)))
               (error "NUMEX conditional's condition applied to non-binary")))]

        [(iseq? e) 
         (let ([v1 (eval-under-env (iseq-e1 e) env)]
               [v2 (eval-under-env (iseq-e2 e) env)])
           (if (and (or (bool? v1)
                    (num? v1))
                   (or (bool? v2)
                    (num? v2)) )
               (cond ((equal? v1 v2) (bool #t))
                    (#t (bool #f)))
               (cond ((not (and (or (bool? v1) (num? v1))
                    (or (bool? v2) (num? v2) ))) (error "NUMEX iseq applied to neither boolean or num"))
                     ((not (or (and (bool? v1) (bool? v2))
                    (and (num? v1) (num? v2) ))) (error "NUMEX iseq applied to different types")))
               ))]

        [(ifnzero? e) 
         (let ([v1 (eval-under-env (ifnzero-e1 e) env)])
           (if (num? v1)
               (cond ((not (equal? v1 (num 0))) (eval-under-env (ifnzero-e2 e) env))
                     (#t (eval-under-env (ifnzero-e3 e) env)))
               (error "NUMEX ifnzero's condition applied to non-num")))]

        [(ifleq? e) 
         (let ([v1 (eval-under-env (ifleq-e1 e) env)]
               [v2 (eval-under-env (ifleq-e2 e) env)])
           (if (and (num? v1) (num? v2))
               (cond ((> (num-int v1) (num-int v2)) (eval-under-env (ifleq-e4 e) env))
                     (#t (eval-under-env (ifleq-e3 e) env)))
               (error "NUMEX ifleq's condition applied to non-num")))]

        [(with? e) 
         (let ([v1 (eval-under-env (with-e1 e) env)])
           (if (string? (with-s e))
               (eval-under-env (with-e2 e) (cons (cons (with-s e) v1) env))
               (error "NUMEX with applied to non-string")))]

        [(lam? e)
           (if (and (or (string? (lam-nameopt e)) (equal? (lam-nameopt e) null)) (string? (lam-formal e)))
               (closure env e)
               (error "NUMEX lam applied to non-string"))]

        [(tlam? e)
           (if (and (or (string? (tlam-nameopt e)) (equal? (tlam-nameopt e) null)) (string? (tlam-arg-type e)) (string? (tlam-formal e)))
               (closure env e)
               (error "NUMEX lam applied to non-string"))]

        
        [(apply? e) 
         (let ([v1 (eval-under-env (apply-funexp e) env)])
           (if (closure? v1)
               (cond ((equal? null (lam-nameopt (closure-f v1)))
                      (eval-under-env (lam-body (closure-f v1)) (cons (cons (lam-formal (closure-f v1)) (eval-under-env (apply-actual e) env))  (closure-env v1))) )
                     (#t (eval-under-env (lam-body (closure-f v1)) (cons (cons (lam-nameopt (closure-f v1)) v1)(cons (cons (lam-formal (closure-f v1)) (eval-under-env (apply-actual e) env))  (closure-env v1))))))
               (cond ((lam? v1) (eval-under-env (apply v1 (apply-actual e)) env))
               (#t (error "NUMEX ~v not a function" (apply-funexp e)) ))))]

        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]

        [(1st? e) 
         (let ([v1 (eval-under-env (1st-e1 e) env)])
           (if (apair? v1)
               (apair-e1 v1)
               (error "NUMEX 1st applied to non-apair")))]

         [(2nd? e) 
          (let ([v1 (eval-under-env (2nd-e1 e) env)])
           (if (apair? v1)
               (apair-e2 v1)
               (error "NUMEX 2nd applied to non-apair")))]

         [(ismunit? e) 
          (let ([v1 (eval-under-env (ismunit-e e) env)])
           (cond ((munit? v1) (bool #t))
               (#t (bool #f))))]

         [(letrec? e)
           (cond ((and (string? (letrec-s1 e)) (string? (letrec-s2 e)) (string? (letrec-s3 e)) (string? (letrec-s4 e)))
                  (eval-under-env (letrec-e5 e) (cons (cons (letrec-s1 e) (letrec-e1 e)) (cons (cons (letrec-s2 e) (letrec-e2 e))
                                                                                                          (cons (cons (letrec-s3 e) (letrec-e3 e))
                                                                                                          (cons (cons (letrec-s4 e) (letrec-e4 e)) env))))))   
               (#t (error "NUMEX letrec applied to non-string")))]



         [(key? e) 
          (let ([v1 (eval-under-env (key-e e) env)])
           (cond  ((string? (key-s e))  (key (key-s e) v1))
               (#t (error "NUMEX key applied to non-string"))))]

         [(record? e) 
          (let ([v1 (eval-under-env (record-k e) env)]
                [v2 (eval-under-env (record-r e) env)])
           (if (key? v1)
               (cond ((or (munit? v2) (record? v2)) (record v1 v2))
                     (#t (error "NUMEX record input should be a record or munit")))
                     (error "NUMEX record input is not a key")))]

         [(value? e) 
           (let ([v1 (eval-under-env (value-r e) env)])
             (if  (string? (value-s e))
                 (cond ((record? v1) (getVal v1 (value-s e))) (#t  (error "NUMEX value input is not a record")))
               (error "NUMEX value input is not a string")))]
         
     
        [(string? e) e]
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3
;; Complete more cases for other kinds of NUMEX expressions.
;; We will test infer-under-env by calling its helper function, infer-exp.
(define (infer-under-env e env)
  (cond [(var? e) 
         (infer-under-env (envlookup env (var-string e)) env)]

        [(plus? e) 
         (let ([t1 (infer-under-env (plus-e1 e) env)]
               [t2 (infer-under-env (plus-e2 e) env)])
           (if (and (equal? "int" t1)
                    (equal? "int" t2))
               "int"
               (error "NUMEX TYPE ERROR: addition applied to non-integer")))]

        [(num? e)
         (cond
           [(integer? (num-int e)) "int"]
           [#t (error "NUMEX TYPE ERROR: num should be a constant number")])]

        [(bool? e)
         (cond
           [(boolean? (bool-b e)) "bool"]
           [#t (error "NUMEX TYPE ERROR: bool should be #t or #f")])]

        [(andalso? e) 
         (let ([t1 (infer-under-env (andalso-e1 e) env)]
               [t2 (infer-under-env (andalso-e2 e) env)])
           (if (and (equal? "bool" t1)
                    (equal? "bool" t2))
               "bool"
               (error "NUMEX TYPE ERROR: andalso applied to non-boolean")))]

        [(neg? e)
         (cond
           [(equal? (infer-under-env (neg-e1 e) env) "bool") "bool"]
           [(equal? (infer-under-env (neg-e1 e) env) "int") "int"]
           [#t (error "NUMEX TYPE ERROR: negation should be applied to boolean or integer type")])]

        [(cnd? e) 
         (let ([t (infer-under-env (cnd-e1 e) env)])
           (if (equal? t "bool")
               (cond ((equal? (infer-under-env (cnd-e2 e) env) (infer-under-env (cnd-e3 e) env)) (infer-under-env (cnd-e2 e) env) )
                     (#t (error "NUMEX TYPE ERROR: cnd branches should have the same type")))                    
               (error "NUMEX TYPE ERROR: condition should be a boolean value")))]

         [(iseq? e)
             (cond ((equal? (infer-under-env (iseq-e1 e) env) (infer-under-env (iseq-e2 e) env)) "bool" )
                     (#t (error "NUMEX TYPE ERROR: iseq inputs should have the same type")))]

         [(with? e) 
           (if (string? (with-s e))
               (infer-under-env (with-e2 e) (cons (cons (with-s e) (infer-under-env (with-e1 e) env)) env))               
               (error "NUMEX TYPE ERROR: with should be applied to a string"))]

         [(tlam? e)
          (if (or (equal? (tlam-arg-type e) "int") (equal? (tlam-arg-type e) "bool") (equal? (tlam-arg-type e) "null")) 
           (if (and (or (string? (tlam-nameopt e)) (equal? (tlam-nameopt e) null)) (string? (tlam-formal e)) (string? (tlam-arg-type e)))
               (function  (tlam-arg-type e) (infer-under-env (tlam-body e) (cons (cons (tlam-formal e)  (tlam-arg-type e)) env)))  
               (error "NUMEX TYPE ERROR: tlam applied to non-string"))
              (error "NUMEX TYPE ERROR: tlam arg-type not recognized")
             )]

         [(apply? e) 
          (let ([t1 (infer-under-env (apply-funexp e) env)]
               [t2 (infer-under-env (apply-actual e) env)])
           (if (function? t1)
                    (cond ((equal? (function-input-type t1) t2) (function-output-type t1))
                          (#t  (error "NUMEX TYPE ERROR: apply argument type mismatch")))
               (error "NUMEX TYPE ERROR: apply input is not a function")))]

         [(munit? e)
         "null"]

        [(apair? e) 
         (let ([t1 (infer-under-env (apair-e1 e) env)]
               [t2 (infer-under-env (apair-e2 e) env)])
           (if (or (equal? t2 (collection t1)) (equal? t2 "null"))
                    (collection t1)
               (error "NUMEX TYPE ERROR: apair's second input should be null or a collection with the correct type")))]

        [(1st? e) 
         (let ([t1 (infer-under-env (1st-e1 e) env)])
           (if (collection? t1)
                    (collection-type t1)
               (error "NUMEX TYPE ERROR: 1st input is not apairs")))]

        [(2nd? e) 
         (let ([t1 (infer-under-env (2nd-e1 e) env)])
           (if (collection? t1)
                    t1
               (error "NUMEX TYPE ERROR: 2nd input is not apairs")))]

        [(ismunit? e) 
         (let ([t1 (infer-under-env (ismunit-e e) env)])
           (if (or (collection? t1) (equal? t1 "null"))
                    "bool"
               (error "NUMEX TYPE ERROR: ismunit input has an invalid type")))]

        

        ;; CHANGE add more cases here
        [(string? e) e]
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (infer-exp e)
  (infer-under-env e null))

;; Problem 4

(define (ifmunit e1 e2 e3)
  (cnd (ismunit e1) e2 e3)
  )

(define (with* bs e2)
  (cond ((null? bs) e2)
        (#t (with (car (car bs)) (cdr (car bs)) (with* (cdr bs) e2)))))

(define (ifneq e1 e2 e3 e4)
 (cnd (iseq e1 e2) e4 e3))

;; Problem 5

(define numex-filter
  (lam null "function" (lam "recursion" "l"
                     (cnd (ismunit (var "l")) (munit)
                          (ifnzero (apply (var "function") (1st (var "l"))) 
                                      (apair (apply (var "function") (1st (var "l"))) (apply (var "recursion") (2nd (var "l"))))
                                     (apply (var "recursion") (2nd (var "l"))) )
                          ) ) ))


(define numex-all-gt
  (with "f" numex-filter
        (lam null "i" (apply numex-filter (lam "g" "x" (ifleq (var "x") (var "i") (num 0) (var "x"))) ))
        ))

;; Problem 6

;; two parts of condition's types are not identical. but evaluates without error.
(define type-error-but-evaluates-ok
  (cnd (bool #t)
       (bool #t)
       (num 8))
  )

;; results in addition of num and munit
(define type-ok-but-evaluates-error
  (plus (num 5)
        (1st
         (2nd
          (apair (num 5) (munit))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment

;; returns a cons cell. first part is the changed expression and the second part is set of free vars in e
(define (compute-free-e-set e)
  (cond [(var? e) (cons e (set (var-string e)))]
        
        ;; values have no free variable
        [(num? e) (cons e (set))]
        [(bool? e) (cons e (set))]
        [(munit? e) (cons e (set))]
        [(closure? e) (cons e (set))]
        
        ;; for arithmetic operators, free variables are union of free variables of operands
        [(plus? e) 
         (let ([c1 (compute-free-e-set (plus-e1 e))]
               [c2 (compute-free-e-set (plus-e2 e))])
           (cons (plus (car c1) (car c2)) (set-union (cdr c1) (cdr c2) )))]

        [(minus? e) 
         (let ([c1 (compute-free-e-set (minus-e1 e))]
               [c2 (compute-free-e-set (minus-e2 e))])
           (cons (minus (car c1) (car c2)) (set-union (cdr c1) (cdr c2) ))
           )]

        [(mult? e) 
         (let ([c1 (compute-free-e-set (mult-e1 e))]
               [c2 (compute-free-e-set (mult-e2 e))])
           (cons (mult (car c1) (car c2)) (set-union (cdr c1) (cdr c2)))
           )]

        [(div? e) 
         (let ([c1 (compute-free-e-set (div-e1 e))]
               [c2 (compute-free-e-set (div-e2 e))])
           (cons (div (car c1) (car c2)) (set-union (cdr c1) (cdr c2) ))
           )]

        ;; logical operators are identical to arithmetic operators
        [(andalso? e) 
         (let ([c1 (compute-free-e-set (andalso-e1 e))]
               [c2 (compute-free-e-set (andalso-e2 e))])
           (cons (andalso (car c1) (car c2)) (set-union (cdr c1) (cdr c2) ))
           )]

        [(orelse? e) 
         (let ([c1 (compute-free-e-set (orelse-e1 e))]
               [c2 (compute-free-e-set (orelse-e2 e))])
           (cons (orelse (car c1) (car c2)) (set-union (cdr c1) (cdr c2) ))
           )]

        
        [(neg? e) 
         (let ([c1 (compute-free-e-set (neg-e1 e))])
           (cons (neg (car c1)) (cdr c1)))]

        [(cnd? e) 
         (let ([c1 (compute-free-e-set (cnd-e1 e))]
               [c2 (compute-free-e-set (cnd-e2 e))]
               [c3 (compute-free-e-set (cnd-e3 e))])
           (cons (cnd (car c1) (car c2) (car c3) ) (set-union (cdr c1) (cdr c2) (cdr c3))))]

        [(iseq? e) 
         (let ([c1 (compute-free-e-set (iseq-e1 e))]
               [c2 (compute-free-e-set(iseq-e2 e))])
           (cons (iseq (car c1) (car c2)) (set-union (cdr c1) (cdr c2) ))
           )]

        [(ifnzero? e) 
         (let ([c1 (compute-free-e-set (ifnzero-e1 e))]
               [c2 (compute-free-e-set (ifnzero-e2 e))]
               [c3 (compute-free-e-set (ifnzero-e3 e))])
           (cons (ifnzero (car c1) (car c2) (car c3) ) (set-union (cdr c1) (cdr c2) (cdr c3))))]

        [(ifleq? e) 
         (let ([c1 (compute-free-e-set (ifleq-e1 e))]
               [c2 (compute-free-e-set (ifleq-e2 e))]
               [c3 (compute-free-e-set (ifleq-e3 e))]
               [c4 (compute-free-e-set (ifleq-e4 e))])
           (cons (ifleq (car c1) (car c2) (car c3) (car c4) ) (set-union (cdr c1) (cdr c2) (cdr c3) (cdr c4))))]

        ;; s is removed from e2 free-vars. e1 is unioned with e2
        [(with? e) 
         (let ([c1 (compute-free-e-set (with-e1 e))]
               [c2 (compute-free-e-set (with-e2 e))])
           (cons (with  (with-s e) (car c1) (car c2)) (set-union (set-remove (cdr c2) (with-s e) ) (cdr c1))))]
        
        ;; lam should be converted to fun-challenge. func name and formal parameter is bounded in the body
        [(lam? e)
         (let ([c1 (compute-free-e-set (lam-body e))])
           (let ([c2 (cdr c1)])
           (cons (fun-challenge (lam-nameopt e) (lam-formal e) (car c1) (set-remove (set-remove c2 (lam-nameopt e)) (lam-formal e))) (set-remove (set-remove c2 (lam-nameopt e)) (lam-formal e))) ))]

        [(tlam? e)
         (let ([c1 (compute-free-e-set (tlam-body e))])
           (let ([c2 (cdr c1)])
           (cons (fun-challenge (tlam-nameopt e) (tlam-formal e) (car c1) (set-remove (set-remove c2 (tlam-nameopt e)) (tlam-formal e))) (set-remove (set-remove c2 (tlam-nameopt e)) (tlam-formal e))) ))]

        ;; apply is just like the others. nothing surprising!
        [(apply? e) 
         (let ([c1 (compute-free-e-set (apply-funexp e))]
               [c2 (compute-free-e-set (apply-actual e))])
           (cons (apply (car c1) (car c2)) (set-union (cdr c1) (cdr c2) )))]

                
        [(apair? e)
         (let ([c1 (compute-free-e-set (apair-e1 e))]
               [c2 (compute-free-e-set (apair-e2 e))])
           (cons (apair (car c1) (car c2)) (set-union (cdr c1) (cdr c2) )))]
        
        [(1st? e) 
         (let ([c1 (compute-free-e-set (1st-e1 e))])
           (cons (1st (car c1)) (cdr c1)))]

        [(2nd? e) 
         (let ([c1 (compute-free-e-set (2nd-e1 e))])
           (cons (2nd (car c1)) (cdr c1)))]

         [(ismunit? e) 
         (let ([c1 (compute-free-e-set (ismunit-e e))])
           (cons (ismunit (car c1)) (cdr c1)))]

         [(letrec? e) 
         (let ([c1 (compute-free-e-set (letrec-e1 e))]
               [c2 (compute-free-e-set (letrec-e2 e))]
               [c3 (compute-free-e-set (letrec-e3 e))]
               [c4 (compute-free-e-set (letrec-e4 e))]
               [c5 (compute-free-e-set (letrec-e5 e))])
           (cons (letrec (letrec-s1 e) (car c1) (letrec-s2 e) (car c2) (letrec-s3 e) (car c3) (letrec-s4 e) (car c4) (car c5)) (set-union (cdr c1) (cdr c2) (cdr c3) (cdr c4) (cdr c5))))]


         [(key? e) 
          (let ([c1 (compute-free-e-set (key-e e))])
           (cons (key (key-s e) (car c1)) (cdr c1)))]

         [(record? e) 
         (let ([c1 (compute-free-e-set (record-k e))]
               [c2 (compute-free-e-set (record-r e))])
           (cons (record (car c1) (car c2)) (set-union (cdr c1) (cdr c2) ))
           )]

         [(value? e) 
         (let ([c1 (compute-free-e-set (value-r e))])
           (cons (value (value-s e) (car c1)) (cdr c1)))]
         
     
        [(string? e) e]
        [#t (error (format "bad NUMEX expression: ~v" e))]))

(define (compute-free-vars e)
  (let ([c1 (compute-free-e-set e)])
    (car c1)
  ))
  

 
;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes


;; createNewEnv gets frees(free variables name) of a function and an environment and returns an environment for closure env f that only contains free-variables of function
(define (createNewEnv frees env)
  (cond ((null? env) null)
         (#t (cond ((set-member? frees (car (car env))) (cons (car env) (createNewEnv frees (cdr env))))
                   (#t (createNewEnv frees (cdr env)))

                   ))
  ))

(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        
        [(num? e) (cond ((integer? (num-int e)) e) (#t (error "NUMEX num does not contain a numeral value")))]
        [(bool? e) (cond ((boolean? (bool-b e)) e) (#t (error "NUMEX bool does not contain a boolean value")))]
        
        [(munit? e) e]
        [(closure? e) e]
        
        [(plus? e) 
         (let ([v1 (eval-under-env-c (plus-e1 e) env)]
               [v2 (eval-under-env-c (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]

        [(minus? e) 
         (let ([v1 (eval-under-env-c (minus-e1 e) env)]
               [v2 (eval-under-env-c (minus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (- (num-int v1) 
                       (num-int v2)))
               (error "NUMEX subtraction applied to non-number")))]

        [(mult? e) 
         (let ([v1 (eval-under-env-c (mult-e1 e) env)]
               [v2 (eval-under-env-c (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1) 
                       (num-int v2)))
               (error "NUMEX multiplication applied to non-number")))]

        [(div? e) 
         (let ([v1 (eval-under-env-c (div-e1 e) env)]
               [v2 (eval-under-env-c (div-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (cond ((equal? (num-int v2) 0) (error "NUMEX division by zero"))
                      (#t (num (quotient (num-int v1) (num-int v2)))))
               (error "NUMEX division applied to non-number")))]

        [(andalso? e) 
         (let ([v1 (eval-under-env-c (andalso-e1 e) env)])
           (if (bool? v1)
               (cond ((equal? v1 (bool #f)) (bool #f))
                     (#t (let ([v2 (eval-under-env-c (andalso-e2 e) env)])
                       (cond ((bool? v2) (bool (and (bool-b v1) (bool-b v2))))
                             (#t error "NUMEX andalso applied to non-boolean")))))
               (error "NUMEX andalso applied to non-boolean")))]

        [(orelse? e) 
         (let ([v1 (eval-under-env-c (orelse-e1 e) env)])
           (if (bool? v1)
               (cond ((equal? v1 (bool #t)) (bool #t))
                     (#t (let ([v2 (eval-under-env-c (orelse-e2 e) env)])
                       (cond ((bool? v2) (bool (or (bool-b v1) (bool-b v2))))
                             (#t error "NUMEX orelse applied to non-boolean")))))
               (error "NUMEX orelse applied to non-boolean")))]

        

        [(neg? e) 
         (let ([v1 (eval-under-env-c (neg-e1 e) env)])
           (cond  ((bool? v1) (bool (not (bool-b v1))))
                  ((num? v1) (num (* -1 (num-int v1))))  
               (#t (error "NUMEX orelse applied to non-boolean"))))]

        [(cnd? e) 
         (let ([v1 (eval-under-env-c (cnd-e1 e) env)])
           (if (bool? v1)
               (cond ((equal? v1 (bool #t)) (eval-under-env-c (cnd-e2 e) env))
                     ((equal? v1 (bool #f)) (eval-under-env-c (cnd-e3 e) env)))
               (error "NUMEX conditional's condition applied to non-binary")))]

        [(iseq? e) 
         (let ([v1 (eval-under-env-c (iseq-e1 e) env)]
               [v2 (eval-under-env-c (iseq-e2 e) env)])
           (if (and (or (bool? v1)
                    (num? v1))
                   (or (bool? v2)
                    (num? v2)) )
               (cond ((equal? v1 v2) (bool #t))
                    (#t (bool #f)))
               (cond ((not (and (or (bool? v1) (num? v1))
                    (or (bool? v2) (num? v2) ))) (error "NUMEX iseq applied to neither boolean or num"))
                     ((not (or (and (bool? v1) (bool? v2))
                    (and (num? v1) (num? v2) ))) (error "NUMEX iseq applied to different types")))
               ))]

        [(ifnzero? e) 
         (let ([v1 (eval-under-env-c (ifnzero-e1 e) env)])
           (if (num? v1)
               (cond ((not (equal? v1 (num 0))) (eval-under-env-c (ifnzero-e2 e) env))
                     (#t (eval-under-env-c (ifnzero-e3 e) env)))
               (error "NUMEX ifnzero's condition applied to non-num")))]

        [(ifleq? e) 
         (let ([v1 (eval-under-env-c (ifleq-e1 e) env)]
               [v2 (eval-under-env-c (ifleq-e2 e) env)])
           (if (and (num? v1) (num? v2))
               (cond ((> (num-int v1) (num-int v2)) (eval-under-env-c (ifleq-e4 e) env))
                     (#t (eval-under-env-c (ifleq-e3 e) env)))
               (error "NUMEX ifleq's condition applied to non-num")))]

        [(with? e) 
         (let ([v1 (eval-under-env-c (with-e1 e) env)])
           (if (string? (with-s e))
               (eval-under-env-c (with-e2 e) (cons (cons (with-s e) v1) env))
               (error "NUMEX with applied to non-string")))]

        [(fun-challenge? e)
           (if (and (or (string? (fun-challenge-nameopt e)) (equal? (fun-challenge-nameopt e) null)) (string? (fun-challenge-formal e)))
               (let ([frees (createNewEnv (fun-challenge-freevars e) env)])
               (closure frees e))
                 (error "NUMEX lam applied to non-string"))]

        
        [(apply? e) 
         (let ([v1 (eval-under-env-c (apply-funexp e) env)])
           (if (closure? v1)
               (cond ((equal? null (lam-nameopt (closure-f v1)))
                      (eval-under-env-c (lam-body (closure-f v1)) (cons (cons (lam-formal (closure-f v1)) (eval-under-env-c (apply-actual e) env))  (closure-env v1))) )
                     (#t (eval-under-env-c (lam-body (closure-f v1)) (cons (cons (lam-nameopt (closure-f v1)) v1)(cons (cons (lam-formal (closure-f v1)) (eval-under-env-c (apply-actual e) env))  (closure-env v1))))))
               (cond ((lam? v1) (eval-under-env-c (apply v1 (apply-actual e)) env))
               (#t (error "NUMEX ~v not a function" (apply-funexp e)) ))))]

        [(apair? e)
         (let ([v1 (eval-under-env-c (apair-e1 e) env)]
               [v2 (eval-under-env-c (apair-e2 e) env)])
           (apair v1 v2))]

        
        [(1st? e) 
         (let ([v1 (eval-under-env-c (1st-e1 e) env)])
           (if (apair? v1)
               (apair-e1 v1)
               (error "NUMEX 1st applied to non-apair")))]

         [(2nd? e) 
          (let ([v1 (eval-under-env-c (2nd-e1 e) env)])
           (if (apair? v1)
               (apair-e2 v1)
               (error "NUMEX 2nd applied to non-apair")))]

         [(ismunit? e) 
          (let ([v1 (eval-under-env-c (ismunit-e e) env)])
           (cond ((munit? v1) (bool #t))
               (#t (bool #f))))]

         [(letrec? e)
           (cond ((and (string? (letrec-s1 e)) (string? (letrec-s2 e)) (string? (letrec-s3 e)) (string? (letrec-s4 e)))
                  (eval-under-env-c (letrec-e5 e) (cons (cons (letrec-s1 e) (letrec-e1 e)) (cons (cons (letrec-s2 e) (letrec-e2 e))
                                                                                                          (cons (cons (letrec-s3 e) (letrec-e3 e))
                                                                                                          (cons (cons (letrec-s4 e) (letrec-e4 e)) env))))))   
               (#t (error "NUMEX letrec applied to non-string")))]



         [(key? e) 
          (let ([v1 (eval-under-env-c (key-e e) env)])
           (cond  ((string? (key-s e))  (key (key-s e) v1))
               (#t (error "NUMEX key applied to non-string"))))]

         [(record? e) 
          (let ([v1 (eval-under-env-c (record-k e) env)]
                [v2 (eval-under-env-c (record-r e) env)])
           (if (key? v1)
               (cond ((or (munit? v2) (record? v2)) (record v1 v2))
                     (#t (error "NUMEX record input should be a record or munit")))
                     (error "NUMEX record input is not a key")))]

         [(value? e) 
           (let ([v1 (eval-under-env-c (value-r e) env)])
             (if  (string? (value-s e))
                 (cond ((record? v1) (getVal v1 (value-s e))) (#t  (error "NUMEX value input is not a record")))
               (error "NUMEX value input is not a string")))]
         
     
        [(string? e) e]
        [#t (error (format "bad NUMEX expression: ~v" e))]))


;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))









