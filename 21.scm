#lang racket

;; aux function to remove the last element in list
(define (remove-last lst)
  (if (null? lst) lst
      (reverse (cdr (reverse lst)))
      ))

;; comptibality to racket
(define empty? null?)

(define append-map  (lambda(func lst)(apply append (map func lst))))

(define (cartesian-product . lists)
  (fold-right (lambda (xs ys)
                (append-map (lambda (x)
                              (map (lambda (y)
                                     (cons x y))
                                   ys))
                            xs))
              '(())
              lists))

(define (first lst) (car lst))

(define (last lst) (car (reverse lst)))


;; this function creating all the pairs of (i, m -i) from 0 to m , aux function for application structre
(define matching-p-to-q
  (lambda(sub-combinators)
  (letrec ((run (lambda(sub-combs acc)
                  (if (empty? sub-combs)
                      acc
                      (if (= (length sub-combs) 1)
                          (cons (cons (car sub-combs) (car sub-combs)) acc)
                          (run (remove-last (cdr sub-combs)) (cons (cons (first sub-combs) (last sub-combs)) (cons (cons (last sub-combs) (first sub-combs)) acc)))
                      ))
                  )))
    (run sub-combinators '() ))))

; aux function to generate new variable
(define (get-new-var count)
       (string->symbol (string-append "v" (number->string count))))

; aux function for iterating over sub-exprs
(define my-range (lambda (a b)
  (cond 
        [(= (+ a 1) b) (list a b)]
        [(= a b) `(,a)]
        [(> (+ a 1) b) '()]
        [else (cons a (my-range (+ 1 a) b))])))



;; this function generates combinators in the length <n>
(define gen-comb
  (lambda(n)
    (letrec ((gen-comb (lambda (n vars count is-application)
                         (cond
                           [(= n 0) '()]
                           [(= n 1) (if
                                     (null? vars)
                                     (if is-application
                                         '()
                                         `(,(get-new-var count))
                                         )
                                     vars)
                                     ]
                           [else
                            (let* ((fresh-var (get-new-var count))
                                   (sub-comb-n-1 (gen-comb (- n 1) (cons fresh-var vars) (+ count 1) #t))
                                   ;; the recursive call to lambda-structre
                                   (lambda-exp (map (lambda(sub-expr) `(lambda (,fresh-var) ,sub-expr)) sub-comb-n-1))
                                   ;; paramaters for the application structre 
                                   (max-m (- n 2))
                                   (range (my-range 1 max-m)) ;; all the posibles m's
                                   (combs_p (map (lambda(m) (gen-comb m vars count #t)) range)) ;computing all the comb for 1 <= i <= n-2
                                   ; creating all the the matching pairs to n-1
                                   (p-q-pairs (matching-p-to-q combs_p))
                                   ; removing nulls
                                   (p-q-pairs (filter (lambda(e) (not (or (null? (car e)) (null? (cdr e)))))  p-q-pairs))
                                   ;computing the cartasian product of each matching pair 
                                   (application-pairs-list (map (lambda(p) (cartesian-product (car p) (cdr p))) p-q-pairs))
                                   ; creating the application structre
                                   (application
                                    ; removing nulls (you never can't be to safe :) )
                                    (filter (lambda(e) (not (null? e)))
                                                        ; creating the application expression
                                                        (map (lambda(cp) (map (lambda(match)`(,(car match) ,(cadr match) ) ) cp)) application-pairs-list)))
                                   ; removing the redunedent parenthesis
                                   (application (if (null? application) '() (car application) ))
                                   )
                              (append
                               lambda-exp
                               application
                               ))                              
                              ]))))
      (gen-comb n '() 0 #f))
    )
  )

; this function returns stream of all combinators
; based on the implementation of (gen-combs) enumarting the stream by length
(define combinators-stream
  (letrec ((run
            (lambda (n curr-list)
              (if (empty? curr-list)
                  (let* ((n (+ n 1))
                         (combs-n (gen-comb n)))
                    (cons (car combs-n) (lambda () (run n (cdr combs-n)))))
                  (cons (car curr-list) (lambda () (run n (cdr curr-list))))))))
    (run 0 '())))

; generic stream functions
(define stream-car (lambda (stream) (car stream)))

(define stream-cdr (lambda (stream) ((cdr stream))))
(define stream-cons
  (lambda (e stream)
    (cons e
          (lambda () stream))))

(define stream-nil '())

(define stream+count->list
  (lambda (stream n)
    (cond ((zero? n) '())
          ((null? stream) '())
          (else (cons (stream-car stream)
                      (stream+count->list
                       (stream-cdr stream)
                       (- n 1)))))))
     

; s-expression equality
(define (test-21)
  (and (equal? '(v0) (gen-comb 1))
       (equal? '((lambda (v0) v0)) (gen-comb 2))
       (equal? '((lambda (v0) (lambda (v1) v1))(lambda (v0) (lambda (v1) v0))) (gen-comb 3))
       (equal?
        '((lambda (v0) (lambda (v1) (lambda (v2) v2)))(lambda (v0) (lambda (v1) (lambda (v2) v1)))(lambda (v0) (lambda (v1) (lambda (v2) v0)))(lambda (v0) (v0 v0)))
        (gen-comb 4))
       ))

(define (test)
  (and (equal? (stream+count->list combinators-stream 1) (gen-comb 1)) #t))
                                                 