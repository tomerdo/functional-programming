#lang racket

(define lambda-string "(lambda (")

(define close-brackets (lambda (len)
                         (make-string len #\))))

; used racket (due to the editor) and now import to chez
(define empty? null?)


(define combinators
  (lambda(n)
    (set! count 0)
    (letrec ((combinators (lambda (n acc vars)
                            (cond [(zero? n) acc]
                                  [(= n 1) 
                                            (if (empty? vars)
                                                `(,(get-new-var))
                                                (if (empty? acc)
                                                    vars
                                                    (map (lambda(var)
                                                           (map (lambda(expr)`(,expr ,var)) acc))
                                                         vars)))]
                                  [else
                                    (let* ((fresh-var (get-new-var))
                                           (fresh-lambda `(lambda (,fresh-var)))
                                           )
                                     (append
                                      ; lambda combinator
                                      (combinators (- n 1)
                                                    (if (empty? acc)
                                                        `(,fresh-lambda)
                                                        (map (lambda(expr) `(,expr (lambda (,fresh-var))))  acc)
                                                        )
                                                 ;   )
                                                   (append vars `(,fresh-var))
                                                   )
                                      
                                      ; application combinators
                                      (if (< n 4)
                                          '()
                                          (let* ((max-m (- n 3))
                                                 (range (my-range 1 max-m)) ;; all the posibles m's
                                                 (combs_p (map (lambda(m) (flatten (combinators m acc (append vars `(,fresh-var)))) ) range))
                                                 (p-q-pairs (matching-p-to-q combs_p))) ; list of all matching i to (m - i) over all possible indexs
                                            (map (lambda(comb_p-comb_q)
                                                  (map (lambda(p_comb)(map (lambda(q_comb)`(,fresh-lambda ( ,p_comb  ,q_comb )) ))
                                                                           (car comb_p-comb_q)))
                                                        (cdr comb_p-comb_q)))
                                                    p-q-pairs)
                                            )
                                      ))]
                                  )
                            )
                          )
             )
      ;(map string->symbol
            (combinators n '() `())
           ;)
      )
    )
  )

(define gen-comb
  (lambda(n)
    (set! count 0)
    (letrec ((gen-comb (lambda (n vars)
                         (cond
                           [(= n 0) '()]
                           [(= n 1) (if
                                     (null? vars)
                                     `(,(get-new-var))
                                     vars)
                                     ]
                           [else
                            (let* ((fresh-var (get-new-var))
                                   (sub-comb-n-1 (gen-comb (- n 1) (cons fresh-var vars)))
                                   ;; the recursive call to lambda-structre
                                   (extend-to-lambda (map (lambda(sub-expr) `(lambda (,fresh-var) ,sub-expr)) sub-comb-n-1))
                                   (max-m (- n 3))
                                   (range (my-range 1 max-m)) ;; all the posibles m's
                                   (combs_p (map (lambda(m) (gen-comb m (cons fresh-var vars))) range))
                                   (p-q-pairs (matching-p-to-q combs_p))
                                   )
                              (append
                               
                               (map (lambda(p-q-pair) (map (lambda(p_expr)
                                                             (map (lambda(q_expr)
                                                                                   `(lambda (,fresh-var) (,p_expr ,q_expr))) (cdr p-q-pair)))
                                                             (car p-q-pair))) p-q-pairs)
                               extend-to-lambda
                               ))                              
                              ]))))
      (gen-comb n '()))
    )
  )

(define matching-p-to-q
  (lambda(sub-combinators)
  (letrec ((run (lambda(sub-combs acc)
                  (if (empty? sub-combs)
                      acc
                      (if (= (length sub-combs) 1)
                          (cons (cons (car sub-combs) (car sub-combs)) acc)
                          (run (reverse (cdr (reverse (cdr sub-combs)))) (cons (cons (first sub-combs) (last sub-combs)) acc))
                      ))
                  )))
    (run sub-combinators '() ))))

 (define count 0)

; aux function to generate new variable
(define (get-new-var)
  (let (
        (new-var (string-append "v" (number->string count)))
        )
    (set! count (+ 1 count)) (string->symbol new-var)) 
    )


; this function returns stream of all combinators

(define combinators-stream-old
  (letrec ((run
            (lambda (n curr-list)
              (if (empty? curr-list)
                  (let* ((n (+ n 1))
                         (combs-n (combinators n)))
                    (cons (car combs-n) (lambda () (run n (cdr combs-n)))))
                  (cons (car curr-list) (lambda () (run n (cdr curr-list))))))))
    (run 0 '())))

(define combinators-stream
  (letrec ((run
            (lambda (n curr-list)
              (if (empty? curr-list)
                  (let* ((n (+ n 1))
                         (combs-n (gen-comb n)))
                    (cons (car combs-n) (lambda () (run n (cdr combs-n)))))
                  (cons (car curr-list) (lambda () (run n (cdr curr-list))))))))
    (run 0 '())))

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
     
(define my-range (lambda (a b)
  (cond [(> a b) '()]
        [(= a b) `(,a)]
        [else (cons a (my-range (+ 1 a) b))])))


; test function
(define (test-old)
  (and (= 1 (length (combinators 1)))
       (= 1 (length (combinators 2)))
       (= 2 (length (combinators 3)))
    
       )
  
  )

(define (test)
  (and (equal? '(v0) (gen-comb 1))
       (equal? '((lambda (v0) v0)) (gen-comb 2))
       (equal? '((lambda (v0) (lambda (v1) v1))(lambda (v0) (lambda (v1) v0))) (gen-comb 3))
       ;(equal?
        ;'((lambda (v0) (lambda (v1) (lambda (v2) v2)))(lambda (v0) (lambda (v1) (lambda (v2) v1)))(lambda (v0) (lambda (v1) (lambda (v2) v0)))(lambda (v0) (v0 v0)))
        ;(gen-comb 4))
       ))

(define (test-stream)
  (and (eq? (stream+count->list combinators-stream 1) (gen-comb 1)) #t))
                                                   

;;; output examples
; ((gen-combs 3)
;    ->
;    ((lambda (v0) (lambda (v1) v1))
;      (lambda (v0) (lambda (v1) v0))))
;  ((gen-combs 4)
;    ->
;    ((lambda (v0) (lambda (v1) (lambda (v2) v2)))
;      (lambda (v0) (lambda (v1) (lambda (v2) v1)))
;      (lambda (v0) (lambda (v1) (lambda (v2) v0)))
;      (lambda (v0) (v0 v0))))
;  ((gen-combs 5)
;    ->
;    ((lambda (v0) (lambda (v1) (lambda (v2) (lambda (v3) v3)))) (lambda (v0) (lambda (v1) (lambda (v2) (lambda (v3) v2))))
;      (lambda (v0) (lambda (v1) (lambda (v2) (lambda (v3) v1))))
;      (lambda (v0) (lambda (v1) (lambda (v2) (lambda (v3) v0))))
;      (lambda (v0) (lambda (v1) (v1 v1)))
;      (lambda (v0) (lambda (v1) (v1 v0)))
;      (lambda (v0) (lambda (v1) ( v0 v1)))
;      (lambda (v0) (lambda (v1) (v0 v0)))
;      (lambda (v0) (v0 (lambda (v1) v1)))
;      (lambda (v0) (v0 (lambda (v1) v0)))
;      (lambda (v0) ((lambda (v1) v1) v0))
;      (lambda (v0) ((lambda (v1) v0) v0))
;      ((lambda (v0) v0) (lambda (v0) v0))))
;  ((gen-combs 6)
;    ->
;    ((lambda (v0)
;       (lambda (v1) (lambda (v2) (lambda (v3) (lambda (v4) v4)))))
;     (lambda (v0)
;       (lambda (v1) (lambda (v2) (lambda (v3) (lambda (v4) v3)))))
;     (lambda (v0)
;       (lambda (v1) (lambda (v2) (lambda (v3) (lambda (v4) v2)))))
;     (lambda (v0)
;       (lambda (v1) (lambda (v2) (lambda (v3) (lambda (v4) v1)))))
;     (lambda (v0)
;       (lambda (v1) (lambda (v2) (lambda (v3) (lambda (v4) v0)))))
;     (lambda (v0) (lambda (v1) (lambda (v2) (v2 v2))))
;     (lambda (v0) (lambda (v1) (lambda (v2) (v2 v1))))
;     (lambda (v0) (lambda (v1) (lambda (v2) (v2 v0))))
;     (lambda (v0) (lambda (v1) (lambda (v2) (v1 v2))))
;     (lambda (v0) (lambda (v1) (lambda (v2) (v1 v1))))
;     (lambda (v0) (lambda (v1) (lambda (v2) (v1 v0))))
;     (lambda (v0) (lambda (v1) (lambda (v2) (v0 v2))))
;     (lambda (v0) (lambda (v1) (lambda (v2) (v0 v1))))
;     (lambda (v0) (lambda (v1) (lambda (v2) (v0 v0))))
;     (lambda (v0) (lambda (v1) (v1 (lambda (v2) v2))))
;     (lambda (v0) (lambda (v1) (v1 (lambda (v2) v1))))
;     (lambda (v0) (lambda (v1) (v1 (lambda (v2) v0))))
;     (lambda (v0) (lambda (v1) (v0 (lambda (v2) v2))))
;     (lambda (v0) (lambda (v1) (v0 (lambda (v2) v1))))
;     (lambda (v0) (lambda (v1) (v0 (lambda (v2) v0))))
;     (lambda (v0) (lambda (v1) ((lambda (v2) v2) v1)))
;     (lambda (v0) (lambda (v1) ((lambda (v2) v2) v0)))
;     (lambda (v0) (lambda (v1) ((lambda (v2) v1) v1)))
;     (lambda (v0) (lambda (v1) ((lambda (v2) v1) v0)))
;     (lambda (v0) (lambda (v1) ((lambda (v2) v0) v1)))
;     (lambda (v0) (lambda (v1) ((lambda (v2) v0) v0)))
;     (lambda (v0) (v0 (lambda (v1) (lambda (v2) v2))))
;     (lambda (v0) (v0 (lambda (v1) (lambda (v2) v1))))
;     (lambda (v0) (v0 (lambda (v1) (lambda (v2) v0))))
;     (lambda (v0) (v0 (v0 v0)))
;     (lambda (v0) ((lambda (v1) v1) (lambda (v1) v1)))
;     (lambda (v0) ((lambda (v1) v1) (lambda (v1) v0)))
;     (lambda (v0) ((lambda (v1) v0) (lambda (v1) v1)))
;     (lambda (v0) ((lambda (v1) v0) (lambda (v1) v0)))
;     (lambda (v0) ((lambda (v1) (lambda (v2) v2)) v0))
;     (lambda (v0) ((lambda (v1) (lambda (v2) v1)) v0))
;     (lambda (v0) ((lambda (v1) (lambda (v2) v0)) v0))
;     (lambda (v0) ((v0 v0) v0))
;     ((lambda (v0) v0) (lambda (v0) (lambda (v1) v1)))
;     ((lambda (v0) v0) (lambda (v0) (lambda (v1) v0)))
;     ((lambda (v0) (lambda (v1) v1)) (lambda (v0) v0))
;     ((lambda (v0) (lambda (v1) v0)) (lambda (v0) v0)))))