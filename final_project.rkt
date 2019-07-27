#lang racket

(define lambda-string "(lambda (")
;;; this function get natural number n and return list of all the combintors in the length l (as string)
;(define combinators
 ; (lambda(n)
  ;  (letrec ((combinators (lambda (n acc vars)
                                        
   ; (cond [(zero? n) acc]
    ;      [(= n 1)
     ;      (let ((var-str (symbol->string(gensym)))) `(,(string-append lambda-string var-str "." var-str)))           
      ;     ]
       ;   [else
        ;   (let ((fresh-var (symbol->string(gensym))))
         ;    (append (combinators (- n 1)) (map (lambda(str)(string-append str lambda-string fresh-var "." fresh-var))(combinators (- n 1))))) ]

          ;))))
     ; (combinators n '() ))
    ;))

(define close-brackets (lambda (len)
                         (make-string len #\))))

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
                                                           (map (lambda(expr)(string-append expr var (close-brackets (length vars)))) (flatten acc)))
                                                         vars)))]
                                  [else
                                    (let* ((fresh-var (get-new-var))
                                           (fresh-lambda (string-append lambda-string fresh-var ") "))
                                           )
                                     (append
                                      ; lambda combinator
                                      (combinators (- n 1)
                                                    (if (empty? acc)
                                                        `(,fresh-lambda)
                                                        (map (lambda(expr) (string-append expr fresh-lambda)) (flatten acc))
                                                        )
                                                 ;   )
                                                   (append vars `(,fresh-var))
                                                   )
                                      
                                      ; application combinators
                                      (if (< n 4)
                                          '()
                                          (let* ((combinators_p (combinators (- n 3) acc (append vars `(,fresh-var))))
                                                 (combinators_q (combinators (- n 3) acc (append vars `(,fresh-var)))))
                                            (map (lambda(p_comb)(map (lambda(q_comb)(string-append  fresh-lambda "(" p_comb " " q_comb ")" )) combinators_q)) combinators_p)
                                            )
                                      )))]
                                  )
                            )
                          )
             )
      (map string->symbol (flatten (combinators n '() `())))
      )
    )
  )

(define count 0)
(define (get-new-var)
  (let (
        (new-var (string-append "v" (number->string count)))
        )
    (set! count (+ 1 count)) new-var) 
    )


; this function returns stream of all combinators
(define combinators-stream '())

(define (test)
  (and (= 1 (length (combinators 1)))
       (= 1 (length (combinators 2)))
       (= 2 (length (combinators 3)))
    
       )
  
  )
                                                   

(define num-of-comb
  (lambda(n)
    (if (< n 2)
        n
        ;; for each 1<= i <= n-1 num-of-comb(i) + num-of-comb(n-i) + num-of-comb(n)
        (let*
            (
             (range (my-range 1 (- n 1) 1))
             (results (map (lambda(n) (num-of-comb n)) range))
             )
          (+ (foldl + 0 results) (* 2 (num-of-comb n)))
          )
        )
    )
  )
         
         
(define my-range (lambda (a b step)
  (if (>= a b)
      '()
      (cons a (my-range (+ step a) b step)))))

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
;      (lambda (v0) (lambda (v1) (v0 v1)))
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