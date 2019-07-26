#lang racket

(define lambda-string "LAMBDA")
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


(define combinators
  (lambda(n)
    (letrec ((combinators (lambda (n acc vars)
                            (cond [(zero? n) acc]
                                  [(= n 1) 
                                            (if (empty? acc)
                                                `(,(symbol->string(gensym)))
                                                (map (lambda(var)
                                                       (map (lambda(expr)(string-append expr var)) (flatten acc)))
                                                     vars))]
                                  [else
                                    (let* ((fresh-var (symbol->string(gensym)))
                                           (fresh-lambda (string-append lambda-string fresh-var "."))
                                           )
                                      (combinators (- n 1)
                                                   ;(append
                                                 ;   `(,fresh-lambda)
                                                    (if (empty? acc)
                                                        `(,fresh-lambda)
                                            ;            '()
                                                        (map (lambda(expr) (string-append expr fresh-lambda)) (flatten acc))
                                                        )
                                                 ;   )
                                                   (append vars `(,fresh-var))
                                                   )
                                      )]
                                  )
                            )
                          )
             )
      (combinators n '() `())
      )
    )
  )


(define (test)
  (and (= 1 (length (combinators 1)))
       (= 1 (length (combinators 2)))
       (= 2 (length (combinators 3)))))
                                                   

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