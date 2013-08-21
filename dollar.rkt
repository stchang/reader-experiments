#lang racket
(require syntax/readerr
         (prefix-in arith: "arith.rkt"))
 
(provide (rename-out [$-read read]
                     [$-read-syntax read-syntax]))
 
(define ($-read in)
  (parameterize ([current-readtable (make-$-readtable)])
    (read in)))
 
(define ($-read-syntax src in)
  (parameterize ([current-readtable (make-$-readtable)])
    (read-syntax src in)))
 
(define (make-$-readtable)
  (make-readtable (current-readtable)
                  #\$ 'terminating-macro read-dollar
                  #\@ 'terminating-macro read-at
                  ))

;(define (values-app x) `(call-with-values (λ () ,(second x)) ,(first x)))
(define (values-app ap) `(call-with-values (λ () ,(second ap)) ,(first ap)))
(define (list-app ap) `(apply ,(first ap) ,@(rest ap)))

(define read-at
  (case-lambda
    [(ch in) (read ch)]
    [(ch in src line col pos)
     (datum->syntax 
      #f
      (let ([ch (peek-char in)])
        (case ch
          [(#\v) (read-char in) (values-app (read in))]
          [(#\l) (read-char in) (list-app (read in))]
          [else (read in)]))
      (let-values ([(l c p) (port-next-location in)])
        (list src line col pos (and pos (- p pos)))))]))

(define read-dollar
  (case-lambda
   [(ch in)
    (check-$-after (arith:read in) in (object-name in))]
   [(ch in src line col pos)
    (check-$-after (arith:read-syntax src in) in src)]))
 
(define (check-$-after val in src)
  (regexp-match #px"^\\s*" in) ; skip whitespace
  (let ([ch (peek-char in)])
    (unless (equal? ch #\$) (bad-ending ch src in))
    (read-char in))
  val)
 
(define (bad-ending ch src in)
  (let-values ([(line col pos) (port-next-location in)])
    ((if (eof-object? ch)
         raise-read-error
         raise-read-eof-error)
     "expected a closing `$'"
     src line col pos
     (if (eof-object? ch) 0 1))))