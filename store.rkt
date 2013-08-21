#lang reader "dollar-racket.rkt"
 
(provide cost)
 
; Cost of ‘n' $1 rackets with 7% sales
; tax and shipping-and-handling fee ‘h':
(define (cost n h)
  $n*107/100+h$)

(define (f x y) (+ x y))

@v(f (values 1 2))
@l(f (list 1 2))
@l(f 1 (list 2))