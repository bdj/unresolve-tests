#lang racket/base

(require racket/private/norm-arity)

(define (make-curry)
  ;; The real code is here
    (let* ([arity (procedure-arity)]
           [max-arity (apply max arity)])
      (define (loop)
        max-arity
           (letrec [(curried
                     (case-lambda
                       [() curried] ; return itself on zero arguments
                       [more (loop)]))]
             curried))
      (loop)))

