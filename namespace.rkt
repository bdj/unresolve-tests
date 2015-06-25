(module namespace racket/private/pre-base

  ;; ----------------------------------------

  (define orig-varref (#%variable-reference orig-varref))

  (define (make-base-empty-namespace)
  orig-varref)


)