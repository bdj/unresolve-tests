(module ill-formed racket/private/pre-base

  (define-struct namespace-anchor (var))
  
  (define (namespace-anchor->empty-namespace ra)
    (unless (namespace-anchor? ra)
      (raise-argument-error 'anchor->empty-namespace
                            "namespace-anchor?"
                            ra))
    (variable-reference->empty-namespace (namespace-anchor-var ra)))
)
