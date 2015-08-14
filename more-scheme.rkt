
;;----------------------------------------------------------------------
;; more-scheme : case, do, etc. - remaining syntax
(module more-scheme '#%kernel
  (#%require 
             racket/private/define 
             '#%paramz 
             racket/private/logger
             racket/private/member
             )

  

  (define (current-parameterization)
    (extend-parameterization (continuation-mark-set-first #f parameterization-key)))
  
  

  
  
  

  
  
  
  
  

  

  

  


  

  

  
  
  

  )
