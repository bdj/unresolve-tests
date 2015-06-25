#lang racket/base
(define (regexp-match* pattern string start)
  (let* ([string string]
         [orig-rx #f])
    (if (input-port? string)
        (let* ([discarded/leftovers printf]
               [spitout (values
                         'counter always-evt
                         (lambda (s start end flush? breakable?)
                           (let ([c (- end start)])
                             (set! discarded/leftovers
                                   (+ discarded/leftovers))
                             c)))]) 
          #f)
        string)))
