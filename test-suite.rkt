#lang racket/base
(require rackunit
         compiler/zo-parse
         compiler/decompile
         compiler/cm
         compiler/compilation-path
         compiler/demodularizer/main
         racket/pretty)

(define (to-zo e)
  (parameterize ([current-namespace (make-base-namespace)]
                 [compile-context-preservation-enabled #f])
    (compile e)))

(define (show e #:decompile [d #t])
  (define o (open-output-bytes))
  (write e o)
  (pretty-print
   (let ([the-zo (zo-parse (open-input-bytes (get-output-bytes o)))])
     (if d (decompile the-zo) the-zo))))

(define-syntax-rule (define-test name code)
  (begin
    (define name
      'code)
    (show (to-zo name) #:decompile #t)
    (let ([recomp 
            (parameterize ([current-namespace (make-base-namespace)]
                           [compile-context-preservation-enabled #f])
              (compiled-expression-recompile (to-zo name)))])
      (show recomp #:decompile #t)  
      (write recomp (open-output-file (path-add-suffix (format "~a" 'name) #"_recomp.zo") #:exists 'replace)))))

(define (read-module name)
  (parameterize ([read-accept-compiled #t])
    (call-with-input-file name read)))

(define (file-test name)
  (demodularize name)
  (define m (read-module (path-add-suffix name #"_merged.zo")))
  ;(show m)
  (check-not-exn (lambda () 
                   (let ([recomp (compiled-expression-recompile m)])
                      (write recomp (open-output-file (path-add-suffix name #"_recomp.zo") #:exists 'replace))
                      (show recomp #:decompile #t)
                      (read-module (path-add-suffix name #"_recomp.zo")))
                   )
                 name))
#|
(define-test x-letrec
  (lambda (x)
    (letrec ([y (lambda (y) (y y))]
             [z (lambda (x) (z x))])
      (list y z))))

(define-test x-letrec2
  (lambda (x)
    (letrec ([y (lambda (x) (y x))])
      (x) y)))

(define-test x-letrec1
  (lambda (x)
    (letrec ([z (lambda (u) (y z w u x))]
             [y (lambda (u) (y z w u x))]
             [w (random)])
      (x) (z) (w) y)))

(define-test x-letrec3
  (lambda (x)
    (letrec ([z (lambda (x) (y z w x))]
             [y (lambda (x) (y z w x))]
             [w (z y)])
      
      (x) (z) (w) y)))

(define-test x-mix
  (lambda (x)
    (letrec ([y (lambda (y) (z y))]
             [q (random)]
             [z (lambda (x) (z x))])
      (list y g z))))

(define-test x-mutable
  (lambda (x)
    (let ([y 0]
          [z 1])
      (set! y z)
      (set! z y))))

(define-test x-closure
  (lambda (x)
    (let ([z (random)])
      (lambda (y) 
        (let ([w (random)])
          (x y z (lambda () w)))))))


(define-test x-toplevel
  (lambda (z) (z x)))
  
(define-test x-problem2
  (let* ([x 'hello]
          [y (lambda (z)
               (set! x z))])
    (x y)))

(define-test x-problem3
  (let* ([x 10]
          [y (x)]
          [z (begin (x) (y))]
          [w (begin (x) (y) (z))])
    (set! x 50)
    (set! y 60)
    (set! z 70)
    w))

(define-test x-problem
  (let* ([x (random)]
          [y (x)]
          [z (begin (x) (y))]
          [w (begin (x) (y) (z))])
    (x) (y) (z) (w)))

(define-test x-standard
  (lambda (u) (let ([x (read)]
               [y (read)]
               [z (read)]
               [w (read)])
           (x) (y) (z) (w) u)))
      
(define-test x-standard2
  (lambda (u) (let* ([x 50]
                 [y 60]
                 [z 70]
                 [w 80])
            (set! x 10)
            (set! y 20)
            (set! z 30)
            (set! w 40)
            (x) (y) (z) (w) u)))

(define-test x-nest
  (printf "~a" (let* ([x 50]
                       [y 60])
                  (set! x 10)
                  (let ([z 40])
                    (set! y 20)
                    (set! z 30)
                    (x) (y) z))))

(define-test x-count
  (lambda ()
    (define-values (a b c d e)
      (make-struct-type 'foo #f 0 0))
    (values a b c d e)))

(define-test x-count2
  (lambda ()
    (let*-values ([(a b c d e) (make-struct-type 'foo #f 0 0)]
                  [(f g h i j) (make-struct-type 'foo2 #f 0 0)])
      (values a b c d e f g h i j))))

(define-test x-seq
  (lambda ()
    (let*-values ([(a) (random)]
                  [(b c d) (random)]
                  [(e) (random)])
      (a) (c) (e))))

(define-test stat-dist
  (module test racket/base
  (lambda ()
      (define (loop)
        (letrec [(c (case-lambda
                      [() c] ; return itself on zero arguments
                      [d (loop)]))]
          c))
      (loop))))
(define-test segfault
  (module test racket/base
    (define (make-curry)
  ;; The real code is here
    (let* ([arity (random)]
           [max-arity (apply max arity)])
      (define (loop)
        max-arity
           (letrec [(curried
                     (case-lambda
                       [() curried] ; return itself on zero arguments
                       [more (loop)]))]
             curried))
      (loop)))))

(define-test odd-even
  (module test racket/base
    (lambda (m)
      (define (even n) 
        (if (= 0 n) '(1 2 3) (odd (- n 1))))
      (define (odd n)
        (if (= 0 n) '(4 5 6) (even (- n 1))))
      (even m))))
(define-test top-access
  (module sort '#%kernel
    (#%require 
     racket/private/small-scheme 
     racket/private/define) 
    (#%provide sort)
    (define sort
      (let ()
        (define sort-internals (make-hasheq))
        (let ([proc 
                (lambda (vec n) 
                  (let* ([n/2- (+ n 1)]
                         [n/2+ (+ n n/2-)])
                    (vector-set! vec n/2+ (vector-ref vec 0))
                    (let iloop ([i 1])
                      (let ([ref-i (vector-ref vec (+ 0 i))])
                        (let jloop ([j (+ n/2+ i)])
                          (vector-set! vec j (vector-ref vec (+ j 1)))
                          (jloop (+ j 1))
                          (vector-set! vec j ref-i) 
                          (iloop (+ i 1))
                          )))
                    5555))])
          (hash-set! sort-internals < proc))
        55555))))
(define-test ill-formed

  (module ill-formed racket/private/pre-base

    (define-struct namespace-anchor (var))

    (define (namespace-anchor->empty-namespace ra)
      (unless (namespace-anchor? ra)
        (raise-argument-error 'anchor->empty-namespace
                            "namespace-anchor?"
                            ra))
      (variable-reference->empty-namespace (namespace-anchor-var ra)))))
|#
#;(define-test varref-test
  (module varref racket/base
    (define orig-insp (variable-reference->module-declaration-inspector
                     (#%variable-reference)))))
#;(define-test nboyer-test
  (module nboyer-test racket/base
    (define (setup-boyer) #t) ; assigned below

    (let ()

      (define (translate-term term)
        (cons (symbol->symbol-record (car term))
              (translate-term (cdr term))))
  
      (define (symbol->symbol-record sym)
        (let ((x (assq sym *symbol-records-alist*)))
          (set! *symbol-records-alist* (random))
          x))

      (define *symbol-records-alist* '())

      (define false-term '*)  ; becomes (translate-term '(f))

      (set! setup-boyer
        (lambda ()
          (set! false-term (translate-term '(f))))))))

#;(define-test set-loop
  (module set-loop racket/base
    (let ([x 0]
          [z 2])
      (let loop ([y 1]
                 [w 3])
        (set! x (+ y x))
        (set! z (+ x z))
        (set! w (add1 w))
        (if (< x 100000)
          (loop (add1 y) x)
          (values x y w z))))))

#;(define-test set-loop-simple
  (module set-loop-simple racket/base
    (let ([x 0])
      (let loop ()
        (set! x (add1 x))
        (loop)))))
#;(define-test set-loop-again
  (module set-loop-again racket/base
    (let ([x 0]
          [y 1])
      (let loop ()
        (set! x (add1 x))
        (set! y (+ x y))
        loop))))

#;(define-test letrec-case-lam
  (module letrec-case-lam racket/base
   (define make-pretty-print
     (lambda (name display? as-qq?)
       (letrec ([pretty-print
		 (case-lambda 
		  [(obj port qq-depth)
		     (void)]
                  [(obj port) (pretty-print obj port 0)]
		  [(obj) (pretty-print obj (current-output-port))])])
	 pretty-print)))

   (define pretty-print (make-pretty-print 'pretty-print #f #t))))

#;(define-test struct-props
  (module struct-props racket/base
    
    (define-struct make-contract [stronger generate exercise list-contract? ]
                   #:omit-define-syntaxes
                   #:property random
                   (random
                     1 2 3
                     #:stronger (lambda (a b) ((make-contract-stronger a) a b))
                     #:generate (lambda (c) (make-contract-generate c))
                     #:exercise (lambda (c) (make-contract-exercise c))
                     #:list-contract? (λ (c) (make-contract-list-contract? c))))))

    
;(file-test "stat-dist.rkt")
;(file-test "five.rkt")
;(file-test "five-b.rkt")
;(file-test "segfault.rkt")
;(file-test "namespace.rkt")
;(read-module "top-access_recomp.zo")
;(file-test "ill-formed.rkt")
;(file-test "varref-anon.rkt")
;(file-test "toplevel.rkt")
;(file-test "collatz.rkt")
;(file-test "nboyer2.rkt")
;(file-test "nqueens.rkt")
;(file-test "scheme.rkt")
;(file-test "regress.rkt")
;(file-test "../demod-benchmarks/a.rkt")
(file-test "more-scheme.rkt")
