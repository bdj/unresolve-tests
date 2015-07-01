#lang racket/base
(require rackunit
         compiler/zo-parse
         compiler/decompile
         compiler/cm
         compiler/compilation-path
         compiler/demodularizer/main
         racket/pretty)

(define (to-zo e)
  (parameterize ([current-namespace (make-base-namespace)])
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
    #;(show (to-zo name))
    (check-not-exn (lambda () (recompile (to-zo name)))
                   (format "~a" 'name))))

(define (read-module name)
  (parameterize ([read-accept-compiled #t])
    (call-with-input-file name read)))

(define (file-test name)
  (demodularize name)
  (define m (read-module (path-add-suffix name #"_merged.zo")))
  ;(show m)
  (check-not-exn (lambda () 
                   (recompile m))
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
|#
(file-test "stat-dist.rkt")
;(file-test "five-b.rkt")
;(file-test "namespace.rkt")
