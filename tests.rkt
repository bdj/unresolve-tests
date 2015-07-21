#lang racket/base
(require compiler/zo-parse
         compiler/decompile
         racket/pretty)

(define (to-zo e)
  (parameterize ([current-namespace (make-base-namespace)])
    (compile e)))

(define (read-module p)
  (parameterize ([read-accept-compiled #t])
    (call-with-input-file p read)))

(define (show e #:decompile [d #t])
  (define o (open-output-bytes))
  (write e o)
  (pretty-print
   (let ([the-zo (zo-parse (open-input-bytes (get-output-bytes o)))])
     (if d (decompile the-zo) the-zo))))

(define (write-recomp e file)
  (write (recompile e) (open-output-file file #:exists 'replace)))
(define x-letrec
  '(lambda (x)
    (letrec ([y (lambda (y) (y y))]
             [z (lambda (x) (z x))])
      (list y z))))

(define x-letrec2
  '(lambda (x)
    (letrec ([y (lambda (x) (y x))])
      (x) y)))

(define x-letrec1
  '(lambda (x)
    (letrec ([z (lambda (u) (y z w u x))]
             [y (lambda (u) (y z w u x))]
             [w (random)])
      (x) (z) (w) y)))

(define x-letrec3
  '(lambda (x)
    (letrec ([z (lambda (x) (y z w x))]
             [y (lambda (x) (y z w x))]
             [w (z y)])
      
      (x) (z) (w) y)))

(define x-mix
  '(lambda (x)
    (letrec ([y (lambda (y) (z y))]
             [q (random)]
             [z (lambda (x) (z x))])
      (list y g z))))

(define x-mutable
  '(lambda (x)
    (let ([y 0]
          [z 1])
      (set! y z)
      (set! z y))))

(define x-closure
  '(lambda (x)
    (let ([z (random)])
      (lambda (y) 
        (let ([w (random)])
          (x y z (lambda () w)))))))


(define x-toplevel
  '(lambda (z) (z x)))
  
(define x-problem2
  '(let* ([x 'hello]
          [y (lambda (z)
               (set! x z))])
    (x y)))

(define x-problem3
  '(let* ([x 10]
          [y (x)]
          [z (begin (x) (y))]
          [w (begin (x) (y) (z))])
    (set! x 50)
    (set! y 60)
    (set! z 70)
    w))

(define x-problem
  '(let* ([x (random)]
          [y (x)]
          [z (begin (x) (y))]
          [w (begin (x) (y) (z))])
    (x) (y) (z) (w)))

(define x-standard
  '(lambda (u) (let ([x (read)]
               [y (read)]
               [z (read)]
               [w (read)])
           (x) (y) (z) (w) u)))
      
(define x-standard2
  '(lambda (u) (let* ([x 50]
                 [y 60]
                 [z 70]
                 [w 80])
            (set! x 10)
            (set! y 20)
            (set! z 30)
            (set! w 40)
            (x) (y) (z) (w) u)))

(define x-nest
  '(printf "~a" (let* ([x 50]
                       [y 60])
                  (set! x 10)
                  (let ([z 40])
                    (set! y 20)
                    (set! z 30)
                    (x) (y) z))))

(define x-count
  '(lambda ()
    (define-values (a b c d e)
      (make-struct-type 'foo #f 0 0))
    (values a b c d e)))

(define x-count2
  '(lambda ()
    (let*-values ([(a b c d e) (make-struct-type 'foo #f 0 0)]
                  [(f g h i j) (make-struct-type 'foo2 #f 0 0)])
      (values a b c d e f g h i j))))

(define x-seq
  `(lambda ()
    (let*-values ([(a) (random)]
                  [(b c d) (random)]
                  [(e) (random)])
      (a) (c) (e))))

(define (show* t)
  (define the-zo (to-zo t))
  (show the-zo #:decompile #t)
  (show (recompile the-zo)))

;(show (recompile (to-zo x-mix)))
;(show (recompile (to-zo x-mutable)))
;(define x-closure-zo (to-zo x-closure))
;(show x-closure-zo)
;(show (recompile x-closure-zo) #:decompile #f)
;(show (recompile (to-zo x-toplevel)))
(printf "\n\n\n\nHERE WE GO!!!\n\n\n")
;(show* x-letrec)
;(show* x-letrec1)
;(define x-problem-zo (to-zo x-problem))
;(show x-problem-zo #:decompile #f)
;(show (recompile x-problem-zo))
;(show (read-module "compiled/base_rkt.zo"))
;(show (read-module "compiled/base_rkt.zo") #:decompile #f)
;(show* (read-module "five-b_rkt_merged.zo"))
;(show* (read-module "compiled/list_rkt.zo"))
;(show* (read-module "compiled/base_rkt.zo" #;(collection-file-path "more-scheme_rkt.zo" "racket" "private" "compiled")))
;(write-recomp (read-module "compiled/list_rkt.zo") "list-done.zo")
;(write-recomp (read-module "compiled/five_rkt.zo") "five-done.zo")
;(show (recompile (read-module "compiled/five_rkt.zo")) #:decompile #f)
;(show (read-module "compiled/five_rkt.zo") #:decompile #f)

;(write-recomp (read-module "five-b_rkt_merged.zo") "five-b-done.zo")
;(show (recompile (read-module "five-b_rkt_merged.zo")) #:decompile #f)
;(show (read-module "five-b_rkt_merged.zo") #:decompile #f)
(write-recomp (read-module "five_rkt_merged.zo") "five-done.zo")
;(show (recompile (read-module "five_rkt_merged.zo")) #:decompile #f)
;(show (read-module "five_rkt_merged.zo") #:decompile #f)
;(write-recomp (read-module "base-2_rkt_merged.zo") "base-2-done.zo")
;(write-recomp (read-module "list_rkt_merged.zo") "list-done.zo")
;(write-recomp (read-module "namespace_rkt_merged.zo") "namespace-done.zo")
;(write-recomp (read-module "compiled/namespace_rkt.zo") "namespace-done.zo")

;(show (read-module "compiled/namespace_rkt.zo") #:decompile #f) 
;(show (recompile (read-module "compiled/namespace_rkt.zo")) #:decompile #f) 
;(show* (read-module "compiled/namespace_rkt.zo")
;(show* x-count2)
;(show* x-seq)
;(show (read-module "a_rkt_merged.zo") #:decompile #f)
;(show (read-module "compiled/a_rkt.zo") #:decompile #f)
;(show (read-module "compiled/b_rkt.zo") #:decompile #f)

;(write-recomp (read-module "fft_rkt_merged.zo") "fft-done.zo")

;(write-recomp (read-module "five_rkt_zo_merged.zo") "five_rkt_zo-done.zo")
