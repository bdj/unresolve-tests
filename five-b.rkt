#lang racket/base
(require (for-syntax racket/base
                     racket/contract/private/application-arity-checking
                     racket/contract/private/arr-util)
         racket/contract/private/kwd-info-struct
         racket/contract/private/arity-checking
         racket/contract/private/blame
         racket/contract/private/misc
         racket/contract/private/prop
         racket/contract/private/guts
         racket/contract/private/generate
         racket/contract/private/arrow-higher-order
         racket/stxparam
         (prefix-in arrow: racket/contract/private/arrow))
(define-for-syntax (->2-handled? stx)
  (syntax-case stx (any values any/c)
    [(_ args ...)
     (syntax-parameter-value #'arrow:making-a-method)
     #f]
    [(_ any/c ... any)
     ;; should turn into a flat contract
     #f]
    [_ #t]))

(define-for-syntax popular-keys
  ;; of the 8417 contracts that get compiled during
  ;; 'raco setup' of the current tree, these are all
  ;; the ones that appear at least 50 times (the
  ;; number indicate how many times each appeared)
  `((0 0 () () #f 1)   ; 1260
    (0 0 () () #t 1)   ;   58
    (1 0 () () #f #f)  ;  116
    (1 0 () () #f 1)   ; 4140
    (1 0 () () #t 1)   ;   71
    (1 1 () () #f 1)   ;  186
    (1 2 () () #f 1)   ;  125
    (2 0 () () #f #f)  ;   99
    (2 0 () () #f 1)   ; 1345
    (2 1 () () #f 1)   ;   68
    (3 0 () () #f 1)   ;  423
    (4 0 () () #f 1)   ;  149
    (5 0 () () #f 1))) ;   74

(define-syntax (generate-popular-key-ids stx)
  (syntax-case stx ()
    [(_ popular-key-ids)
     #`(define-for-syntax popular-key-ids
         (list #,@(map (λ (x y) #`(list (quote-syntax #,x) (quote-syntax #,y)))
                       (generate-temporaries (for/list ([e (in-list popular-keys)])
                                               'popular-plus-one-key-id))
                       (generate-temporaries (for/list ([e (in-list popular-keys)])
                                               'popular-chaperone-key-id)))))]))
(generate-popular-key-ids popular-key-ids)

(define-for-syntax (build-plus-one-arity-function+chaperone-constructor
                    stx
                    regular-args
                    optional-args
                    mandatory-kwds
                    optional-kwds
                    pre pre/desc
                    rest
                    rngs
                    post post/desc)
  (define key (and (not pre) (not pre/desc)
                   (not post) (not post/desc)
                   (list (length regular-args)
                         (length optional-args)
                         (map syntax-e mandatory-kwds)
                         (map syntax-e optional-kwds)
                         (and rest #t)
                         (and rngs (if (syntax? rngs)
                                       (length (syntax->list rngs))
                                       (length rngs))))))
  (cond
    [(and key (member key popular-keys))
     =>
     (λ (l)
       (define index (- (length popular-keys) (length l)))
       (define ids (list-ref popular-key-ids index))
       (values (list-ref ids 0) (list-ref ids 1)))]
    [else
     (values (build-plus-one-arity-function/real
              regular-args
              optional-args
              mandatory-kwds
              optional-kwds
              pre pre/desc
              rest
              rngs
              post post/desc)
             (build-chaperone-constructor/real
              '() ;; this-args 
              regular-args
              optional-args
              mandatory-kwds
              optional-kwds
              pre pre/desc
              rest
              rngs
              post post/desc))]))

(define-for-syntax (build-plus-one-arity-function/real
                    regular-args
                    optional-args
                    mandatory-kwds
                    optional-kwds
                    pre pre/desc
                    rest
                    rngs
                    post post/desc)
  (with-syntax ([(regb ...) (generate-temporaries regular-args)]
                [(optb ...) (generate-temporaries optional-args)]
                [(kb ...) (generate-temporaries mandatory-kwds)]
                [(okb ...) (generate-temporaries optional-kwds)]
                [(rb ...) (generate-temporaries (or rngs '()))]
                [(arg-x ...) (generate-temporaries regular-args)]
                [(res-x ...) (generate-temporaries (or rngs '()))]
                [(kwd-arg-x ...) (generate-temporaries mandatory-kwds)])
    
    (with-syntax ([(formal-kwd-args ...)
                   (apply append (map list mandatory-kwds (syntax->list #'(kwd-arg-x ...))))]
                  [(kwd-arg-exps ...)
                   (apply append (map (λ (kwd kwd-arg-x kb) 
                                        (list kwd #`((#,kb #,kwd-arg-x) neg-party)))
                                      mandatory-kwds
                                      (syntax->list #'(kwd-arg-x ...))
                                      (syntax->list #'(kb ...))))]
                  [(letrec-bound-id) (generate-temporaries '(f))])
      
      (with-syntax ([(wrapper-args ...) #'(neg-party arg-x ... formal-kwd-args ...)]
                    [(the-call ...) #'(f ((regb arg-x) neg-party) ... kwd-arg-exps ...)]
                    [(pre-check ...)
                     (if pre 
                         (list #`(check-pre-cond  #,pre blame neg-party f))
                         (list))]
                    [(post-check ...)
                     (if post
                         (list #`(check-post-cond #,post blame neg-party f))
                         (list))]
                    [(restb) (generate-temporaries '(rest-args))])
        (define body-proc
          (cond
            [(or (and (null? optional-args)
                      (null? optional-kwds))
                 (and (null? mandatory-kwds)
                      (null? optional-kwds)))
             (define case-lambda-clauses
               (let loop ([optional-args (reverse optional-args)]
                          [ob (reverse (syntax->list #'(optb ...)))]
                          [first? #t])
                 (define no-rest-call
                   #`(the-call ... #,@(for/list ([ob (in-list (reverse ob))]
                                                 [optional-arg (in-list (reverse optional-args))])
                                        #`((#,ob #,optional-arg) neg-party))))
                 (define full-call
                   (if (and first? rest)
                       #`(apply #,@no-rest-call ((restb rest-arg) neg-party))
                       no-rest-call))
                 (define the-args #`(wrapper-args ... 
                                     #,@(reverse optional-args)
                                     #,@(if (and first? rest)
                                            #'rest-arg
                                            '())))
                 (define the-clause
                   (if rngs
                       #`[#,the-args
                          pre-check ...
                          (define-values (failed res-x ...)
                            (call-with-values
                             (λ () #,full-call)
                             (case-lambda
                               [(res-x ...)
                                (values #f res-x ...)]
                               [args
                                (values args #,@(map (λ (x) #'#f) 
                                                     (syntax->list #'(res-x ...))))])))
                          (cond
                            [failed
                             (wrong-number-of-results-blame 
                              blame neg-party f
                              failed
                              #,(length 
                                 (syntax->list
                                  #'(res-x ...))))]
                            [else
                             post-check ...
                             (values ((rb res-x) neg-party) ...)])]
                       #`[#,the-args
                          pre-check ...
                          #,full-call]))
                 (cons the-clause
                       (cond
                         [(null? optional-args) '()]
                         [else (loop (cdr optional-args)
                                     (cdr ob)
                                     #f)]))))
             
             (cond
               [(null? (cdr case-lambda-clauses))
                ;; need to specialize this case because
                ;; there might be keyword arguments here
                #`(λ #,@(car case-lambda-clauses))]
               [else
                ;; (but there won't here)
                #`(case-lambda #,@case-lambda-clauses)])]
            [else
             #`(make-checking-proc f blame
                                   '(#,@mandatory-kwds) (list kb ...)
                                   '(#,@optional-kwds) (list okb ...) 
                                   #,(length regular-args) (list regb ... optb ...) 
                                   #,(if rest #'restb #'#f))]))
        #`(λ (blame f regb ... optb ... kb ... okb ... rb ... #,@(if rest (list #'restb) '()))
            #,body-proc)))))


(define-for-syntax (->-valid-app-shapes stx)
  (syntax-case stx ()
    [(_ args ...)
     (let ()
       (define this-> (gensym 'this->))
       (define-values (regular-args kwds kwd-args let-bindings)
         10)
       (valid-app-shapes (list (- (length regular-args) 1))
                         (map syntax->datum kwds)
                         '()))]))



(define (wrong-number-of-results-blame blame neg-party val reses expected-values)
  (define length-reses (length reses))
  (raise-blame-error 
   blame #:missing-party neg-party val
   '("received ~a value~a" expected: "~a value~a")
   length-reses
   (if (= 1 length-reses) "" "s")
   expected-values
   (if (= 1 expected-values) "" "s")))
(define (build--> who 
                  raw-regular-doms raw-optional-doms 
                  mandatory-kwds mandatory-raw-kwd-doms
                  optional-kwds optional-raw-kwd-doms
                  raw-rest-ctc
                  pre-cond raw-rngs post-cond
                  plus-one-arity-function
                  chaperone-constructor)
  (define mandatory-kwd-infos 
    (for/list ([kwd (in-list mandatory-kwds)]
               [dom (in-list mandatory-raw-kwd-doms)])
      (kwd-info kwd (coerce-contract who dom) #t)))
  (define optional-kwd-infos 
    (for/list ([kwd (in-list optional-kwds)]
               [dom (in-list optional-raw-kwd-doms)])
      (kwd-info kwd (coerce-contract who dom) #f)))
  (define kwd-infos (sort (append optional-kwd-infos mandatory-kwd-infos)
                          keyword<?
                          #:key kwd-info-kwd))
  (define rest-ctc (and raw-rest-ctc (coerce-contract who raw-rest-ctc)))
  (define rngs
    (and raw-rngs
         (for/list ([rng (in-list raw-rngs)])
           (coerce-contract who rng))))
  10)

;; min-arity : nat
;; doms : (listof contract?)[len >= min-arity]
;;        includes optional arguments in list @ end
;; kwd-infos : (listof kwd-info)
;; rest : (or/c #f contract?)
;; pre? : boolean?
;; rngs : (listof contract?)
;; post? : boolean?
;; plus-one-arity-function : procedure? -- special, +1 argument wrapper that accepts neg-party
;; chaperone-constructor ; procedure? -- function that builds a projection tailored to this arrow
(define-struct base-> (min-arity doms kwd-infos rest pre? rngs post?
                                 plus-one-arity-function chaperone-constructor)
  #:property prop:custom-write custom-write-property-proc)

(define (->-generate ctc)
  (cond
    [(and (equal? (length (base->-doms ctc))
                  (base->-min-arity ctc))
          (not (base->-rest ctc)))
     ;; only handle the case with no optional args and no rest args
     (define dom-ctcs (base->-doms ctc))
     (define doms-l (length dom-ctcs))
     (λ (fuel)
       (define dom-exers '())
       (define addl-available dom-ctcs)
       (for ([c (in-list (base->-doms ctc))])
         (define-values (exer ctcs) ((contract-struct-exercise c) fuel))
         (set! dom-exers (cons exer dom-exers))
         (set! addl-available (append ctcs addl-available)))
       (define rngs-gens 
         (if (base->-rngs ctc)
             (with-definitely-available-contracts
              addl-available
              (λ ()
                (for/list ([c (in-list (base->-rngs ctc))])
                  (contract-random-generate/choose c fuel))))
             '()))
       (cond
         [(for/and ([rng-gen (in-list rngs-gens)])
            rng-gen)
          (define env (contract-random-generate-get-current-environment))
          (λ ()
            (procedure-reduce-arity
             (λ args
               ; stash the arguments for use by other generators
               (for ([ctc (in-list dom-ctcs)]
                     [arg (in-list args)])
                 (contract-random-generate-stash env ctc arg))
               ; exercise the arguments
               (for ([arg (in-list args)]
                     [dom-exer (in-list dom-exers)])
                 (dom-exer arg))
               ; compute the results 
               (define results
                 (for/list ([rng-gen (in-list rngs-gens)])
                   (rng-gen)))
               ; return the results
               (apply values results))
             doms-l))]
         [else #f]))]
    [else (λ (fuel) #f)]))

(define (->-exercise ctc)
  (define rng-ctcs (base->-rngs ctc))
  (define dom-ctcs (for/list ([doms (in-list (base->-doms ctc))]
                              [i (in-range (base->-min-arity ctc))])
                     doms))
  (define dom-kwd-infos (for/list ([dom-kwd (in-list (base->-kwd-infos ctc))]
                                   #:when (kwd-info-mandatory? dom-kwd))
                          dom-kwd))
  (define dom-kwds (map kwd-info-kwd dom-kwd-infos))
  (cond
    [(not (base->-rest ctc))
     (λ (fuel)
       (define gens 
         (for/list ([dom-ctc (in-list dom-ctcs)])
           (contract-random-generate/choose dom-ctc fuel)))
       (define kwd-gens
         (for/list ([kwd-info (in-list dom-kwd-infos)])
           (contract-random-generate/choose (kwd-info-ctc kwd-info) fuel)))
       (define env (contract-random-generate-get-current-environment))
       (cond
         [(and (andmap values gens)
               (andmap values kwd-gens))
          (values 
           (λ (f)
             (call-with-values
              (λ ()
                (keyword-apply 
                 f
                 dom-kwds
                 (for/list ([kwd-gen (in-list kwd-gens)])
                   (kwd-gen))
                 (for/list ([gen (in-list gens)])
                   (gen))))
              (λ results 
                (when rng-ctcs
                  (for ([res-ctc (in-list rng-ctcs)]
                        [result (in-list results)])
                    (contract-random-generate-stash env res-ctc result))))))
           (or rng-ctcs '()))]
         [else
          (values void '())]))]
    [else
     (λ (fuel) (values void '()))]))

(define (base->-name ctc)
  (define rngs (base->-rngs ctc))
  (define rng-sexp
    (cond
      [(not rngs) 'any]
      [(= 1 (length rngs))
       (contract-name (car rngs))]
      [else
       `(values ,@(map contract-name rngs))]))
  (cond
    [(and (andmap kwd-info-mandatory? (base->-kwd-infos ctc))
          (= (base->-min-arity ctc)
             (length (base->-doms ctc)))
          (not (base->-rest ctc))
          (not (base->-pre? ctc))
          (not (base->-post? ctc)))
     `(-> ,@(map contract-name (base->-doms ctc))
          ,@(apply
             append
             (for/list ([kwd-info (base->-kwd-infos ctc)])
               (list (kwd-info-kwd kwd-info) 
                     (contract-name (kwd-info-ctc kwd-info)))))
          ,rng-sexp)]
    [else
     (define (take l n) (reverse (list-tail (reverse l) (- (length l) n))))
     (define mandatory-args
       `(,@(map contract-name (take (base->-doms ctc) (base->-min-arity ctc)))
         ,@(apply
            append
            (for/list ([kwd-info (base->-kwd-infos ctc)]
                       #:when (kwd-info-mandatory? kwd-info))
              (list (kwd-info-kwd kwd-info) 
                    (contract-name (kwd-info-ctc kwd-info)))))))
     
     (define optional-args
       `(,@(map contract-name (list-tail (base->-doms ctc) (base->-min-arity ctc)))
         ,@(apply
            append
            (for/list ([kwd-info (base->-kwd-infos ctc)]
                       #:when (not (kwd-info-mandatory? kwd-info)))
              (list (kwd-info-kwd kwd-info) 
                    (contract-name (kwd-info-ctc kwd-info)))))))
     
     `(->* ,mandatory-args 
           ,@(if (null? optional-args)
                 '()
                 (list optional-args))
           ,@(if (base->-rest ctc)
                 (list '#:rest (contract-name (base->-rest ctc)))
                 (list))
           ,@(if (base->-pre? ctc)
                 (list '#:pre '...)
                 (list))
           ,rng-sexp
           ,@(if (base->-post? ctc)
                 (list '#:post '...)
                 (list)))]))

(define ((->-first-order ctc) x)
  (define l (base->-min-arity ctc))
  (define man-kwds (for/list ([kwd-info (base->-kwd-infos ctc)]
                              #:when (kwd-info-mandatory? kwd-info))
                     (kwd-info-kwd kwd-info)))
  (define opt-kwds (for/list ([kwd-info (base->-kwd-infos ctc)]
                              #:unless (kwd-info-mandatory? kwd-info))
                     (kwd-info-kwd kwd-info)))
  (and (procedure? x) 
       (if (base->-rest ctc)
           (arrow:procedure-accepts-and-more? x l)
           (procedure-arity-includes? x l #t))
       (arrow:keywords-match man-kwds opt-kwds x)
       #t))

(define (make-property build-X-property chaperone-or-impersonate-procedure)
  (define proj 
    (λ (->stct)
      (->-proj chaperone-or-impersonate-procedure ->stct
               (base->-min-arity ->stct)
               (base->-doms ->stct)
               (base->-kwd-infos ->stct)
               (base->-rest ->stct)
               (base->-pre? ->stct)
               (base->-rngs ->stct)
               (base->-post? ->stct)
               (base->-plus-one-arity-function ->stct)
               (base->-chaperone-constructor ->stct))))
  (parameterize ([skip-projection-wrapper? #t])
    (build-X-property
     #:name base->-name 
     #:first-order ->-first-order
     #:projection
     (λ (this)
       (define cthis (proj this))
       (λ (blame)
         (define cblame (cthis blame))
         (λ (val)
           ((cblame val) #f))))
     #:stronger
     (λ (this that) 
       (and (base->? that)
            (= (length (base->-doms that))
               (length (base->-doms this)))
            (= (base->-min-arity this) (base->-min-arity that))
            (andmap contract-stronger? (base->-doms that) (base->-doms this))
            (= (length (base->-kwd-infos this))
               (length (base->-kwd-infos that)))
            (for/and ([this-kwd-info (base->-kwd-infos this)]
                      [that-kwd-info (base->-kwd-infos that)])
              (and (equal? (kwd-info-kwd this-kwd-info)
                           (kwd-info-kwd that-kwd-info))
                   (contract-stronger? (kwd-info-ctc that-kwd-info)
                                       (kwd-info-ctc this-kwd-info))))
            (if (base->-rngs this)
                (and (base->-rngs that)
                     (andmap contract-stronger? (base->-rngs this) (base->-rngs that)))
                (not (base->-rngs that)))
            (not (base->-pre? this))
            (not (base->-pre? that))
            (not (base->-post? this))
            (not (base->-post? that))))
     #:generate ->-generate
     #:exercise ->-exercise
     #:val-first-projection proj)))

(define-struct (-> base->) ()
  #:property
  prop:chaperone-contract
  (make-property build-chaperone-contract-property chaperone-procedure))

(define-struct (impersonator-> base->) ()
  #:property
  prop:contract
  (make-property build-contract-property impersonate-procedure))
