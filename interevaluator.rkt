#lang racket

;**********TIPI
(struct const(n) #:transparent) ;n je stevilska vrednost
(struct bool(b) #:transparent) ;b je logicna vrednost
(struct interval(a b) #:transparent) ;interval v s podanimi mejami (vkljucno z njimi).a <= b.
(struct pair(e1 e2) #:transparent) ;ce sta e1 in e1 izraza v Intervaluatorju, potem je (pair e1 e2)
                                   ;par Intervaluatorjevih vrednosti v1 in v2, ki sta rezultat vrednotenja izrazov e1 in e2.
(struct nil() #:transparent) ;nicelna vrednost

;**********OPERACIJE
(struct if-then-else(b e1 e2) #:transparent) ;vejitev
(struct is-const?(e) #:transparent) ;poizvedbe tipov
(struct is-bool?(e) #:transparent)
(struct is-interval?(e) #:transparent)
(struct is-nil?(e) #:transparent)
(struct negate(e) #:transparent) ;negacija
(struct add(e1 e2) #:transparent) ;sestevanje
(struct multiply(e1 e2) #:transparent) ;mnozenje
(struct exponentiate(e) #:transparent) ;eksponentna funkcija vrednosti v izraza e
(struct left(e) #:transparent)
(struct right(e) #:transparent)
(struct greater(e1 e2) #:transparent)
(struct intersect(e1 e2) #:transparent)

;**********SPREMENLJIVKE
(struct with(vars e) #:transparent)
(struct valof(s) #:transparent)

;**********FUNKCIJE
(struct closure(env f) #:transparent)
(struct function(name farg body) #:transparent)
(struct call(e arg) #:transparent)
(struct script(name body) #:transparent)

;**********MAKROJI
(define (subtract e1 e2)
  (add e1 (negate e2)))

(define (lower e1 e2)
  (if-then-else (greater e1 e2)
                       (bool #f)
                       (if-then-else (greater e2 e1)
                                     (bool #t)
                                     (bool #f))))
(define (equal e1 e2)
  (if-then-else (greater e1 e2)
                        (bool #f)
                        (if-then-else (greater e2 e1)
                                      (bool #f)
                                      (bool #t))))
(define (encloses i1 i2)
  (if-then-else (is-nil? (intersect i1 i2))
                (bool #f)
                (if-then-else (greater (left i2) (left i1))
                              (if-then-else (greater (right i1) (right i2))
                                            (bool #t)
                                            (if-then-else (greater (right i2) (right i1))
                                                          (bool #f)
                                                          (bool #t)))
                              (if-then-else (greater (left i1) (left i2))
                                            (bool #f)
                                            (if-then-else (greater (right i1) (right i2))
                                                          (bool #t)
                                                          (if-then-else (greater (right i2) (right i1))
                                                                        (bool #f)
                                                                        (bool #t)))))))

;**********INTERVALUATOR
(define (iv exp env)
  (cond [(const? exp)
         (if(number? (const-n exp))
           exp
           (error "tip const ni stevilska vrednost"))]
        [(bool? exp)
         (if(boolean? (bool-b exp))
             exp
             (error "tip bool ni logicna vrednost"))]
        [(interval? exp)
         (if (and (number? (interval-a exp)) (number? (interval-b exp)))
             exp
             (error "tip interval ne vsebuje dve stevilski vrednosti"))]
        [(pair? exp)
         (letrec ([v1 (iv (pair-e1 exp) env)]
               [v2 (iv (pair-e2 exp) env)])
           (pair v1 v2))]
        [(nil? exp) exp]

        [(if-then-else? exp)
         (define vb(iv (if-then-else-b exp) env))
         (if (bool? vb)
             (if (bool-b vb)
                 (iv (if-then-else-e1 exp) env)
                 (iv (if-then-else-e2 exp) env))
             (error "pogoj ni logicna vrednost"))]
        [(is-const?? exp)
         (define ve(iv (is-const?-e exp) env))
         (if (const? ve)
             (bool #t)
             (bool #f))]
        [(is-bool?? exp)
         (define ve(iv (is-bool?-e exp) env))
         (if (bool? ve)
             (bool #t)
             (bool #f))]
        [(is-interval?? exp)
         (define ve(iv (is-interval?-e exp) env))
         (if (interval? ve)
             (bool #t)
             (bool #f))]
        [(is-nil?? exp)
         (define ve(iv (is-nil?-e exp) env))
         (if (nil? ve)
             (bool #t)
             (bool #f))]
        [(negate? exp)
         (define v(iv (negate-e exp) env))
         (cond [(const? v)
                (const(-(const-n v)))]
               [(bool? v)
                (if (bool-b v)
                    (bool #f)
                    (bool #t))]
               [(interval? v)
                (interval (-(interval-b v)) (-(interval-a v)))]
               [#t (error "izraza ni mogoce negirati")])]
        [(add? exp)
         (define v1(iv (add-e1 exp) env))
         (define v2(iv (add-e2 exp) env))
         (cond [(and (const? v1) (const? v2))
                (const (+ (const-n v1) (const-n v2)))]
               [(and (interval? v1) (interval? v2))
                (interval (+ (interval-a v1) (interval-a v2)) (+ (interval-b v1) (interval-b v2)))]
               [(and (const v1) (interval? v2))
                (interval (+ (const-n v1) (interval-a v2)) (+ (const-n v1) (interval-b v2)))]
               [(and (interval? v1) (const? v2))
                (interval (+ (interval-a v1) (const-n v2)) (+ (interval-b v1) (const-n v2)))]
               ;[(nil? v1)(printf"***")]
               [#t (error "izrazov ni mogoce sesteti")])]
               ;[#t exp])]
        [(multiply? exp)
         (define v1(iv (multiply-e1 exp) env))
         (define v2(iv (multiply-e2 exp) env))
         (cond [(and (const? v1) (const? v2))
                (const (* (const-n v1) (const-n v2)))]
               [(and (interval? v1) (interval? v2))
                (define ac(*(interval-a v1)(interval-a v2)))
                (define ad(*(interval-a v1)(interval-b v2)))
                (define bc(*(interval-b v1)(interval-a v2)))
                (define bd(*(interval-b v1)(interval-b v2)))
                (interval (min ac ad bc bd) (max ac ad bc bd))]
               [#t (error "izrazov ni mogoce zmnoziti")])]
        [(exponentiate? exp)
         (define v(iv (exponentiate-e exp) env))
         (cond [(const? v)
                (const (expt 2.718281828459045 (const-n v)))]
               [(interval? v)
                (interval (expt 2.718281828459045 (interval-a v)) (expt 2.718281828459045 (interval-b v)))]
               [#t (error "izraza ni mogoce eksponirati")])]
        [(left? exp)
         (define v(iv (left-e exp) env))
         (cond [(interval? v)
                (const (interval-a v))]
               [(pair? v)
                (pair-e1 v)]
               [#t (error "leva ekstrakcija ni mogoca na podanem izrazu")])]
        [(right? exp)
         (define v(iv (right-e exp) env))
         (cond [(interval? v)
                (const (interval-b v))]
               [(pair? v)
                (pair-e2 v)]
               [#t (error "leva ekstrakcija ni mogoca na podanem izrazu")])]
        [(greater? exp)
         (define v1(iv (greater-e1 exp) env))
         (define v2(iv (greater-e2 exp) env))
         (cond [(and (const? v1) (const? v2))
                (if (> (const-n v1) (const-n v2))
                    (bool #t)
                    (bool #f))]
               [(and (interval? v1) (interval? v2))
                (define intervalWidth1(- (interval-b v1) (interval-a v1)))
                (define intervalWidth2(- (interval-b v2) (interval-a v2)))
                (if (> intervalWidth1 intervalWidth2)
                    (bool #t)
                    (bool #f))]
               [#t (error "izrazov ni mogoce primerjati")])]
               ;[#t v1])]
        [(intersect? exp)
         (define v1(iv (intersect-e1 exp) env))
         (define v2(iv (intersect-e2 exp) env))
         (if (and (interval? v1) (interval? v2))
             (if (or (> (interval-a v2) (interval-b v1)) (> (interval-a v1) (interval-b v2)))
                 (nil)
                 (interval (max (interval-a v1) (interval-a v2)) (min (interval-b v1) (interval-b v2))))
             (error "izraza nista intervala"))]
        
        [(with? exp)
          (if(hash? (with-vars exp))
             (iv (with-e exp) (hash-evaluateAndAdd (with-vars exp) env))
             (error "spremenljivke niso hash tabela"))]
        [(valof? exp)
         (define ime(valof-s exp))
         (if (string? ime)
             (if (hash-has-key? env ime)
                 (hash-ref env ime)
                 (nil))
             (error "ime spremenljivke ni string"))]

        [(function? exp)
         ;(define body(iv (function-body) env))
         (cond [(string? (function-name exp)) ;imenovana funkcija
                (define envWithNameAndClosure(hash-set* env (function-name exp) (closure env exp)))
                (if (list? (function-farg exp))
                 (closure envWithNameAndClosure exp)
                 (error "argumenti funkcije farg niso podani kot seznam imen"))]
               [(false? (function-name exp)) ;anonimna funkcija
                (if (list? (function-farg exp))
                 (closure env exp)
                 (error "argumenti funkcije farg niso podani kot seznam imen"))]
               [#t (error "ime funkcije ni niz ali #f")])]

        [(script? exp)
         (if (or (string? (script-name exp)) (false? (script-name exp)))
             exp
             (error "ime funkcije ni niz ali #f"))]
        
        [(call? exp)
         ;(hash-print env)
         (define o(iv (call-e exp) env))
            (cond [(and (closure? o) (list? (call-arg exp)))
                   (define fun(closure-f o))
                   (define argHash(hash-fromLists (function-farg fun) (call-arg exp)))
                   (define oArgEnv(hash-evaluateAndAdd2 argHash (closure-env o) env))
                   (define oWithOEnv(hash-set* oArgEnv (function-name fun) o))
                   ;(printf "-----oArgEnv------\n")
                   ;(hash-print env)
                   (if (false? (function-name (closure-f o)))
                       ;anonimna funkcija
                       (iv (function-body fun) oArgEnv)
               
                       ;imenovana funkcija
                       (iv (function-body fun) oWithOEnv))]
                  ;skripta
                  [(and (script? o) (nil? (call-arg exp)))
                   (iv (script-body o) env)]

                  [(and (false? (list? (call-arg exp))) (false? (nil? (call-arg exp))))
                   (error "napacni argumenti: funkcija zahteva list argumentovn anonimna funkcija pa (nil)")]
                  ;[(nil? o)
                   ;(printf "*ovojnica je nil*")]
                  [#t (error "ne morem klicati to funkcijo (izraz ni closure ali skripta)")])]
        
        [#t (error "sintaksa izraza ni pravilna")]
  )
)

(define (hash-fromLists kl vl)
  (cond [(> (length kl) (length vl))
         (error "funkcija zahteva vec argumentov, kot jih je bilo podanih")]
        [(< (length kl) (length vl))
         (error "funkcija zahteva manj argumentov, kot jih je bilo podanih")]
        [(= (length kl) (length vl))
         (for/hash ([key kl] [val vl])
           (values key val))]))

(define (hash-evaluateAndAdd2 h o env)
  ;evalviraj vrednosti izrazov spremenljivk v env hashu (kot map)
  (define newH(for/hash ([(k v) (in-hash h)])
    (values k (iv v env))))
  ;dodaj elemente h(kot list) v o in vrni posodobljen hash
  (hash-add o (hash->list newH)))

(define (hash-evaluateAndAdd h env)
  ;evalviraj vrednosti izrazov spremenljivk v h hashu (kot map)
  (define newH(for/hash ([(k v) (in-hash h)])
    (values k (iv v env))))
  ;dodaj elemente h(kot list) v env in vrni posodobljen hash
  (hash-add env (hash->list newH)))

(define (hash-add env hl)
  (if (null? hl)
      env
      (hash-add (hash-set* env (car(car hl)) (cdr(car hl))) (cdr hl))))

(define (hash-print h)
  (printf "+++++BEGIN+++++\n")
  (if (hash-empty? h)
      (printf "hash je prazen\n")
      (for ([(key value) (in-hash h)])
        (printf "key: ~a value: ~a\n" key value)))
  (printf "+++++END+++++\n"))