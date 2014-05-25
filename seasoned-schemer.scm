#lang racket

(define (atom? x)
   (ormap (lambda (p) (p x)) (list number? symbol? boolean? string?)))
(define (member? a lat)
  (if (null? lat)
      #f
      (or (eq? a (car lat))
          (member? a (cdr lat)))))
(member? 4 '(2 3 4 5))
(member? 'sardines '(italian sardines))

(define (is-first? a lat)
  (if (null? lat) #f
      (or (eq? a (car lat))
          (two-in-a-row? lat))))

(define (two-in-a-row? lat)
  (if (null? lat) #f
      (is-first? (car lat) (cdr lat))))

(two-in-a-row? '(1 2 1 4 5 6 6 7))


(define (two-in-a-row-b? preceeding lat)
  (if (null? lat) #f
      (or (eq? preceeding (car lat))
          (two-in-a-row-b? (car lat) (cdr lat)))))

(define (two-in-a-row!? lat)
  (if (null? lat) #f
      (two-in-a-row-b? (car lat) (cdr lat))))

(two-in-a-row!? '(1 2 3 3 4 5 6))

(define (sum-of-prefixes tup)
  (letrec
      ((sum-of-prefixes-b (lambda (sonssf tup)
                           (if (null? tup)
                               '()
                               (cons (+ sonssf (car tup))
                                     (sum-of-prefixes-b (+ sonssf (car tup)) (cdr tup)))))))
    (cond
      ((null? tup) '())
      (else (sum-of-prefixes-b 0 tup)))))

;

(sum-of-prefixes '(2 1 9 17 0))

(define (pick n lat)
  (if (= 1 n) (car lat)
      (pick (- n 1) (cdr lat))))

(pick 4 '(1 2 3 4))



(define (scramble tup)
  (letrec
      ([S (lambda (tup rev-pre)
            (if (null? tup) tup
                (cons (pick (car tup)
                            (cons (car tup)
                                  rev-pre))
                      (S (cdr tup)
                         (cons (car tup) rev-pre)))))])
    (S tup '())))

(scramble '(1 1 1 3 4 2 1 1 9 2))


(define (multirember a lat)
  ((letrec
       ((mr (lambda (lat)
              (cond
                ((null? lat) '())
                ((eq? a (car lat)) (mr (cdr lat)))
                (else
                 (cons (car lat)
                       (mr (cdr lat))))))))
     mr)
   lat))
(multirember 'pie '(apple custard pie linze pie fish pie pie sauce))

(define (multirember-f test?)
  (lambda (a lat)
    ((letrec
         ((mr (lambda (lat)
                (cond
                  [(null? lat) '()]
                  [(test? a (car lat)) (mr (cdr lat))]
                  [else
                   (cons (car lat) (mr (cdr lat)))]))))
       mr) lat)))
(define foo (multirember-f eq?))
(foo 'pie '(apple pie pie custard lize fish sauce pie))

(define (union set1 set2)
  ((letrec
       [(U (lambda (set)
             (cond
               [(null? set) set2]
               [(member? (car set) set2) (U (cdr set))]
               [else
                (cons (car set) (U (cdr set)))])))
        (member? (lambda (a lat)
                   (cond
                     [(null? lat) #f]
                     [else (or
                             (eq? (car lat) a)
                             (member? a (cdr lat)))])))]
     U) set1))

(define (intersect set1 set2)
  (letrec
      ((I (lambda (set)
            (cond
              [(null? set) '()]
              [(member? (car set) set2)
               (cons (car set)
                     (I (cdr set)))]
              [else (I (cdr set))]))))
    (I set1)))

(intersect '(your house) '(my house))
(union '(cats and dogs) '(birds and fishs))

(define (rember a lat)
  (letrec
      [(inner-rember (lambda (atoms)
                 (cond
                   [(null? atoms) '()]
                   [(eq? a (car atoms)) (cdr atoms)]
                   [else (cons (car atoms)
                               (inner-rember (cdr atoms)))])))]
    (inner-rember lat)))

(rember 'foo '(bar baz foo sauce fish foo))

(define (rember-beyond-first a lat)
  (let/cc hop
    (letrec
        ((inner (lambda (lat)
                  (cond
                    [(null? lat) (hop '())]
                    [(eq? (car lat) a) '()]
                    [else (cons (car lat)
                                (inner (cdr lat)))]))))
      (inner lat))))

(rember-beyond-first 'desserts '(cookies chocolate mints caramel delight ginger snaps desserts chocolate mousse vanilla ice cream))

(define (rember-up-to-last a lat)
  (let/cc skip
    (letrec
        ((inner (lambda (lat)
                  (cond
                    [(null? lat) '()]
                    [(eq? a (car lat)) (skip (inner (cdr lat)))]
                    [else (cons (car lat)
                                (inner (cdr lat)))]))))
      (inner lat))))

(rember-up-to-last 'foo '(foo bar baz egg foo bar bar baz egg foo bar bar baz egg))

(define (rember1* a l)
  (letrec
      ((R (lambda (l)
            (cond
              [(null? l) '()]
              [(atom? (car l))
               (cond
                 [(eq? (car l) a) (cdr l)]
                 [else (cons (car l) (R (cdr l)))])]
              [else
               (cond
                 [(equal? (R (car l)) (car l))
                  (cons (car l) (R (cdr l)))]
                 [else (cons (R (car l)) (cdr l))])]))))
    (R l)))

(rember1* 'meat '((pasta meat) pasta (noodles meat sauce meat tomates)))

(define depth*
  (lambda (l)
    (cond
      [(null? l) 1]
      [(atom? (car l)) (depth* (cdr l))]
      [else (max
             (add1 (depth* (car l)))
             (depth* (cdr l)))])))


(depth* '((1 2 3 '(4 5 6 7 '(8 9 '(10 11 12))))))


(define leftmost
  (lambda (l)
    (let/cc skip
      (letrec
          ([lm (lambda (l)
                 (cond
                   [(null? l) l]
                   [(atom? (car l)) (skip (car l))]
                   [else (begin
                           (lm (car l))
                           (lm (cdr l)))]))])
        (lm l)))))

(leftmost '((((a) b) c) d))

(define rm
  (lambda (a l oh)
    (cond
      [(null? l) (oh 'no)]
      [(atom? (car l))
       (if (eq? (car l) a)
           (cdr l)
           (cons (car l)
                 (rm a (cdr l) oh)))]
      [else
       (let/cc success
         (let/cc oh2
           (success (cons (rm a (car l) oh2)
                          (cdr l))))
         (cons (car l)
               (rm a (cdr l) oh)))])))

(define rember1**
  (lambda (a l)
    (if (atom? (let/cc oh (rm a l oh)))
        l
        (rm a l '()))))
                     
(rember1** 'meat '((pasta meat) pasta (noodles meat sauce meat tomates)))


(define last-food 'fruit)
(define (sweet-toothL food)
  (set! last-food food)
  (cons food
        (cons 'cake
              '())))
(sweet-toothL 'chocolate)
last-food

(define (find n Ns Rs)
  (letrec
      ([A (lambda (ns rs)
            (cond
              [(null? ns) #f]
              [(= (car ns) n) (car rs)]
              [else
               (A (cdr ns) (cdr rs))]))])
    (A Ns Rs)))

(define counter (lambda() '()))
(define set-counter (lambda(x) '()))
(define consC
  (let [(N 0)]
    (set! counter (lambda () N))
    (set! set-counter (lambda (x) (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(consC 1 (consC 2 (consC 3 '())))
(counter)

(define (deep n)
  (cond
    [(zero? n) 'pizza]
    [else (consC (deep (sub1 n))
                '())]))

(define deepM
  (let ([Rs '()]
        [Ns '()])
    (lambda (n)
      (or (find n Ns Rs)
          (let ([result (deep n)])
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)))))

(deepM 10) (deepM 15)

(define length
  (let [(h (lambda (l) 0))]
    (set! h
          (lambda (l)
            (if (null? l)
                0
                (add1 (h (cdr l))))))
    h))

(length '(1 2 3 4 5 6 7 8 8 8 88 88 8 8 8 8 8 8 8 8))


(define Y
  (lambda (L)
    (let [(h (lambda (l) '()))]
      (set! h
            (L (lambda (arg) (h arg))))
      h)))

(define lengthY
  (Y (lambda (f)
       (lambda (l)
         (if (null? l)
             0
             (add1 (f (cdr l))))))))

(lengthY '())

(define depthY
  (Y (lambda (depth*)
       (lambda (s)
         (cond
           [(null? s) 1]
           [(atom? (car s)) (depth* (cdr s))]
           [else
            (max
             (add1 (depth* (car s)))
             (depth* (cdr s)))])))))

(depthY '(1 2 '(1 2 3)))

(define supercounter
  (lambda (f)
    (letrec
        [(S (lambda (n)
              (if (zero? n)
                  (f n)
                  (let ()
                    (f n)
                    (S (sub1 n))))))]
      (S 1000)
      (counter))))

(set-counter 0)
(supercounter deep)

(define rember1*C2
  (lambda (a l)
    (letrec
        [(R (lambda (l)
              (cond
                [(null? l) '()]
                [(atom? (car l))
                 (if (eq? (car l) a)
                     (cdr l)
                     (consC (car l)
                            (R (cdr l))))]
                [else
                 (let [(av (R (car l)))]
                   (if (eq? (car l) av)
                       (consC (car l)
                              (R (cdr l)))
                       (consC av (cdr l))))])))]
      (R l))))

(set-counter 0)
(rember1*C2 'noodles '((food) more (food)))
(counter)

(define bons
  (lambda (kar)
    (let [(kdr '())]
      (lambda (selector)
        (selector (lambda (x) (set! kdr x))
                  kar
                  kdr)))))

(define kons
  (lambda (a d)
    (let [(c (bons a))]
      (set-kdr c d)
      c)))

(define set-kdr
  (lambda (c x)
    ((c (lambda (s a d) s)) x)))

(define kar
  (lambda (c)
    (c (lambda (s a d) a))))

(define kdr
  (lambda (c)
    (c (lambda (s a d) d))))

(define lots
  (lambda (m)
    (if (zero? m)
        '()
        (kons 'egg
              (lots (sub1 m))))))

(kar (kdr (lots 20)))

(define add-at-end-too
  (lambda (l)
    (letrec
        ((A (lambda (ls)
              (cond
                [(null? (kdr ls))
                 (set-kdr ls (kons 'egg '()))]
                [else (A (kdr ls))]))))
      (A l)
      l)))

(define same?
  (lambda (c1 c2)
    (let ((t1 (kdr c1))
          (t2 (kdr c2)))
      (set-kdr c1 1)
      (set-kdr c2 2)
      (let ((v (= (kdr c1) (kdr c2))))
        (set-kdr c1 t1)
        (set-kdr c2 t2)
        v))))

(define dozen (lots 12))
(define bakers-dozen (add-at-end-too dozen))
(same? dozen bakers-dozen)

(define toppings null)
(define (deepB m)
  (if (zero? m)
      (let/cc jump
        (set! toppings jump)
        'pizza)
      (cons (deepB (sub1 m)) '())))

(deepB 10)
(toppings 'mozzarela)


(cons '() (toppings 'apple))
(cons (toppings 'apple) (toppings 'apple))

(define (deep&co m k)
  (if (zero? m)
      (k 'pizza)
      (deep&co (sub1 m)
               (lambda (x)
                 (k (cons x '()))))))

(deep&co 0 (lambda (x) x))

(deep&co 1 (lambda (x) x))

(deep&co 5 (lambda (x) x))

(define (deep&coB m k)
  (if (zero? m)
      (begin
        (set! toppings k)
        (k 'pizza))
      (deep&coB (sub1 m)
                (lambda (x)
                  (k (cons x '()))))))

(define leave null)
(define (walk l)
  (cond
    ((null? l) '())
    ((atom? (car l)) (leave (car l)))
    (else
     (begin
       (walk (car l))
       (walk (cdr l))))))


(define (start-it l)
  (let/cc here
    (set! leave here)
    (walk l)))

(define fill null)
(define (waddle l)
  (cond
    [(null? l) '()]
    [(atom? (car l))
     (begin
       (let/cc rest
         (set! fill rest)
         (leave (car l)))
       (waddle (cdr l)))]
    [else
     (begin
       (waddle (car l))
       (waddle (cdr l)))]))

(define (start-it2 l)
  (let/cc here
    (set! leave here)
    (waddle l)))

(define (get-first l)
  (let/cc here
    (set! leave here)
    (waddle l)
    (leave '())))

(define (get-next x)
  (let/cc here-again
    (set! leave here-again)
    (fill 'go)))

(get-first '(donut))
(get-next 'go)

(get-first '(fish (chips)))
(get-next 'go)
(get-next 'go)

(define (two-in-a-row*? l)
  (let
      [(fst (get-first l))]
    (if (atom? fst)
        (two-in-a-row-b*? fst)
        #f)))

(define (two-in-a-row-b*? a)
  (let
      [(n (get-next 'go))]
    (if (atom? n)
        (or (eq? n a)
            (two-in-a-row-b*? n))
        #f)))


(define (lookup table name)
  (table name))

(define (extend name1 value table)
  (lambda (name2)
    (if (eq? name1 name2)
        value
        (table name2))))

(define (define? e)
  (cond
    [(atom? e) #f]
    [(atom? (car e)) (eq? (car e) 'define)]
    [else #f]))

(define (*define e)
  (set! global-table
        (extend (name-of e)
                (box (the-meaning (right-side-of e)))
                global-table)))

(define (box it)
  (lambda (sel)
    (sel it (lambda (new) (set! it new)))))

(define (setbox box new)
  (box (lambda (it set) (set new))))


(define (unbox box)
  (box (lambda (it set) it)))

(define (the-meaning e)
  (meaning e lookup-in-global-table))

(define (lookup-in-global-table name)
  (lookup global-table name))

(define (meaning e table)
  ((expression-to-action e) e table))

(define (*quote e table)
  (text-of e))

(define (*identifier e table)
  (unbox (lookup table e)))

(define (*set e table)
  (setbox (lookup table (name-of e))
          (meaning (right-side-of e) table)))

(define (*lambda e table)
  (lambda (args)
    (beglis (body-of e)
            (multi-extend (formals-of e)
                          (box-all args)
                          table))))
(define (beglis es table)
  (if (null? (cdr es))
      (meaning (car es) table)
      ((lambda (val) (beglis (cdr es) table)) (meaning (car es) table))))

(define (box-all vals)
  (if (null? vals)
      '()
      (cons (box (car vals))
            (box-all (cdr vals)))))

(define (multi-extend names values table)
  (if (null? names)
      table
      (extend (car names)
              (car values)
              (multi-extend (cdr names)
                            (cdr values)
                            table))))
(define (*application e table)
  ((meaning (function-of e) table) (evlis (arguments-of e) table)))

(define (evlis args table)
  (if (null? args)
      '()
      ((lambda (val)
         (cons val (evlis (cdr args) table)))
       (meaning (car args) table))))

(define (:car args-in-a-list)
  (car (car args-in-a-list)))

(define (a-prim p)
  (lambda (args-in-a-list)
    (p (car args-in-a-list))))

(define (b-prim p)
  (lambda (args-in-a-list)
    (p (car args-in-a-list)
       (car (cdr args-in-a-list)))))

(define *const
  (lambda (:cons :car :cdr :null? eq? atom?
           :zero? :add1 :sub1 :number?)
    (lambda (e table)
      (cond
        [(number? e) e]
        [(eq? e #t) #t]
        [(eq? e #f) #f]
        [(eq? e 'cons) :cons]
        [(eq? e 'car) :car]
        [(eq? e 'cdr) :cdr]
        
        [(eq? e 'null?) :null]
        [(eq? e 'eq?) :eq?]
        [(eq? e 'atom?) :atom?]
        [(eq? e 'zero?) :zero?]
        [(eq? e 'add1) :add1]
        [(eq? e 'sub1) :sub1]
        [(eq? e 'number?) :number?])))
  (b-prim cons)
  (a-prim car)
  (a-prim cdr)
  (a-prim null?)
  (a-prim eq?)
  (a-prim atom?)
  (a-prim zero?)
  (a-prim add1)
  (a-prim sub1)
  (a-prim number?))

(define (*cond e table)
  (evcon (cond-lines-of e) table))

(define (evcon lines table)
  (cond
    [(else? (question-of (car lines)))
     (meaning (answer-of (car lines)) table)]
    [(meaning (question-of (car lines)) table)
     (meaning (answer-of (car lines) table))]
    [else (evcon (cdr lines) table)]))

(define (*letcc e table)
  (let/cc skip
    (beglis (ccbody-of e)
            (extend (name-of e)
                    (box (a-prim skip))
                    table))))

(define abort null)

(define (value e)
  (let/cc the-end
    (set! abort the-end)
    (if (define? e)
        (*define e)
        (the-meaning e))))

(define (expression-to-action e)
  (if (atom? e)
      (atom-to-action e)
      (list-to-action e)))

(define (atom-to-action e)
  (cond
    [(number? e) *const]
    [(eq? e #t) *const]
    [(eq? e #f) *const]
    [(eq? e 'cons) *const]
    [(eq? e 'car) *const]
    [(eq? e 'cdr) *const]
    [(eq? e 'null?) *const]
    [(eq? e 'eq?) *const]
    [(eq? e 'atom?) *const]
    [(eq? e 'zero?) *const]
    [(eq? e 'add1) *const]
    [(eq? e 'sub1) *const]
    [(eq? e 'number?) *const]
    [else *identifier]))

(define (the-empty-table name)
  (abort (cons 'no-answer
               (cons name '()))))

(define (list-to-action e)
  (if (atom? (car e)
             (cond
               [(eq? (car e) 'quote) *quote]
               [(eq? (car e) 'lambda) *lambda]
               [(eq? (car e) 'letcc) *letcc]
               [(eq? (car e) 'set!) *set]
               [(eq? (car e) 'cond) *cond]
               [else *application])
             *application)))

(define (text-of x)
  (cadr x))

(define (formals-of x)
  (cadr x))

(define (body-of x)
  (cddr x))

(define (ccbody-of x)
  (cddr x))

(define (name-of x)
  (cadr x))

(define (right-side-of x)
  (if (null? (cddr x))
      0
      (caddr x)))

(define (cond-lines-of x)
  (cdr x))

(define (else? x)
  (and (atom? x)
       (eq? x 'else)))

(define (question-of x)
  (car x))

(define (answer-of x)
  (cadr x))

(define (function-of x)
  (car x))

(define (arguments-of x)
  (cdr x))