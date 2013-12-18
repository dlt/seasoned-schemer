#lang racket

(define (atom? x)
   (ormap (lambda (p) (p x)) (list number? symbol? boolean? string?)))
(define (member? a lat)
  (if (null? lat) #f
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

