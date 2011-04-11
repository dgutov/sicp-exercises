(define-macro (cons-stream a b)
  `(cons ,a (delay ,b)))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())

(define stream-null? null?)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define integers (cons-stream 1 (add-streams ones integers)))

;; Ex. 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

;; Ex. 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams factorials
                                               (stream-cdr integers))))
;; Ex. 3.55
(define (partial-sums s)
  (define sums
    (cons-stream (stream-car s)
                 (add-streams sums (stream-cdr s))))
  sums)

;; Ex. 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))

;; Ex. 3.59
(define (integrate-series s)
  (stream-map / s integers))

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;; Ex. 3.60
(define (mul-series s1 s2)
  (let ((car1 (stream-car s1))
        (car2 (stream-car s2)))
    (cons-stream (* car1 car2)
                 (let ((cdr1 (stream-cdr s1))
                       (cdr2 (stream-cdr s2)))
                   (stream-map +
                               (scale-stream cdr2 car1)
                               (scale-stream cdr1 car2)
                               (cons-stream 0
                                            (mul-series cdr2 cdr1)))))))

;; (define one (add-streams (mul-series cosine-series cosine-series)
;;                          (mul-series sine-series sine-series)))

;; Ex. 3.61
(define invert-unit-series
  (cons-stream 1
               (scale-stream (mul-series ones
                                         invert-unit-series)
                             -1)))

;; Ex. 3.62
(define (div-series num den)
  (define ratio-series
    (scale-stream (stream-map - num
                              (cons-stream 0
                                           (mul-series (stream-cdr den)
                                                       ratio-series)))
                  (stream-car den)))
  (if (zero? (stream-car den))
      (error "The constant term of denominator is 0!")
      ratio-series))

;; (div-series sine-series cosine-series)
;; -> (0 1 0 1/3 0 2/15 0 17/315 ...)

;; Ex. 3.64
(define (stream-limit stream tolerance)
  (let* ((tail (stream-cdr stream))
         (next (stream-car tail)))
    (if (< (abs (- (stream-car stream) next))
           tolerance)
        next
        (stream-limit tail tolerance))))

;; Ex. 3.67
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (all-pairs (stream-cdr s) (stream-cdr t))
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
                 (stream-cdr t))
     (stream-map (lambda (x) (list x (stream-car t)))
                 (stream-cdr s))))))

;; Ex. 3.69
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (pairs t u))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define pythagorean-triples
  (stream-filter
   (lambda (triple)
     (let ((i (car triple))
           (j (cadr triple))
           (k (caddr triple)))
       (= (+ (* i i) (* j j)) (* k k))))
   (triples integers integers integers)))

;; Ex. 3.70
(define (merge-weighted s t weight)
  (cond ((stream-null? s) t)
        ((stream-null? t) s)
        (else
         (let ((first (stream-car s))
               (second (stream-car t)))
           (if (> (weight first) (weight second))
               (cons-stream second
                            (merge-weighted s
                                            (stream-cdr t)
                                            weight))
               (cons-stream first
                            (merge-weighted (stream-cdr s)
                                            t
                                            weight)))))))

;; weight function must be monotone,
;; and each stream should be already sorted
(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define pairs-ordered-by-sum
  (weighted-pairs integers integers
                  (lambda (pair) (+ (car pair) (cadr pair)))))

;; Ex. 3.71
(define (cubes-weight pair)
  (let ((first (car pair))
        (second (cadr pair)))
    (+ (* first first first)
       (* second second second))))

(define pairs-ordered-by-cubes
  (weighted-pairs integers integers cubes-weight))

(define (ramanujan pairs previous last-found)
  (let* ((pair (stream-car pairs))
         (current (cubes-weight pair)))
    (if (and (= current previous) (not (= current last-found)))
        (cons-stream current
                     (ramanujan (stream-cdr pairs) current current))
        (ramanujan (stream-cdr pairs) current last-found))))

(define ramanujan-numbers
  (ramanujan pairs-ordered-by-cubes 0 0))
;; (1729 4104 13832 20683 32832 39312 ...)

;; Ex. 3.73
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-streams
     (scale-stream i R)
     (integral (scale-stream i (/ 1 C))
               v0
               dt))))

;; Ex. 3.74
(define zero-crossings
  (stream-map sign-change-detector sense-data (stream-cdr sense-data)))

;; Ex. 3.75
(define (make-zero-crossings input-stream last-value last-avg)
  (let* ((value (stream-cdr input-stream))
         (avpt (/ (+ value last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avg)
                 (make-zero-crossings (stream-cdr input-stream)
                                      value
                                      avpt))))

;; Ex. 3.76
(define (smooth input)
  (stream-map (lambda (a b) (/ (+ a b) 2))
              input
              (stream-cdr input)))

(define (zero-crossings-1 sense-data)
  (let ((smooth-data (smooth sense-data)))
    (stream-map sign-change-detector
                smooth-data
                (stream-cdr smooth-data))))

;; Ex. 3.77
(define (solve f y0 dt)
  (define (y) (integral dy y0 dt))
  (define dy (delay (stream-map f (y))))
  (y))

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

;; Ex. 3.78
(define (solve-2nd a b dt y0 dy0)
  (define y (delay (integral dy y0 dt)))
  (define dy (delay (integral ddy dy0 dt)))
  (define ddy (delay (stream-map (lambda (dx x)
                                   (+ (* a dx)
                                      (* b x)))
                                 (force dy) (force y))))
  (force y))

;; Ex. 3.79
(define (solve-2nd f y0 dy0 dt)
  (define y (delay (integral dy y0 dt)))
  (define dy (delay (integral ddy dy0 dt)))
  (define ddy (delay (stream-map f (force dy) (force y))))
  (force y))
;; (stream-ref (solve-2nd (lambda (dx x) (* -1 x))
;;                        1 0
;;                        (* 0.0001 1.57079633))
;;             10000) ; cos(pi/2)
;; 9.71537478339723e-9

;; Ex. 3.80
(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define iL (delay (integral diL iL0 dt)))
    (define vC (delay (integral dvC vC0 dt)))
    (define dvC (delay (scale-stream (force iL) (/ (- C)))))
    (define diL (delay (stream-map (lambda (v i)
                                     (- (/ v L)
                                        (/ (* R i) L)))
                                   (force vC) (force iL))))
    (cons (force vC) (force iL))))
