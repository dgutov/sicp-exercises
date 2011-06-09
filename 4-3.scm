;; Ex. 4.35
(define (an-integer-between low high)
  (if (<= low high)
      (amb low (an-integer-between (1+ low) high))
      (amb)))

;; Ex. 4.36
(define (all-pythagorean-triples)
  (let ((i (an-integer-starting-from 3)))
    ;; i^2 > (j + 1)^2 - j^2 = 2j + 1
    (let ((j (an-integer-between i (/ (* i i) 2))))
      (let ((k (sqrt (+ (* i i) (* j j)))))
        (require (integer? k))
        (list i j k)))))

;; Ex. 4.39
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; Ex. 4.40
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4))
        (cooper (amb 2 3 4 5)))
    (require (not (= baker cooper)))
    (let ((fletcher (amb 2 3 4)))
      (require (not (memq fletcher (list baker cooper))))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((miller (amb 1 2 3 4 5)))
        (require (> miller cooper))
        (require (not (memq miller (list baker fletcher))))
        (let ((smith (amb 1 2 3 4 5)))
          (require (not (= (abs (- smith fletcher)) 1)))
          (require (not (memq smith (list baker fletcher miller cooper))))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))))

;; Ex. 4.42
(define (liars)
  (define (xor a b)
    (not (eq? a b)))
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5))
        (girls (list betty ethel joan kitty mary)))
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= ethel 1) (= joan 2)))
    (require (xor (= joan 3) (= ethel 5)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= mary 4) (= betty 1)))
    (require (distinct? girls))
    (map cons '(betty ethel joan kitty mary) girls)))

;; Ex. 4.43
(define (yacht)
  (define father car)
  (define owner cdr)
  (let ((melissa (cons 'hood 'downing))
        (gabrielle (cons (amb 'downing 'hall) 'hood))
        (lorna (cons (amb 'downing 'hall 'parker) 'moore)))
    (require (not (eq? (father gabrielle) (father lorna))))
    (let ((rosalind (cons (amb 'downing 'hall 'parker) 'hall))
          (mary (cons 'moore 'parker)))
      (require (not (memq (father rosalind) (list (father gabrielle)
                                                  (father lorna)))))
      ;; At this point we can just name Gabrielle's father,
      ;; but we'll go ahead with the nondeterministic solution.
      (require (not (null? (filter
                            (lambda (name) (and (= 'parker (father name))
                                           (= (father gabrielle)
                                              (owner name))))
                            (list melissa rosalind)))))
      (father lorna)))) ; Downing

;; Ex. 4.44
(define (queens)
  (define (repeat fun times)
    (if (positive? times)
        (cons (fun) (repeat fun (1- times)))
        '()))
  (define (chess-pos)
    (cons (an-integer-between 1 8)
          (an-integer-between 1 8)))
  (define (attacks? a b)
    (or (= (car a) (car b))
        (= (cdr a) (cdr b))
        (= (- (car a) (car b))
           (- (cdr a) (cdr b)))))
  (define (coexist? positions)
      (let ((first (car positions))
            (rest (cdr positions)))
        (and (null? (filter (lambda (b) (attacks? first b)) rest)
                    (coexist? rest)))))
  (let ((positions (repeat chess-pos 8)))
    (require (coexist? positions))
    positions))

;; Ex. 4.50
(define (shuffle lst)
  (if (< (length lst) 2)
      lst
      (let ((r (random (length lst))))
        (cons (list-ref lst r)
              (shuffle (append (list-head lst r)
                               (list-tail lst (1+ r))))))))

(define (analyze-ramb exp)
  (let ((cprocs (shuffle (map analyze (amb-choices exp)))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

;; Ex. 4.51
;; This only works while the program flow doesn't leave the env
;; the var is defined in (which is the case in the example).
(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok fail2))
             fail))))

;; Ex. 4.52
(define (analyze-if-fail exp)
  (let ((eproc (analyze (if-predicate exp)))
        (fproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (eproc env
             succeed
             (lambda ()
               (fproc env
                      succeed
                      fail))))))

;; Ex. 4.54
(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (false? pred-value)
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))
