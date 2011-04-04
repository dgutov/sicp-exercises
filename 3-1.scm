;; Ex. 3.1
(define (make-accumulator i)
  (lambda (n)
    (set! i (+ i n))
    i))

;; Ex. 3.2
(define (make-monitored f)
  (let ((cnt 0))
    (lambda (n)
      (cond ((eq? n 'how-many-calls?)
             cnt)
            ((eq? n 'reset-count)
             (set! cnt 0))
            (else
             (set! cnt (1+ cnt))
             (f n))))))

;; Ex. 3.4
(define (make-account balance secret-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password m)
    (define wrong-pass-counter 0)
    (if (eq? password secret-password)
        (begin (set! wrong-pass-counter 0)
               (cond ((eq? m 'withdraw) withdraw)
                     ((eq? m 'deposit) deposit)
                     (else (error "Unknown request -- MAKE-ACCOUNT"
                                  m))))
        (lambda (n)
          (set! wrong-pass-counter (1+ wrong-pass-counter))
          (if (= wrong-pass-counter 7)
              (call-the-cops))
          "Incorrect password")))
  dispatch)

;; Ex. 3.8
(define saved 0)

;; This one is flaky
;; (+ (f 0) (f 1)) -> 0
;; (+ (f 1) (f 0)) -> 1
;; (+ (f 1) (f 0)) -> 0
(define (f n)
  (let ((prev saved))
    (set! saved n)
    (if (= prev 0)
        0
        n)))
