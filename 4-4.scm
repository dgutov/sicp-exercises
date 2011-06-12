;; Ex. 4.55
(supervisor ?x (Bitdiddle Ben))

(job ?name (accounting . ?x))

(address ?name (Slumerville . ?addr))

;; Ex. 4.56
(and (supervisor ?name (Bitdiddle Ben))
     (address ?name ?address))

(and (lisp-value < ?salary ?bens-salary)
     (salary ?name ?salary)
     (salary (Bitdiddle Ben) ?bens-salary))

(and (supervisor ?name ?superv)
     (not (job ?superv (computer . ?x))))

;; Ex. 4.57
(rule (can-replace ?person-1 ?person-2)
      (and (job ?person-1 ?job-1)
           (job ?person-2 ?job-2)
           (not (same ?person-1 ?person-2))
           (or (same ?job-1 ?job-2)
               (can-do-job ?job-1 ?job-2))))

(can-replace ?person (Fect Cy D))

(and (can-replace ?person-1 person-2)
     (salary ?person-1 ?salary-1)
     (salary ?person-2 ?salary-2)
     (lisp-value < ?salary-1 ?salary-2))

;; Ex. 4.58
(rule (big-shot ?person ?division)
      (and (job ?person (?division . ?x))
           (not (and (supervisor ?person ?super)
                     (job ?super (?division . ?xs))))))

;; Ex. 4.59
(rule (meetings ?day)
      (meeting ?x (?day ?y)))

(rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?day-and-time)
          (and (job ?person (?division . ?x))
               (meeting ?division ?day-and-time))))

(meeting-time (Hacker Alyssa P) (Wednesday ?x))

;; Ex. 4.60
;; The relation is symmetric.
;; Yes, if we have an ordering rule.
(and (lives-near ?person-1 ?person-2)
     (before ?person-1 ?person-2))

;; Ex. 4.62
(rule (last-pair (?x) (?x)))

(rule (last-pair (?h . ?t) ?x)
      (last-pair ?t ?x))

;; Ex. 4.63
(rule (grandson ?g ?s)
      (and (son ?f ?s)
           (son ?g ?f)))

(rule (son ?m ?s)
      (and (wife ?m ?w)
           (son ?w ?s)))

;; Ex. 4.66
;; The specific example should work, but the general form fails
;; when the query returns duplicate values.
;; Adding an aggregate filtering function should help:
(sum ?amount
     (and (distinct ?x
                    (wheel ?x))
          (salary ?x ?amount)))

;; Ex. 4.68
(rule (reverse (?x) (?x)))

(rule (reverse (?x . ?z) ?y)
      (append-to-form ?z (?x) ?y))

;; Ex. 4.69
(rule ((great . ?rel) ?x ?y)
      (and (last-pair ?rel (grandson))
           (ancestor (great . ?rel) ?x ?y)))

(rule (ancestor (great . ?rel) ?x ?y)
      (and (son ?x ?z)
           (ancestor ?rel ?z ?y)))

(rule (ancestor (grandson) ?x ?y)
      (grandson ?x ?y))

;; Ex. 4.74
(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter (lambda (s) (not (stream-null? s)))
                             stream)))

;; Ex. 4.75
(define (uniquely-asserted contents frames)
  (simple-stream-flatmap
   (lambda (frame)
     (let ((extensions (qeval (unique-query contents)
                              (singleton-stream frame))))
       (if (= 1 (stream-length extensions))
           extensions
           the-empty-stream)))
   frames))

;; Ex. 4.77
;; We can rearrange the conjuncts inside `and` queries before
;; running them: for each `not` or `lisp-value` query:
;; - start at the top; create a `mentioned vars` set
;; - while not at the bottom, and not all required vars are mentioned
;;   by the previous queries, swap places with the next query
;; - when swapping places with a query, add new vars from it to the set
;; If a `not` ot `lisp-value` query is inside some other type of query
;; currently implemented, its position is not important.

;; Ex. 4.78
(define (qeval query frame)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame)
        (simple-query query frame))))

(define (simple-query query-pattern frame)
  (amb (find-assertion query-pattern frame)
       (apply-rule query-pattern frame)))

;; `conjoin`: s/frame-stream/frame

(define (disjoin disjuncts frame)
  (if (empty-disjunction? disjuncts)
      frame
      (amb (qeval (first-disjunct disjuncts) frame)
           (disjoin (rest-disjuncts disjuncts) frame))))

(define (negate operands frame)
  (if-fail (begin
             (qeval (negated-query operands) frame)
             (amb))
           frame))

(define (lisp-value call frame)
  (require (execute
            (instantiate call frame
              (lambda (v f) (error "Unknown pat var -- LISP_VALUE" v)))
            frame))
  frame)

(define (find-assertion pattern frame)
  (let ((assertion (fetch-assertion pattern frame))
        (match-result (pattern-match query-pat assertion query-frame)))
    (require (not (eq? match-result 'failed)))
    match-result))

(define (apply-rule pattern frame)
  (let* ((clean-rule (rename-variables-in (fetch-rule pattern frame)))
         (unify-result (unify-match pattern
                                    (conclusion clean-rule)
                                    frame)))
    (require (not (eq? 'failed unify-result)))
    (qeval (rule-body clean-rule)
           unify-result)))

(define (stream-amb s)
  (if (stream-null? s)
      (amb)
      (amb (stream-car s)
           (stream-amb (stream-cdr s)))))

(define (get-from-stream key1 key2)
  (let ((s (get key1 key2)))
    (require s)
    (stream-amb s)))

(define (fetch-assertion pattern frame)
  (if (use-index? pattern)
      (get-from-stream (index-key-of pattern)
                       'assertion-stream)
      (stream-amb THE-ASSERTIONS)))

;; etc.
;; 
;; Differences:
;; - results will be returned in different order (depth-first search)
;; - running times should differ wildly, depending on the results count
;;   and their positions
;; - an infinite loop won't lead to a crash
