;; Ex. 5.2
(controller
 test
 (test (op <=) (reg c) (reg n))
 (branch (label fac-done))
 (assign p (op *) (reg c) (reg p))
 (assign c (op +) (reg c) (const 1))
 (branch (label test))
 fac-done)

;; Ex. 5.3
(controller
 (assign g (const 1.0))
 test
 (assign t (op square) (reg g))
 (assign t (op -) (reg t) (reg x))
 (assign t (op abs) (reg t))
 (test (op <) (reg t) (const 0.001))
 (branch (label sqrt-done))
 (assign t (op /) (reg x) (reg g))
 (assign g (op average) (reg g) (reg t))
 (goto (label test))
 sqrt-done)

;; Ex. 5.4
(controller
 (assign continue (label expt-done))
 expt-loop
 (test (op =) (reg n) (const 0))
 (branch (label base-case))
 (save continue)
 (save n)
 (assign n (op -) (reg n) (const 1))
 (assign continue (label after-expt))
 (goto (label expt-loop))
 after-expt
 (restore n)
 (restore continue)
 (assign val (op *) (reg b) (reg val))
 (goto (reg continue))
 base-case
 (assign val (const 1))
 (goto (reg continue))
 expt-done)

(controller
 (assign c (reg n))
 (assign val (const 1))
 test
 (test (op =) (reg c) (const 0))
 (branch (label expt-done))
 (assign c (op -) (reg c) (const 1))
 (assign val (op *) (reg val) (reg b))
 (goto (label test))
 expt-done)
