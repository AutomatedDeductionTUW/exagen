;; Random number generator seed: Just 123
;; Index: 9
(define-sort A () (Array Int Int))
(declare-fun b () Int)
(declare-fun c () Int)
(declare-fun f (Int) Int)
(declare-fun a () A)
(assert (= (+ b 1) (- c 3)))
(assert (not (= (f (+ b 2)) (+ b 3))))
(assert (= (select a (f (- c 2))) (- c 3)))
(assert (or (= (select a (f (+ b 2))) (+ b 4)) (= (select (store a (+ b 3) (f (- c 3))) (f (- c 2))) (- c 1))))
(check-sat)
