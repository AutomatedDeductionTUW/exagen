;; Random number generator seed: Just 123
;; Index: 3
(define-sort A () (Array Int Int))
(declare-fun b () Int)
(declare-fun c () Int)
(declare-fun f (Int) Int)
(declare-fun a () A)
(assert (= (+ b 0) (+ c 2)))
(assert (not (= (f (+ b 1)) (+ b 2))))
(assert (= (select a (f (+ c 3))) (+ c 2)))
(assert (or (= (select a (f (+ b 1))) (+ b 3)) (= (select (store a (+ b 2) (f (+ c 2))) (f (+ c 3))) (+ c 4))))
(check-sat)
