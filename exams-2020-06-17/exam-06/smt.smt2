;; Random number generator seed: Just 123
;; Index: 6
(define-sort A () (Array Int Int))
(declare-fun b () Int)
(declare-fun c () Int)
(declare-fun f (Int) Int)
(declare-fun a () A)
(assert (= (- b 1) (+ c 2)))
(assert (not (= (f (+ b 0)) (+ b 1))))
(assert (= (select a (f (+ c 3))) (+ c 2)))
(assert (or (= (select a (f (+ b 0))) (+ b 2)) (= (select (store a (+ b 1) (f (+ c 2))) (f (+ c 3))) (+ c 4))))
(check-sat)
