;; Random number generator seed: Just 123
;; Index: 4
(define-sort A () (Array Int Int))
(declare-fun b () Int)
(declare-fun c () Int)
(declare-fun f (Int) Int)
(declare-fun a () A)
(assert (= (- b 2) (+ c 0)))
(assert (not (= (f (- b 1)) (+ b 0))))
(assert (= (select a (f (+ c 1))) (+ c 0)))
(assert (or (= (select a (f (- b 1))) (+ b 1)) (= (select (store a (+ b 0) (f (+ c 0))) (f (+ c 1))) (+ c 2))))
(check-sat)
