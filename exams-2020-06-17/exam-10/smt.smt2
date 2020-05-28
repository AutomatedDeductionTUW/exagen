;; Random number generator seed: Just 123
;; Index: 10
(define-sort A () (Array Int Int))
(declare-fun b () Int)
(declare-fun c () Int)
(declare-fun f (Int) Int)
(declare-fun a () A)
(assert (= (- b 4) (+ c 3)))
(assert (not (= (f (- b 3)) (- b 2))))
(assert (= (select a (f (+ c 4))) (+ c 3)))
(assert (or (= (select a (f (- b 3))) (- b 1)) (= (select (store a (- b 2) (f (+ c 3))) (f (+ c 4))) (+ c 5))))
(check-sat)
