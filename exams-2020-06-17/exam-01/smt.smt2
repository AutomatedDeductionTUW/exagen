;; Random number generator seed: Just 123
;; Index: 1
(define-sort A () (Array Int Int))
(declare-fun b () Int)
(declare-fun c () Int)
(declare-fun f (Int) Int)
(declare-fun a () A)
(assert (= (- b 3) (- c 1)))
(assert (not (= (f (- b 2)) (- b 1))))
(assert (= (select a (f c)) (- c 1)))
(assert (or (= (select a (f (- b 2))) b) (= (select (store a (- b 1) (f (- c 1))) (f c)) (+ c 1))))
(check-sat)
