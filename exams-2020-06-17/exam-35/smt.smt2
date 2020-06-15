;; Random number generator seed: Just 123
;; Index: 35
;; Uninterpreted constants: fromList ["A","b","c"]
;; Numeric constants: fromList [1,2,3,4]
;; Function symbols: fromList ["-","=","f","not","or","select","store"]
(set-logic QF_AUFLIA)
(set-info :latex-linebreak-after-clause "3")
(declare-fun b () Int)
(declare-fun c () Int)
(declare-fun f (Int) Int)
(declare-fun A () (Array Int Int))
(assert (= (- b 4) (- c 3)))
(assert (not (= (f (- b 3)) (- b 2))))
(assert (= (select A (f (- c 2))) (- c 3)))
(assert (or (= (select A (f (- b 3))) (- b 1)) (= (select (store A (- b 2) (f (- c 3))) (f (- c 2))) (- c 1))))
(check-sat)
