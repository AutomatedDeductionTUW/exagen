;; Random number generator seed: Just 123
;; Index: 36
;; Uninterpreted constants: fromList ["A","b","c"]
;; Numeric constants: fromList [1,2,3]
;; Function symbols: fromList ["+","=","f","not","or","select","store"]
(set-logic QF_AUFLIA)
(set-info :latex-linebreak-after-clause "3")
(declare-fun b () Int)
(declare-fun c () Int)
(declare-fun f (Int) Int)
(declare-fun A () (Array Int Int))
(assert (= b c))
(assert (not (= (f (+ b 1)) (+ b 2))))
(assert (= (select A (f (+ c 1))) c))
(assert (or (= (select A (f (+ b 1))) (+ b 3)) (= (select (store A (+ b 2) (f c)) (f (+ c 1))) (+ c 2))))
(check-sat)
