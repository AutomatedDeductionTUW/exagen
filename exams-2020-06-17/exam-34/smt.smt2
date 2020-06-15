;; Random number generator seed: Just 123
;; Index: 34
;; Uninterpreted constants: fromList ["A","b","c"]
;; Numeric constants: fromList [1,2,3,4]
;; Function symbols: fromList ["+","-","=","f","not","or","select","store"]
(set-logic QF_AUFLIA)
(set-info :latex-linebreak-after-clause "3")
(declare-fun b () Int)
(declare-fun c () Int)
(declare-fun f (Int) Int)
(declare-fun A () (Array Int Int))
(assert (= (+ b 1) (- c 2)))
(assert (not (= (f (+ b 2)) (+ b 3))))
(assert (= (select A (f (- c 1))) (- c 2)))
(assert (or (= (select A (f (+ b 2))) (+ b 4)) (= (select (store A (+ b 3) (f (- c 2))) (f (- c 1))) c)))
(check-sat)
