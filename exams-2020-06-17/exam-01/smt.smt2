;; Random number generator seed: Just 123
;; Index: 1
;; Uninterpreted constants: fromList ["A","b","c"]
;; Numeric constants: fromList [1,2,3]
;; Function symbols: fromList ["+","-","=","f","not","or","select","store"]
(set-logic QF_AUFLIA)
(set-info :latex-linebreak-after-clause "3")
(declare-fun b () Int)
(declare-fun c () Int)
(declare-fun f (Int) Int)
(declare-fun A () (Array Int Int))
(assert (= (- b 3) (- c 1)))
(assert (not (= (f (- b 2)) (- b 1))))
(assert (= (select A (f c)) (- c 1)))
(assert (or (= (select A (f (- b 2))) b) (= (select (store A (- b 1) (f (- c 1))) (f c)) (+ c 1))))
(check-sat)
