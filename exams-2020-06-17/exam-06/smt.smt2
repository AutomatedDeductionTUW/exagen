;; Random number generator seed: Just 123
;; Index: 6
;; Uninterpreted constants: fromList ["A","b","c"]
;; Numeric constants: fromList [1,2,3,4]
;; Function symbols: fromList ["+","-","=","f","not","or","select","store"]
(set-logic QF_AUFLIA)
(set-info :latex-linebreak-after-clause "3")
(declare-fun b () Int)
(declare-fun c () Int)
(declare-fun f (Int) Int)
(declare-fun A () (Array Int Int))
(assert (= (- b 1) (+ c 2)))
(assert (not (= (f b) (+ b 1))))
(assert (= (select A (f (+ c 3))) (+ c 2)))
(assert (or (= (select A (f b)) (+ b 2)) (= (select (store A (+ b 1) (f (+ c 2))) (f (+ c 3))) (+ c 4))))
(check-sat)
