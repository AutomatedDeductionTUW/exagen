(declare-sort A 0)
(declare-const a A)
(declare-const b A)
(declare-const c A)
(declare-const d A)
(declare-fun f (A) A)
(declare-fun g (A A) A)
(declare-fun h (A) A)
(declare-fun P (A) Bool)
(assert (forall ((x A) (y A)) (or (not (P (h y))) (not (= (h d) (g a (g x x)))))))
(assert (or (P (f c)) (not (= (h d) (g a (g d d))))))
(assert (not (or (P (h (h b))) (P (f c)) (not (= (h d) (g a (g d d)))))))
(check-sat)