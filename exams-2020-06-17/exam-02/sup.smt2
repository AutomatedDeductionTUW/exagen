(declare-sort A 0)
(declare-const b A)
(declare-fun g (A) A)
(declare-const a A)
(declare-fun f (A) A)
(assert (or (= (g (f a)) a) (= (g (f b)) a)))
(assert (or (not (= (f b) (f b))) (= (f a) a)))
(assert (= (f b) a))
(assert (not (= (g a) a)))