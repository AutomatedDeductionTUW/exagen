(declare-sort A 0)
(declare-fun f (A) A)
(declare-fun g (A) A)
(declare-const b A)
(declare-const a A)
(assert (or (= (f (f b)) a) (= (f (g b)) a)))
(assert (= (f b) a))
(assert (or (not (= (g b) (g b))) (= (g b) a)))
(assert (not (= (f a) a)))