(declare-sort A 0)
(declare-fun g (A) A)
(declare-fun f (A) A)
(declare-const b A)
(declare-const a A)
(assert (or (= (f (g a)) a) (= (f (f b)) a)))
(assert (= (g a) a))
(assert (or (not (= (g b) (g b))) (= (f b) a)))
(assert (not (= (f a) a)))