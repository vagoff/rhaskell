apply_logical op1 op2 op3 (Cons a aa) (Cons b bb) = Cons (op1 a b) (apply_logical op1 op2 op3 aa bb)
apply_logical op1 op2 op3 a@(Cons _ _) Nil = map0 op2 a
apply_logical op1 op2 op3 Nil b@(Cons _ _) = map0 op3 b
apply_logical op1 op2 op3 Nil Nil = Nil

prepend2 a aa = (Cons a (Cons a aa))
prepend4 a aa = prepend2 a (prepend2 a aa)
prepend8 a aa = prepend4 a (prepend4 a aa)
