map2 f (List2 a1 a2 aa) = List2 (f a1) (f a2) (map0 f aa)
map1 f (List1 a aa) = List1 (f a) (map0 f aa)
map0 f (Cons a aa) = Cons (f a) (map0 f aa)
map0 _ Nil = Nil

l1to0 (List1 a aa) = Cons a aa
l2to0 (List2 a1 a2 aa) = Cons a1 (Cons a2 aa)
l2to1 (List2 a1 a2 aa) = List1 a1 (Cons a2 aa)

reverse aa = reverse_to aa Nil

reverse_to (Cons a aa) bb = reverse_to aa (Cons a bb)
reverse_to Nil bb = bb

concat Nil bb = bb
concat (Cons a aa) bb = Cons a (concat aa bb)

singleton0 a = Cons a Nil
singleton1 a = List1 a Nil
