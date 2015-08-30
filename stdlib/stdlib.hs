data List0 a = Cons a | Nil -- [!] embedded!
data List1 a = List1 a (List0 a) -- [!] embedded!
data List2 a = List2 a a (List0 a) -- [!] embedded!
data Option a = Some a | None

data OMap = ??? ??? ???
