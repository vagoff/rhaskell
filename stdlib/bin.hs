data BinInt = Pos (List1 Bit) | Zero | Neg (List1 Bit)

neg (Pos m) = Neg m
neg Zero = Zero
neg (Neg m) = Pos m

sub a b = add a (neg b)

abs (Pos a) = Pos a
abs Zero = Zero
abs (Neg a) = Pos a

add Zero b = b
add a Zero = a
add (Pos a) (Pos b) = Pos (add_magnitudes a b)
add (Pos a) (Neg b) = ???
add (Neg a) (Pos b) = ???
add (Neg a) (Neg b) = Neg (add_magnitudes a b)

sub_loop aa Nil borrow = ???
sub_loop Nil bb borrow = ???
sub_loop (Cons a aa) (Cons b bb) borrow =
    let
        (a_minus_b, borrow_1) = bit_sub a b
        (result, borrow_2) = bit_sub a_minus_b borrow
        tail = sub_loop aa bb (bit_or borrow_1 borrow_2)
    in
        Cons result tail

add_magnitudes (List1 a aa) (List1 b bb) =
    let
        (a_plus_b, carry) = bit_add a b
        tail = add_loop aa bb carry
    in
        List1 a_plus_b tail

add_loop aa Nil carry = ???
add_loop Nil bb carry = ???
add_loop (Cons a aa) (Cons b bb) carry =
    let
        (a_plus_b, carry_1) = bit_add a b
        (result, carry_2) = bit_add a_plus_b carry
        tail = add_loop aa bb (bit_or carry_1 carry_2)
    in
        Cons result tail

mul a b = ???

mod a b = ???

trunc_div a b = ???

print_binary a = bin2s a

bin2s (Pos aa) = strcat "+" ( bb2s (l1to0 aa) 1 0 )
bin2s Zero = " 0"
bin2s (Neg aa) = strcat "-" ( bb2s (l1to0 aa) 1 0 )

bb2s (Cons a aa) !m !a = bb2s aa (int_mul m 2) (int_add a (int_mul m (b2i a)))
bb2s Nil _ _ = 0
