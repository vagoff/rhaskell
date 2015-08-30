data Bit = I | O

bit_sub O O = (O, O)
bit_sub O I = (I, I)
bit_sub I O = (I, O)
bit_sub I I = (O, O)

bit_add O O = (O, O)
bit_add O I = (I, O)
bit_add I O = (I, O)
bit_add I I = (O, I)

bit_xor O O = O
bit_xor O I = I
bit_xor I O = I
bit_xor I I = O

bit_or O O = O
bit_or I O = I
bit_or O I = I
bit_or I I = I

bit_and O O = O
bit_and I O = O
bit_and O I = O
bit_and I I = I

bit_not O = I
bit_not I = O

bit_id O = O
bit_id I = I

b2b O = True
b2b I = False

b2i O = 0
b2i I = 1
