data Uint8 --   0   1   2   3    4   5   6   7
    = UInt8   Bit Bit Bit Bit  Bit Bit Bit Bit

uint8_to_bb (Uint8 b0 b1 b2 b3 b4 b5 b6 b7) = Cons b0 (Cons b1 (Cons b2 (Cons b3 (Cons b4 (Cons b5 (Cons b6 (Cons b7 Nil)))))))

bb_to_uint8 aa =
    case apply_logical bit_or bit_id bit_id a zero8 of
        Cons b0 (Cons b1 (Cons b2 (Cons b3 (Cons b4 (Cons b5 (Cons b6 (Cons b7 bb))))))) -> (Uint8 b0 b1 b2 b3 b4 b5 b6 b7, bb)

zero8 = prepend8 O Nil
