data Exp
    = Let (List1 (PatExp, DataExp)) DataExp
    | If DataExp DataExp DataExp
    | Case DataExp (List1 (PatExp, DataExp))
    | App DataExp (List1 DataExp)
    | Ref Name
    | Val Data

data Pat
    = UndPat
    | AppPat ConstrName (List1 Pat)
    | TupPat (List2 Pat)
    | VarPat (Maybe Pat)
    | LitPat Lit

data Lit
    = BoolLit Bool
    | IntLit Int
    | FloatLit Float
    | StrLit String
