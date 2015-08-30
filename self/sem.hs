type TypEnv = OMap TypeName TypeDef
type DefEnv = OMap DataName Def
type ValEnv = OMap DataName Val

lookup_local defs name = ???
lookup_global env name = ???

type Def = List1 (List0 Pat,Exp)

data Val
    = LitVal Lit
    | OMpVal (OMap Data Data) ???
    | OStVal (OSet Data Data) ???
    | AppVal ConstrName (List1 Data)
    | TupVal (List2 Data)

eval (If v e2 e3) env =
    match v of
        Lit (BoolLit b) -> if b then eval e2 env else eval e3 env
        _ -> error "type error"

eval a@(AppVal h aa) =
    case lookup_local env h of
        Some cc -> eval_cases cc aa env
        None ->
            case lookup_global h of
                Some cc -> eval_cases cc aa env
                None -> a

eval (Let dd e) env = eval_defs dd env
eval (Case e cc) env = eval_cases (l1to0 cc) e env

eval_defs Nil env = env
eval_defs (Cons (p,e) dd) env =
    case match p (eval e env) env of
        Some env' -> eval_defs dd env'
        None -> error "let pattern failed"

eval_cases Nil e1 env = error "non exhaustive case operator"
eval_cases (Cons (p,e2) cc) e1 env =
    case match p e1 env of
        Some env' -> eval e2 env'
        None -> eval_cases cc e1 env

match UndPat _ env = Some env
match (VarPat n None) v env = Some (extend env n v)
match (VarPat n (Some p)) v env = match p v (extend env n v)
match (AppPat ph pp) (AppVal vh vv) env = if eq ph vh then match_all pp vv env
match (TupPat pp) (TupVal vv) env = match_all pp vv env
match _ _ _ = None

match_all Nil (Cons _ _) _ = None
match_all (Cons _ _) Nil _ = None
match_all Nil Nil env = Some env
match_all (Cons p pp) (Cons v vv) env =
    case match p v env of
        Some env' -> match_all pp vv env'
        None -> None
