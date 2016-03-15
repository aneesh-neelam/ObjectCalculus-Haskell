module While(
    Aexp(IntExp, LocExp, PlusExp, MinusExp, TimesExp),
    Comm(Pass, Seq, InThenElse, While, Set),
    State(Empty, Assign),
    find, update, evalA, evalC
) where
--This is While.hs from class resource
-- Abstract syntax of arithmetic expressions
data Aexp = IntExp Int
    | LocExp String
    | PlusExp Aexp Aexp
    | MinusExp Aexp Aexp
    | TimesExp Aexp Aexp

-- Abstract syntax of boolean expressions
data Bexp = TrueExp
    | FalseExp
    | EqualsExp Aexp Aexp
    | LessExp Aexp Aexp
    | NotExp Bexp
    | AndExp Bexp Bexp
    | OrExp Bexp Bexp

-- Abstract syntax of commands
data Comm = Pass
    | Seq Comm Comm
    | IfThenElse Bexp Comm Comm
    | While Bexp Comm
    | Set String Aexp

-- State
data State = Empty
    | Assign String Int State
    deriving (Eq, Show)

-- Find a variable binding, return 0 by default
find :: State -> String -> Int
find Empty _ = 0
find (Assign x0 v0 s0) x =
    if x == x0 then v0 else find s0 x

-- Update state with assignment
update :: State -> String -> Int -> State
update s x v = Assign x v s

-- Evaluate an arithmetic expression
evalA :: Aexp -> State -> Int
evalA (IntExp n) _env = n
evalA (LocExp v) env = find env v
evalA (PlusExp e1 e2) env = (evalA e1 env) + (evalA e2 env)
evalA (MinusExp e1 e2) env = (evalA e1 env) - (evalA e2 env)
evalA (TimesExp e1 e2) env = (evalA e1 env) * (evalA e2 env)

-- Evaluate a boolean expression
evalB :: Bexp -> State -> Bool
evalB TrueExp _env = True
evalB FalseExp _env = False
evalB (EqualsExp e1 e2) env = (evalA e1 env) == (evalA e2 env)
evalB (LessExp e1 e2) env = (evalA e1 env) < (evalA e2 env)
evalB (NotExp b) env = not (evalB b env)
evalB (AndExp b1 b2) env = (evalB b1 env) && (evalB b2 env)
evalB (OrExp b1 b2) env = (evalB b1 env) || (evalB b2 env)

-- Evaluate a command
evalC :: Comm -> State -> State
evalC Pass env = env
evalC (Seq c1 c2) env =
    let env' = evalC c1 env in
    evalC c2 env'
evalC (IfThenElse b c1 c2) env =
    if bv then evalC c1 env else evalC c2 env
    where bv = evalB b env
evalC (While b c) env =
    if bv then evalC (Seq c (While b c)) env else env
    where bv = evalB b env
evalC (Set x e) env =
    update env x (evalA e env)