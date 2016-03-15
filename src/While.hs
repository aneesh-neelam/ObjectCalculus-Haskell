module While(
    Aexp(IntExp, PlusExp, MinusExp, TimesExp),
    evalA
) where
--This is While.hs from class resource
-- Abstract syntax of arithmetic expressions
data Aexp = IntExp Int
    | LocExp String
    | PlusExp Aexp Aexp
    | MinusExp Aexp Aexp
    | TimesExp Aexp Aexp

-- Evaluate an arithmetic expression
evalA :: Aexp -> Int
evalA (IntExp n)  = n
evalA (PlusExp e1 e2)  = (evalA e1 ) + (evalA e2 )
evalA (MinusExp e1 e2)  = (evalA e1 ) - (evalA e2 )
evalA (TimesExp e1 e2)  = (evalA e1 ) * (evalA e2 )


data Method =

data Object = Empty
    | Var String Int Object
    | Met String Method Object
