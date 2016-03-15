module ZetaCalculi(
    Label,
    Method(Zeta),
    Object,
    ZetaCalculus(Name, MI, MU),
    subs, reduce
) where

import qualified While as While
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set

--Primitives of Zeta Calculus
type Label = String
data Method = Zeta Object Body | Const Int
type Object = (Map.Map Label Method)


data Body = Empty
    | Assign Object Label Aexp

--Zeta Calculus
data ZetaCalculus = Name Label
    | Obj Object
    | MI Object Label
    | MU Object Label Method
    | Met Method

-- Generates a name that does not appear in the given list.
-- This utility function is useful for defining substitution ("subs" below).
--This method is copied from Tommy's code
fresh :: Set.Set Label -> Label
fresh set = f 0 where
    f n =
        let x = "v" ++ show n in
        if  Set.member x set then
            f (n+1)
        else
            x

--Object substitution
subs::ZetaCalculus->Object->Object->ZetaCalculus
subs (Met (Zeta y b)) x c
    | y /= x = Met (Zeta y b)
    | y == x = Met (Zeta c (subsBody b x c))
subs (Name y) x c
    | y /= x = Name y
    | y == x = Name c
subs (Obj obj) x c
    | obj == x = c
    | obj /= x = obj
subs (MI a l) x c = case (subs (Obj a) x c) of
    (Obj tmp) -> MI tmp l

subs (MU a l m) x c = MU a1 l m1
    where (Obj a1) = (subs (Obj a) x c)
          (Met m1) = (subs (Met m) x c)

subsBody::Body -> Object -> Object ->Body
subsBody Empty _ _ = b
subsBody (Assign o l a) o1 o2
    | o1 == o2 = (Assign o2 l a)
    | o1 /= o2 = (Assign o l a)


--Primitive Semantics
reduce:: ZetaCalculus -> ZetaCalculus
reduce (MI o l) = let res = (Map.lookup l o) in case res of
    Just (Zeta xj bj) -> subs bj xj o
reduce (MU o l m) = Obj (Map.adjust (\x -> m) l o)

--Translation of the untyped A-calculus
translate::LambdaCalculi.Term -> Object
translate (LambdaCalculi.Obj x) = x

assign::Object -> Label -> Int -> Object
assign o l i = (Map.adjust (\x -> i) l o)

evalBody::Object-> Label-> Aexp->Object
evalBody o l a = assign o l (While.evalA a)

testCase = do
    let
        emptyObj = Map.fromList[("x", Const 0)];
        body = Assign emptyObj "x" (PlusExp (IntExp 2) (IntExp arg))
        testObj = Map.fromList[("x", Const 0), ("mv", )]

--movable points
--One dimensional object
-- oneDPoint::(Name n, Attribute a)=>[(a,b)]
-- oneDPoint = Map.fromList [("x", 0), ("mv_x", )]