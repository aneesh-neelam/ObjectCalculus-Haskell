module ZetaCalculi(
    Label,
    Method(Zeta),
    Object,
    ZetaCalculus(Name, MI, MU),
    fv,
    subs,
    reduce
) where

import qualified Data.Map as Map
-- import qualified LambdaCalculi as LambdaCalculi
import qualified Data.List as List
import qualified Data.Set as Set



--Primitives of Zeta Calculus
type Label = String
data Method = Zeta Label ZetaCalculus
type Object = Map.Map Label Method

--Zeta Calculus
data ZetaCalculus = Name Label
    | Obj Object
    | MI Object Label
    | MU Object Label Method
    | Met Method

--Object Scoping
fv::ZetaCalculus -> (Set.Set Label)
fv (Met (Zeta y b)) = Set.delete y (fv b)
fv (Name x) = Set.singleton x
fv (Obj o) = Map.fold (\x acc -> Set.union (fv (Met x)) acc) Set.empty o
fv (MI a l) = fv (Obj a)
fv (MU a l m) = Set.union (fv (Obj a)) (fv (Met m))

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
subs::ZetaCalculus->Label->Label->ZetaCalculus
subs (Met (Zeta y b)) x c = Met (Zeta y' (subs (subs b y y') x c))
    where y' = fresh (Set.union (Set.union (fv (Met (Zeta y b))) (fv (Name c))) (Set.singleton x))
subs (Name y) x c
    | y /= x = Name y
    | y == x = Name c
subs (Obj obj) x c = Obj (Map.map (\y -> let q = (subs (Met y) x c) in case q of (Met m)->m) obj)
subs (MI a l) x c = case (subs (Obj a) x c) of
    (Obj tmp) -> MI tmp l

subs (MU a l m) x c = MU a1 l m1
    where (Obj a1) = (subs (Obj a) x c)
          (Met m1) = (subs (Met m) x c)

--Primitive Semantics
reduce:: ZetaCalculus -> ZetaCalculus
reduce (MI o l) = let res = (Map.lookup l o) in case res of
    Just (Zeta xj bj) -> subs bj xj o
reduce (MU o l m) = Obj (Map.adjust (\x -> m) l o)

--movable points
--One dimensional object
-- oneDPoint::(Name n, Attribute a)=>[(a,b)]
-- oneDPoint = Map.fromList [("x", 0), ("mv_x", )]
