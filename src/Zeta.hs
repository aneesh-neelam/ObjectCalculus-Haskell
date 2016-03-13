import qualified Data.Map as Map
import qualified LambdaCalculi as Lambda
import qualified Data.List as List
import qualified Data.Set as Set

module ZetaCalculi(
) where

--Primitives of Zeta Calculus
type Parameter = String
type Name = String
data Method = Zeta Parameter Body
data Attribute = Method | Double
type Object = Map.Map Name Attribute

--Primitive Semantics

invoke::Object->Name->
invoke obj methodName = invocationReduction obj[methodName] obj

update::Object->Name->Object
update obj methodName = newObj
    where newObj = obj
        newObj[methodName] = obj[methodName]

--Object Scoping
FV Zeta y b = Set.delete y (FV b)
--x is string
FV x = Set.singleton x
FV mapObject = Map.fold (\x acc -> Set.union (FV x) acc) Set.empty mapObject



--movable points
--One dimensional object
oneDPoint::(Name n, Attribute a)=>[(a,b)]
oneDPoint = Map.fromList [("x", 0), ("mv_x", )]
