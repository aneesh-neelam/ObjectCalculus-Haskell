import qualified Data.Map as Map
import qualified LambdaCalculi as Lambda

module ZetaCalculi(
) where

--Primitives of Zeta Calculus
type Parameter = String
data Method = Zeta Parameter Body
data Attribute = Method | Double
data Object = Map.fromList []

--movable points
--One dimensional object
oneDPoint = Map.fromList [("x", 0), ("mv_x", )]
