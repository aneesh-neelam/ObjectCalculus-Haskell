-- Import QuickCheck and Map
import Test.QuickCheck
import qualified Data.Map as Map

-- Object is a map between labels and methods
type Object = (Map.Map String Method)

-- Function: Method Body: Similar to Lambda Calculus
data Body =
    VarB String
  | Abs String Body
  | App Body Body
  deriving (Show, Eq)

-- Method Data Structure, can be a field as well
data Method =
    VarM Int
  | Fun Body
  deriving (Show, Eq)

-- Object Calculus Syntax: Untyped Imperative
data Term =
    Var String
  | Obj Object
  | MetInv String
  | MetUpd String Method
  deriving (Show, Eq)

-- Generate fresh name
fresh :: [String] -> String
fresh avoid = f 0 where
  f n =
    let  x = "v" ++ show n in
    if  x `elem` avoid then
      f (n+1)
    else
      x

-- Return names of all variable in Method Body
bodyNames :: Body -> [String]
bodyNames (VarB x) = [x]
bodyNames (Abs _ t) = bodyNames t
bodyNames (App t0 t1) = bodyNames t0 ++ bodyNames t1

-- Evaluate Body
evalBody :: Body -> Maybe Body
evalBody (VarB x) = Nothing
evalBody (Abs x t) = Just (Abs x t)
evalBody (App t0 t1) = do
  Abs x t2 <- evalBody t0
  v1 <- evalBody t1
  evalBody (subsBody t2 x v1)

-- Substitution for Body
subsBody :: Body -> String -> Body -> Body
subsBody (VarB y) x s
  | x == y = s
  | otherwise = VarB y
subsBody (App t1 t2) x s = App (subsBody t1 x s) (subsBody t2 x s)
subsBody (Abs y t) x s = result where
  z = fresh (x : bodyNames s ++ bodyNames t)
  t' = subsBody t y (VarB z)
  result = Abs z (subsBody t' x s)

-- Invoke a Method
methodInvocation :: Object -> String -> Object
methodInvocation obj label = resultantObj where
  m = (lookup label obj)
  m' = evalMethod m
  resultantObj = Map.update m' label obj

-- Evaluate a Method
evalMethod :: Method -> Maybe Method
evalMethod (VarM number) = VarM number
evalMethod (Fun body) = Fun (evalBody body)

-- Update Method with new Method
methodUpdate :: Object -> String -> Method -> Object
methodUpdate obj label m' = resultantObj where
  resultantObj = Map.update m' label obj


-- Test Method Invocation
testMethodInvocation :: IO()
testMethodInvocation = do
  putStrLn "test1"

-- Test Method Update
testMethodUpdate :: IO()
testMethodUpdate = do
  putStrLn "test2"

-- Main function
main :: IO()
main = do
  testMethodInvocation
  testMethodUpdate
