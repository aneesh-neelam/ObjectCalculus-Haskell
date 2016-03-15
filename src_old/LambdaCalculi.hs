module LambdaCalculi(
    Name,
    Term(Var, Abs, App),
    names,
    fresh,
    subs,
    eval
) where

--Lambda Calculi

type Name = String

data Term =
    Var Name
  | Abs Name Term
  | App Term Term
  deriving Show

-- Lists all the variables appearing in a term.
--   This function lists not just the free variables, but ALL the variables.
--   It's fine if you wrote a function to list just the free variables, but I
--   chose to do this instead since it is easier.
-- This utility function is useful for defining substitution ("subs" below).
names :: Term -> [Name]
names (Var x) = [x]
names (Abs _ t) = names t
names (App t0 t1) = names t0 ++ names t1

-- Generates a name that does not appear in the given list.
-- This utility function is useful for defining substitution ("subs" below).
fresh :: [Name] -> Name
fresh avoid = f 0 where
  f n =
    let  x = "v" ++ show n  in
    if  x `elem` avoid  then
      f (n+1)
    else
      x


-- subs t x s
--   Performs capture-avoiding substitution of s for x in t.
subs :: Term -> Name -> Term -> Term
subs (Var y)     x s | x == y    = s
                     | otherwise = Var y
subs (App t1 t2) x s             = App (subs t1 x s) (subs t2 x s)
subs (Abs y t)   x s             = result where
  z = fresh (x : names s ++ names t)
  t' = subs t y (Var z)
  result = Abs z (subs t' x s)


-- Evaluates a term according to the call-by-value rules.
-- A return value of Nothing indicates an error.
--   (The only possible error is to encounter an unbound variable.)
eval :: Term -> Maybe Term
eval (Var x) = Nothing
eval (Abs x t) = Just (Abs x t)
eval (App t0 t1) = do
  Abs x t2 <- eval t0
  v1 <- eval t1
  eval (subs t2 x v1)
