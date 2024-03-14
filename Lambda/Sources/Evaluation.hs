module Lambda.Sources.Evaluation where

import Lambda.Sources.Expr
import Data.List

-- All possible variable names ("x1", "x2", "x3", "x4", ...)
-- n - specify variable number 1, 2, 3, 4, ...
-- c - all variables start with `x` letter
allVars :: [String]
allVars = [c : if n == 0 then "" else show n | n <- [1..], c <- ['x']]

-- Get a New variable name, first one from allVars who is not in usedVars
newVar :: [String] -> String
newVar usedVars = head [v | v <- allVars, v `notElem` usedVars]

-- Check if the expression has a redex
hasRedex :: Expr -> Bool
hasRedex (Application (Function _ _) _) = True
hasRedex (Application e1 e2) = hasRedex e1 || hasRedex e2
hasRedex (Function _ e) = hasRedex e
hasRedex _ = False

-- Replace a lambda expression
replace :: Expr -> String -> Expr -> Expr
replace (Variable y) x e = if y == x then e else Variable y
replace (Function x e1) y e2 = if y == x then Function x e1 else Function x (replace e1 y e2)
replace (Application e1 e2) x e3 = Application (replace e1 x e3) (replace e2 x e3)

-- Recursively traverses the expression and performs lambda substitution
substitute :: String -> Expr -> Expr -> Expr
-- Variable substiute: expr expression or original variable
substitute var expr (Variable x) = if x == var then expr else Variable x
-- Function substitute: 
substitute var expr (Function x e)
  -- original function, no substitution
  | x == var = Function x e
  -- check variable if is used, if it is generate a expr variable for e and variable to be substituted var
  | x `elem` free_vars expr = Function new_var (substitute var expr new_expr)
  -- substitute occurances of x in e
  | x `notElem` free_vars expr = Function x (substitute var expr e)
  where
    -- expr variable obtained which is free in usedVars, function x e and var variable
    new_var = newVar (free_vars expr `union` free_vars (Function x e) `union` [var])
    -- performing substitution of x
    new_expr = substitute x (Variable new_var) e
-- Application substitute: replace occurances of var with the expression expr in e1 and e2
substitute var expr (Application e1 e2) = Application (substitute var expr e1) (substitute var expr e2)

-- | Applicative substitution: replace occurances of x with e2 -- last expression R->L
substituteA :: String -> Expr -> Expr -> Expr
substituteA x e1 e2 = substitute x e2 e1
-- | Normal substitution: replace occurances of x with e1 -- first expression L->R
substituteN :: String -> Expr -> Expr -> Expr
substituteN x e1 e2 = substitute x e1 e2

-- Find free variables of a Expr
free_vars :: Expr -> [String]
free_vars (Variable x) = [x]
free_vars (Function x e) = filter (/= x) (free_vars e)
free_vars (Application e1 e2) = free_vars e1 `union` free_vars e2
free_vars (Macro name) = [name]

-- Reduce a redex
reduce :: Expr -> String -> Expr -> Expr
-- reduce Variable
reduce (Variable y) x e = if y == x then e else Variable y
-- reduce Function
reduce (Function x e1) y e2
  | x == y = Function x e1
  -- substitute all occurences and apply reduce
  | otherwise = Function new_var (reduce (substitute x (Variable new_var) e1) y e2)
  where
    -- generate New variable consists variables that are not used in x e1 or y e2 with [y] variable
    new_var = newVar (free_vars (Function x e1) `union` free_vars (Function y e2) `union` [y])
-- reduce Application
reduce (Application e1 e2) x e3 = Application (reduce e1 x e3) (reduce e2 x e3)

-- Normal Evaluation - perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN (Application (Function x e1) e2) = substituteN x e2 e1
stepN (Application e1 e2)
  | hasRedex e1 = Application (stepN e1) e2
  | hasRedex e2 = Application e1 (stepN e2)
  | otherwise = error "No redex found in application N"
stepN (Function x e) = Function x (stepN e)
stepN e = e

-- Reduce to Normal form using the Normal strategy
reduceN :: Expr -> Expr
reduceN e = if hasRedex e then reduceN (stepN e) else e

-- Reduce to Normal form using the Normal strategy & return all intermediate results
reduceAllN :: Expr -> [Expr]
reduceAllN e = e : case stepN e of
  r | r == e -> []
    | otherwise -> reduceAllN r
  
-- Applicative Evaluation - perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA (Application e1 e2)
  | hasRedex e1 = Application (stepA e1) e2
  | hasRedex e2 = Application e1 (stepA e2)
  | (Function x e1') <- e1 = substituteA x e1' e2
  | otherwise = error "No redex found in application A"
stepA (Function x e) = Function x (stepA e)
stepA e = e

-- Reduce to Aplicative form using the Aplicative strategy
reduceA :: Expr -> Expr
reduceA e = if hasRedex e then reduceA (stepA e) else e
-- | Reduce to Applicative form using the Applicative strategy & return all intermediate results
reduceAllA :: Expr -> [Expr]
reduceAllA e = e : case stepA e of
  r | r == e -> []
    | otherwise -> reduceAllA r

-- Make a substitutions into a expression with Macros
-- Substitutes expressions with their corresponding macro definitions
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros context (Macro x) =
  case lookup x context of
    Just e -> evalMacros context e
    Nothing -> Macro x
evalMacros context (Variable x) =
  case lookup x context of
    Just e -> evalMacros context e
    Nothing -> Variable x
evalMacros context (Function x e) = Function x (evalMacros context e)
evalMacros context (Application e1 e2) = Application (evalMacros context e1) (evalMacros context e2)

-- Evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode strategy code = evalCode' strategy code []

-- Evaluates a code sequence using the given strategy
evalCode' :: (Expr -> Expr) -> [Code] -> [(String, Expr)] -> [Expr]
-- DEFAULT
evalCode' _ [] _ = []
-- EVALUATE
evalCode' strategy (Evaluate expr : code) context =
  let value = strategy (evalMacros context expr)
  in value : evalCode' strategy code context
-- ASSIGN
evalCode' strategy (Assign name expr : code) context =
  let value = evalMacros context (evalMacros context expr)
  in evalCode' strategy code ((name, value) : context)