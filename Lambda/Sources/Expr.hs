module Lambda.Sources.Expr where

{-- A lambda expression is:
        - a Variable    - x
        - a Function    - \\x.e
        - a Application - e1 e2
        - a Macro       - $m
--}

-- Data Types

-- Expr can be a Variable, a Function, an Application or a Macro
--  Variable: a string of characters (e.g. x)
--  Function: a string of characters and an expression (e.g. \\x.e)
--  Application: two expressions (e.g. e1 e2)
--  Macro: a string of characters preceded by a dollar sign (e.g. $m)
data Expr = Variable String
          | Function String Expr
          | Application Expr Expr
          | Macro String

-- Code can be an evaluation or an assignment
-- Evaluate: an expression to be evaluated (e.g. Evaluate e)
-- Assign: a string of characters and an expression (e.g. Assign "x" e)
data Code = Evaluate Expr
          | Assign String Expr
    deriving (Eq, Show)

-- Shorthand functions
v = Variable
f = Function
a = Application
macro = Macro

-- Show Instance 
instance Show Expr where
    show (Variable x) = x
    show (Function x (Application e1 e2)) = ('λ':x) ++ ".(" ++ (show e) ++ ")"
        where e = (Application e1 e2)
    show (Function x e) = ('λ':x) ++ ('.':(show e))
    show (Application e1 (Application u v)) = (show e1) ++ " (" ++ (show e2) ++ ")"
        where e2 = (Application u v)
    show (Application e1 e2) = (show e1) ++ (' ':(show e2))
    show (Macro x) = "$" ++ x

-- Equality Instance
instance Eq Expr where
    (==) e1 e2 = equal e1 e2 []
      where
        equal :: Expr -> Expr -> [(String, String)] -> Bool
        equal (Variable x) (Variable y) env = case (lookup x env) of 
                                                (Just xv) -> xv == y
                                                Nothing -> x == y

        equal (Function x e1) (Function y e2) env = equal e1 e2 ((x,y):env)
        equal (Application e1 e2) (Application e3 e4) env = (equal e1 e3 env) && (equal e2 e4 env)
        equal (Macro x) (Macro y) env = x == y
        equal _ _ _ = False
