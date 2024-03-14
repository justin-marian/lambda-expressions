module Lambda.Sources.Parser (parse_expr, parse_code) where
import Lambda.Sources.Expr as Expr

import Data.Char
import Data.Text (strip)

import Control.Monad
import Control.Applicative

-- Parser data type

newtype Parser a = Parser {
    parse :: String -> Maybe(a, String)
}

--- type declaration ---

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  p >>= f = Parser $ \s -> case parse p s of
    Nothing -> Nothing
    Just (a, s') -> parse (f a) s'

instance Applicative Parser where
    pure x = return x
    pf <*> px = do
        f <- pf
        x <- px
        return $ f x

instance Functor Parser where
    fmap f px = do
        x <- px
        return $ f x

--- type declaration ---

-- Parse a single character that satisfies a predicate
-- | apply predicate on a character, if satisfy return tuple else emptry string/ doesn't satisfy predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
    c:cs | p c -> Just (c, cs)
    _    | otherwise -> Nothing

-- Parse a specific character
-- | successed only if the character is c
char :: Char -> Parser Char
char c = satisfy (== c)

-- Parse an identifier starting with a lowercase letter and followed with a number
-- | check 1st character is lower then  if the next characters are ciphers
identifier :: Parser String
identifier = do
    c <- satisfy isLower
    cs <- many $ satisfy isAlphaNum
    return (c:cs)

-- Parse a term = general expressions
term :: Parser Expr
term = parse_macro <|> var <|> func <|> parse_nestedExpr

-- Parse a var, identifier parse exactly a var
var :: Parser Expr
var = Variable <$> identifier

-- Parse a function = \\x.e
-- | starts with \\ followed by a lower character and a numver then . and a lambda expresion
func :: Parser Expr
func = do
    char '\\'
    vars <- some identifier
    char '.'
    body <- term
    let fct = foldr Function body vars
    return fct

-- Parse a nested expression in parentheses
-- | using combinator to match and consume parenthesis that wraps an expression
parse_nestedExpr :: Parser Expr
parse_nestedExpr = char '(' *> app <* char ')'

-- Parse an expression
-- | parse first general expression, parse the rest terms separated by spaces, then construct app
app :: Parser Expr
app = do
  t <- term
  ts <- many (char ' ' *> term)
  let exp = foldl Application t ts
  return exp

-- Parse a macro expression
-- | parse first character $, match an alphanumeric character and match a general expression, then construct macro
parse_macro :: Parser Expr
parse_macro = do
    char '$'
    name <- some $ satisfy isAlphaNum
    args <- many term
    let macro = foldl Application (Macro name) args
    return macro
    
-- Parse Code expression
-- | parse code with parse_assign if it is a macro Application (Macro name) args else evaluate the expression
parse_code :: String -> Code
parse_code input =
  case parse_assign input of
    Just (name, args) -> Assign name args
    Nothing -> Evaluate $ parse_expr input
  
-- Parse Macros inside of a Code expression
-- | using break function split input name and args and escape the spaces before and after "="
parse_assign :: String -> Maybe (String, Expr)
parse_assign input =
  case break (== '=') input of
    (name, '=' : args) -> Just (whiteSpace name, parse_expr (whiteSpace args))
    _ -> Nothing

-- Remove whitespaces before & after "=" character
whiteSpace :: String -> String
whiteSpace = whiteSpaceStart . whiteSpaceEnd

whiteSpaceStart :: String -> String
whiteSpaceStart = dropWhile isSpace

whiteSpaceEnd :: String -> String
whiteSpaceEnd = reverse . whiteSpaceStart . reverse

-- Parse an lambda expression
parse_expr :: String -> Expr
parse_expr input =
    case parse app input of
        Just (e, "") -> e
        Nothing -> error "Failed to parse expression"