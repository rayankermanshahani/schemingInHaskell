module Main where

import Data.Char (digitToInt)
import Numeric (readHex, readOct)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found value"

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (escapedChar <|> noneOf "\"")
  _ <- char '"'
  return $ String x
  where
    escapedChar :: Parser Char
    escapedChar = do
      _ <- char '\\'
      c <- oneOf "\\\"nrt"
      return $ case c of
        '\\' -> '\\'
        '"' -> '"'
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        _ -> error ("Unsupported escape character: " ++ [c])

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = try parseRadixedNumber <|> parseDecimal

parseRadixedNumber :: Parser LispVal
parseRadixedNumber = do
  _ <- char '#'
  radix <- oneOf "bodx"
  case radix of
    'b' -> do
      digits <- many1 (oneOf "01")
      return $ Number (bin2dec digits)
    'o' -> do
      digits <- many1 (oneOf "01234567")
      case readOct digits of
        [(n, "")] -> return $ Number n
        _ -> fail "Invalid octal number"
    'd' -> parseDecimal
    'x' -> do
      digits <- many1 hexDigit
      case readHex digits of
        [(n, "")] -> return $ Number n
        _ -> fail "Invalid hexadecimal number"
    _ -> fail "Invalid radix prefix"

parseDecimal :: Parser LispVal
parseDecimal = Number . read <$> many1 digit

-- Helper function to convert binary string to an Integer
bin2dec :: String -> Integer
bin2dec = foldl (\acc x -> acc * 2 + toInteger (digitToInt x)) 0

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn (readExpr expr)

-- Notes:
--
-- `readExpr` is a function from a string to string.
-- it takes a parameter `input` and passes it along with our
-- `symbol` parser to the `parse` function from the Parsec module.
--
-- Error handling is done with via an `Either` type which has an
-- `Left` constructor for errors and `Right` one for normal values.
--
-- Algebraic Data Types (ADTs) define a set of possible values that a variable
-- of that type can hold.
--
-- Product types are an "AND" combination of types:
-- eg. `data Point = Point Float Float`
--
-- Sum types are an "OR" combination of types:
-- eg. `data Maybe a = Nothing | Just a`
--
-- ADTs can combine both product and sum types:
-- eg. `data Shape = Circle Point Float         -- center and radius
--                 | Rectangle Point Point      -- two corners
--                 | Triangle Point Point Point -- three corners`
--
-- Chaining operations:
-- Use `>>` if actions don't return a value.
-- ie. "do this, then that"
--
-- Use `>>=` if you need to immediately pass the action's value into the next action.
-- ie. "do this and use its result immediately for that"
--
-- Use `do` otherwise.
-- ie. "write a recipe with multiple steps"
--
-- Each line of a `do` block must have the same type.
--
-- `liftM` does for Monads what `fmap` / `<$>` does for Functors:
--  ```
--  fmap  :: Functor f     => (a -> b) -> f a -> f b
--  liftM :: Monad m       => (a -> b) -> m a -> m b
--  ```
-- `liftM` implementation:
--  ```
--  liftM f m = do
--    x <- m       -- extract value from monad
--    return (f x) -- apply f and wrap it back in the monad
--  ```
--
--
