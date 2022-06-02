{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6 where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Scientific
import GHC.Natural (Natural)
import HW2.T1 (Annotated (..), Except (..))
import HW2.T4
import HW2.T5

data ParseError = ErrorAtPos Natural deriving (Show)

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

-- | function that run parser on some string
runP :: 
  Parser a -> -- ^ parser
  String ->   -- ^ string
  Except ParseError a
runP (P (ES es)) s = unpack (es (0, s))
  where
    unpack (Success (a :# _)) = Success a
    unpack (Error (ErrorAtPos n)) = Error (ErrorAtPos n)

-- | function that parse one char 
pChar ::
 Parser Char
pChar =
  P $                                                 -- ^ wrap in P
    ES                                                -- ^ wrap in ES
      ( \(pos, s) ->                                  -- ^ create lambda function (pos, s) -> Except ParseError a
          case s of                                   -- ^ look at our string
            [] -> Error (ErrorAtPos pos)              -- ^ if it is empty we throw Error
            (c : cs) -> Success (c :# (pos + 1, cs))  -- ^ else, we return success annotation with our parsed char :# (incremented pos, rest of string)
      )

-- | returns parser, that always fails
parseError :: 
  Parser a
parseError = P (ES (\(pos, _) -> Error (ErrorAtPos pos)))

-- | it's helper function for <|> implementation. It takes two Excepts and if 1st is Error, returns 2nd else - 1st
orExp :: 
  Except e a -> -- ^ left result 
  Except e a -> -- ^ right result
  Except e a
orExp (Error _) b = b
orExp a _ = a

instance Alternative Parser where
  empty = parseError
  (P p) <|> (P q) = P (ES (\s -> orExp (runES p s) (runES q s)))

instance MonadPlus Parser

-- | parser that parse end of string. fails if string is not empty
pEof :: Parser ()
pEof =
  P $
    ES
      ( \(pos, s) -> case s of
          [] -> Success (() :# (pos, s))
          _ -> Error (ErrorAtPos pos)
      )

-- | expression parser
parseExpr :: 
  String ->               -- ^ string to parse
  Except ParseError Expr
parseExpr = runP pStart

-- | parser that parse one expected char
pExChar :: 
  Char -> 
  Parser Char
pExChar ch = mfilter (== ch) pChar >>= \s -> pure s

-- |  whitespace parser
pWS :: 
  Parser [Char]
pWS = many (pExChar ' ')

-- | left bracket parser
pLeftBr :: 
  Parser Char
pLeftBr = pExChar '('

-- | right bracket parser
pRightBr ::
 Parser Char
pRightBr = pExChar ')'

-- | dot parser
pDot :: 
  Parser Char
pDot = pExChar '.'

-- | digit parser
pDigit :: 
  Parser Char
pDigit = mfilter isDigit pChar >>= \s -> pure s

-- | num parser
pNum :: 
  Parser [Char]
pNum = some pDigit >>= \s -> pure s

-- | double parser
pDouble :: 
  Parser Expr
pDouble = do
  befDot <- pNum
  aftDot <- optional pAfterDot
  let num = befDot ++ fromMaybe "" aftDot
  let (size, number) = pD num (-1, 0)
  pure $ Val $ toRealFloat $ scientific number $ - size
  where
    -- | helper function that takes double in string format and returns its integer value without dot and number of digits after dot
    pD :: 
      String ->         -- ^ number
      (Int, Integer) -> -- ^ (number of after dot digits, integer)
      (Int, Integer)    
    pD ('.' : as) (_, num) = pD as (0, num)
    pD (a : as) (-1, num) = pD as (-1, num * 10 + toInteger (digitToInt a))
    pD (a : as) (sz, num) = pD as (sz + 1, num * 10 + toInteger (digitToInt a))
    pD [] (-1, num) = (0, num)
    pD [] acc = acc

-- | parser dot and digits after it
pAfterDot :: Parser [Char]
pAfterDot = do
  dot <- pDot
  num <- pNum
  pure (dot : num)

-- | below are parser written in next grammar :
-- Start -> Expr eof 
-- Expr -> Term Expr'
-- Expr' -> + Term Expr'
-- Expr' -> - Term Expr'
-- Term -> Fact Term'
-- Term' -> * Fact Term'
-- Term' -> / Fact Term'
-- Fact -> <num> | (Expr)

pStart :: Parser Expr
pStart = do
  ex <- pExpr
  pEof
  pure ex
  
pExpr :: Parser Expr
pExpr = do
  _ <- pWS
  l <- pTerm
  _ <- pWS
  ex <- pExpr' l <|> pEps l
  pure ex

pExpr' :: Expr -> Parser Expr
pExpr' l = do
  _ <- pWS
  l <- pExprPlus l <|> pExprMinus l
  _ <- pWS
  ex <- pExpr' l <|> pEps l
  pure ex

pExprPlus :: Expr -> Parser Expr
pExprPlus l = do
  _ <- pWS
  pExChar '+'
  _ <- pWS
  r <- pTerm
  pure (Op (Add l r))

pExprMinus :: Expr -> Parser Expr
pExprMinus l = do
  _ <- pWS
  pExChar '-'
  _ <- pWS
  r <- pTerm
  pure (Op (Sub l r))

pTerm :: Parser Expr
pTerm = do
  _ <- pWS
  l <- pFact
  _ <- pWS
  ex <- pTerm' l <|> pEps l
  pure ex

pTerm' :: Expr -> Parser Expr
pTerm' l = do
  _ <- pWS
  l <- pTermMult l <|> pTermDiv l
  _ <- pWS
  ex <- pTerm' l <|> pEps l
  pure ex

pTermDiv :: Expr -> Parser Expr
pTermDiv l = do
  _ <- pWS
  _ <- pExChar '/'
  _ <- pWS
  r <- pFact
  pure (Op (Div l r))

pTermMult :: Expr -> Parser Expr
pTermMult l = do
  _ <- pWS
  _ <- pExChar '*'
  _ <- pWS
  r <- pFact
  pure (Op (Mul l r))

pFact :: Parser Expr
pFact = pDouble <|> (pLeftBr *> pExpr <* pRightBr)

pEps :: Expr -> Parser Expr
pEps = pure
