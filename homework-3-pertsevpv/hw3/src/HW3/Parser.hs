{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.ByteString (ByteString, pack)
import Data.Char
import Data.Text (Text, pack)
import Data.Void (Void)
import Data.Word
import HW3.Base
import Text.Megaparsec hiding (State, parse)
import Text.Megaparsec.Char (char, space, string)
import Text.Megaparsec.Char.Lexer (charLiteral)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ParseErrorBundle)

type HiParser = Parsec Void String

-- | takes string and parses expression of it
-- returns (Left error) if parse failed and (Right expression) otherwise
parse :: 
  String -> 
  Either (ParseErrorBundle String Void) HiExpr
parse = runParser parse' ""

parse' ::
 HiParser HiExpr
parse' = parseHiOp <* eof

-- | parse expression and its continuation
parseExpr ::
 HiParser HiExpr
parseExpr = do
  left <- parseHiExprValue
  parseCont left

-- | parse arguments and its continuation
parseArgs ::
 HiExpr ->  -- ^ left part
 HiParser HiExpr
parseArgs expr = do
  args <- parseArguments
  parseCont $ HiExprApply expr args

-- | parse '!' and its continuation
parseRun ::
 HiExpr ->  -- ^ left part
 HiParser HiExpr
parseRun expr = do
  _ <- char '!'
  parseCont (HiExprRun expr)

-- | parse dot and field after it and its continuation
parseDot ::
 HiExpr ->  -- ^ left part
 HiParser HiExpr
parseDot expr = do
  _ <- char '.'
  name <- HiExprValue . HiValueString <$> parseId
  parseCont $ HiExprApply expr [name]

-- | take some HiExpr and then try to parse continuation for it
parseCont ::
 HiExpr -> -- ^ left part
 HiParser HiExpr
parseCont left =
  choice
    [ try $ parseArgs left,
      try $ parseRun left,
      parseDot left
    ]
    <|> return left

-- | parse HiFun
parseHiFun ::
 HiParser HiFun
parseHiFun =
  choice
    [ HiFunDiv <$ string "div",
      HiFunMul <$ string "mul",
      HiFunAdd <$ string "add",
      HiFunSub <$ string "sub",
      HiFunAnd <$ string "and",
      HiFunOr <$ string "or",
      HiFunLessThan <$ string "less-than",
      HiFunGreaterThan <$ string "greater-than",
      HiFunEquals <$ string "equals",
      HiFunNotLessThan <$ string "not-less-than",
      HiFunNotGreaterThan <$ string "not-greater-than",
      HiFunNotEquals <$ string "not-equals",
      HiFunNot <$ string "not",
      HiFunIf <$ string "if",
      HiFunLength <$ string "length",
      HiFunToUpper <$ string "to-upper",
      HiFunToLower <$ string "to-lower",
      HiFunReverse <$ string "reverse",
      HiFunTrim <$ string "trim",
      HiFunList <$ string "list",
      HiFunRange <$ string "range",
      HiFunFold <$ string "fold",
      HiFunPackBytes <$ string "pack-bytes",
      HiFunUnpackBytes <$ string "unpack-bytes",
      HiFunEncodeUtf8 <$ string "encode-utf8",
      HiFunDecodeUtf8 <$ string "decode-utf8",
      HiFunZip <$ string "zip",
      HiFunUnzip <$ string "unzip",
      HiFunSerialise <$ string "serialise",
      HiFunDeserialise <$ string "deserialise",
      HiFunRead <$ string "read",
      HiFunWrite <$ string "write",
      HiFunMkDir <$ string "mkdir",
      HiFunChDir <$ string "cd",
      HiFunParseTime <$ string "parse-time",
      HiFunRand <$ string "rand",
      HiFunEcho <$ string "echo",
      HiFunCount <$ string "count",
      HiFunKeys <$ string "keys",
      HiFunValues <$ string "values",
      HiFunInvert <$ string "invert"
    ]

-- | parse HiExpr
parseHiExprValue ::
 HiParser HiExpr
parseHiExprValue =
  choice
    [ try parseHiValue,
      try parseSeq,
      try parseDict,
      inBrackets parseHiOp
    ]

-- | parse HiValue
parseHiValue ::
 HiParser HiExpr
parseHiValue =
  HiExprValue
    <$> choice
      [ try $ HiValueBool <$> inSpaces parseBool,
        try $ HiValueNumber <$> inSpaces parseRational,
        try $ HiValueFunction <$> inSpaces parseHiFun,
        try $ HiValueString <$> inSpaces parseString,
        try $ HiValueBytes <$> inSpaces parseByteString,
        try $ HiValueAction <$> inSpaces parseAction,
        inSpaces parseNull
      ]

-- | parse HiAction
parseAction :: HiParser HiAction
parseAction =
  choice
    [ try $ HiActionCwd <$ string "cwd",
      try $ HiActionNow <$ string "now"
    ]

-- | parse Rational number
parseRational :: HiParser Rational
parseRational = do
  minus <- optional (char '-')
  num <- L.scientific
  case minus of
    (Just _) -> do
      return $ toRational (negate num)
    Nothing -> do
      return $ toRational num

-- | parse 2 hex digits
parseHex ::
 HiParser Word8
parseHex = do
  a <- digitToInt <$> satisfy isHexDigit
  b <- digitToInt <$> satisfy isHexDigit
  return $ fromIntegral $ a * 16 + b

-- | parse field
parseId ::
 HiParser Text
parseId = Data.Text.pack . joinWith <$> sepBy1 ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) (char '-')
  where
    joinWith (x : xs) = x <> foldr joinWithHelper [] xs
    joinWith [] = mempty
    joinWithHelper res list = '-' : (res <> list)

-- | parse null
parseNull ::
 HiParser HiValue
parseNull = HiValueNull <$ string "null"

-- | parse string
parseString ::
 HiParser Text
parseString = Data.Text.pack <$> (char '"' *> manyTill charLiteral (char '"'))

-- | parse arguments 
-- (a, b, c)
parseArguments ::
 HiParser [HiExpr]
parseArguments = inBrackets (sepBy parseHiOp (char ','))

-- | parse sequence 
-- [a, b, c]
parseSeq ::
 HiParser HiExpr
parseSeq =
  between
    (inSpaces (char '['))
    (inSpaces (char ']'))
    (HiExprApply (HiExprValue (HiValueFunction HiFunList)) <$> sepBy parseHiOp (char ','))

-- | parse bytestring 
-- [# 00 11 aa #]
parseByteString ::
 HiParser ByteString
parseByteString =
  between
    (string "[#" <* space)
    (string "#]")
    (Data.ByteString.pack <$> sepEndBy (space *> parseHex) (some $ satisfy Data.Char.isSpace))

-- | parse dictionary
-- | { k1: v1, k2: v2, k3: v3 } 
parseDict ::
 HiParser HiExpr
parseDict =
  between
    (char '{')
    (char '}')
    (HiExprDict <$> sepBy parsePair (char ','))
  where
    parsePair :: HiParser (HiExpr, HiExpr)
    parsePair = do
      key <- parseHiOp
      _ <- char ':'
      value <- parseHiOp
      return (key, value)

-- | parse boolean 
-- true
parseBool ::
 HiParser Bool
parseBool =
  choice
    [ True <$ string "true",
      False <$ string "false"
    ]

-- | parse spaces around some parser
inSpaces ::
 HiParser a -> 
 HiParser a
inSpaces parser = space *> parser <* space

-- | parse brackets around some parser
inBrackets ::
 HiParser a -> HiParser a
inBrackets parser = inSpaces $ char '(' *> inSpaces parser <* char ')'

-- | parse infix operator
parseHiOp ::
 HiParser HiExpr
parseHiOp = makeExprParser term table

-- | parse term
term ::
 HiParser HiExpr
term =
  choice
    [ try $ inSpaces parseExpr,
      inBrackets parseHiOp
    ]

-- | table for expression parser
table =
  [ [ binaryInfixL "*" (binFunc HiFunMul),
      binaryInfixLNF "/" "=" (binFunc HiFunDiv)
    ],
    [ binaryInfixL "+" (binFunc HiFunAdd),
      binaryInfixL "-" (binFunc HiFunSub)
    ],
    [ binaryInfixNNF ">" "=" (binFunc HiFunGreaterThan),
      binaryInfixN ">=" (binFunc HiFunNotLessThan),
      binaryInfixNNF "<" "=" (binFunc HiFunLessThan),
      binaryInfixN "<=" (binFunc HiFunNotGreaterThan),
      binaryInfixN "==" (binFunc HiFunEquals),
      binaryInfixN "/=" (binFunc HiFunNotEquals)
    ],
    [ binaryInfixR "&&" (binFunc HiFunAnd)
    ],
    [ binaryInfixR "||" (binFunc HiFunOr)
    ]
  ]

binFunc :: HiFun -> HiExpr -> HiExpr -> HiExpr
binFunc fun left right = HiExprApply (HiExprValue (HiValueFunction fun)) [left, right]

binaryInfixL :: MonadParsec e s m => Tokens s -> (a -> a -> a) -> Operator m a
binaryInfixL name f = InfixL (f <$ string name)

binaryInfixR :: MonadParsec e s m => Tokens s -> (a -> a -> a) -> Operator m a
binaryInfixR name f = InfixR (f <$ string name)

binaryInfixN :: MonadParsec e s m => Tokens s -> (a -> a -> a) -> Operator m a
binaryInfixN name f = InfixN (f <$ string name)

binaryInfixLNF :: MonadParsec e s m => Tokens s -> Tokens s -> (a -> a -> a) -> Operator m a
binaryInfixLNF name fol f = InfixL (f <$ try (string name <* notFollowedBy (string fol)))

binaryInfixNNF :: MonadParsec e s m => Tokens s -> Tokens s -> (a -> a -> a) -> Operator m a
binaryInfixNNF name fol f = InfixN (f <$ try (string name <* notFollowedBy (string fol)))
