module HW3.Pretty where

import HW3.Base

import Prettyprinter.Render.Terminal(AnsiStyle)
import Prettyprinter
import Data.Ratio (numerator, denominator, (%))
import Data.Scientific (fromRationalRepetendUnlimited, formatScientific, FPFormat(Fixed), scientific) 
import Data.Foldable (toList)
import Data.ByteString (unpack)
import Numeric (showHex)
import Data.Word
import Data.Map(assocs)

-- | renders HiValue to Doc AnsiStyle document
prettyValue ::
 HiValue ->   -- ^ value render
 Doc AnsiStyle
prettyValue (HiValueNumber rat) = 
  let 
    numer = numerator rat
    denom = denominator rat
  in case quotRem numer denom of
    (a, 0) -> pretty a
    (a, b) -> 
      case fromRationalRepetendUnlimited (b % denom) of
        (lim, Nothing) -> 
          if a /= 0 then
            pretty (formatScientific Fixed Nothing (scientific a 0 + lim))
          else
            pretty (formatScientific Fixed Nothing lim)
        (_, Just _) -> 
          if a /= 0 then
            if b > 0 then
              pretty a <+> pretty "+" <+> pretty b <> pretty "/" <> pretty denom
            else 
              pretty a <+> pretty "-" <+> pretty (abs b) <> pretty "/" <> pretty denom
          else
            pretty b <> pretty "/" <> pretty denom
prettyValue (HiValueBool True) = pretty "true"
prettyValue (HiValueBool False) = pretty "false"
prettyValue (HiValueFunction f) = pretty $ getFunName f
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueString s) = viaShow s
prettyValue (HiValueList s) = prettyCollection "[" "]" ", " (toList s) prettyValue
prettyValue (HiValueBytes bytes) = prettyCollection "[#" "#]" " " (unpack bytes) toHex2
prettyValue (HiValueTime utc) = pretty "parse-time(\"" <> viaShow utc <> pretty "\")"
prettyValue (HiValueAction (HiActionRead dir)) = pretty "read(\"" <> pretty dir <> pretty "\")"
prettyValue (HiValueAction (HiActionWrite dir text)) = pretty "write(\"" <> pretty dir <> pretty ", " <> prettyValue (HiValueBytes text) <> pretty "\")"
prettyValue (HiValueAction (HiActionMkDir dir)) = pretty "mkdir(\"" <> pretty dir <> pretty "\")"
prettyValue (HiValueAction (HiActionChDir dir)) = pretty "cd(\"" <> pretty dir <> pretty "\")"
prettyValue (HiValueAction HiActionCwd) = pretty "cwd"
prettyValue (HiValueAction HiActionNow) = pretty "now"
prettyValue (HiValueAction (HiActionRand l r)) = pretty "rand(" <> pretty l <> pretty ", " <> pretty r <> pretty ")"
prettyValue (HiValueAction (HiActionEcho txt)) = pretty "echo(\"" <> pretty txt <> pretty "\")"
prettyValue (HiValueDict m) = prettyCollection "{" "}" ", " (assocs m) (\(k,v) -> prettyValue k <> pretty ": " <> prettyValue v)

-- | helper for render lists, strings or dictionaries
prettyCollection ::
 String ->          -- ^ opening string
 String ->          -- ^ closing string
 String ->          -- ^ delimiter string
 [a] ->             -- ^ collection
 (a -> Doc ann) ->  -- ^ pretty function
 Doc ann
prettyCollection op cl del lst form =
  case lst of
    [] -> pretty op <+> pretty cl
    (x : xs) -> pretty op <+> form x <> foldr helper mempty xs <+> pretty cl
    where
      helper x l = pretty del <> form x <> l

-- | format number to hex format
toHex2 ::
 Word8 ->
 Doc ann
toHex2 d
  | d < 0x10 = pretty "0" <> pretty (showHex d mempty)
  | otherwise = pretty $ showHex d mempty

-- | get HiFun function name
getFunName ::
 HiFun -> -- ^ function
 String
getFunName HiFunDiv = "div"
getFunName HiFunMul = "mul"
getFunName HiFunAdd = "add"
getFunName HiFunSub = "sub"
getFunName HiFunNot = "not"
getFunName HiFunAnd = "and"
getFunName HiFunOr = "or"
getFunName HiFunLessThan = "less-than"
getFunName HiFunGreaterThan = "greater-than"
getFunName HiFunEquals = "equals"
getFunName HiFunNotLessThan = "not-less-than"
getFunName HiFunNotGreaterThan = "not-greater-than"
getFunName HiFunNotEquals = "not-equals"
getFunName HiFunIf = "if"
getFunName HiFunReverse = "reverse"
getFunName HiFunLength = "length"
getFunName HiFunToUpper = "to-upper"
getFunName HiFunToLower = "to-lower"
getFunName HiFunTrim = "trim"
getFunName HiFunList = "list"
getFunName HiFunRange = "range"
getFunName HiFunFold = "fold"
getFunName HiFunPackBytes = "pack-bytes"
getFunName HiFunUnpackBytes = "unpack-bytes"
getFunName HiFunEncodeUtf8 = "encode-utf8"
getFunName HiFunDecodeUtf8 = "decode-utf8"
getFunName HiFunZip = "zip"
getFunName HiFunUnzip = "unzip"
getFunName HiFunSerialise = "serialise"
getFunName HiFunDeserialise = "deserialise"
getFunName HiFunRead = "read"
getFunName HiFunWrite = "write"
getFunName HiFunMkDir = "mkdir"
getFunName HiFunChDir = "chdir"
getFunName HiFunParseTime = "parse-time"
getFunName HiFunRand = "rand"
getFunName HiFunEcho = "echo"
getFunName HiFunKeys = "keys"
getFunName HiFunValues = "values"
getFunName HiFunCount = "count"
getFunName HiFunInvert = "invert"