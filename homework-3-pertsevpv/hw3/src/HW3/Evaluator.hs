{-# LANGUAGE OverloadedStrings #-}

module HW3.Evaluator where

import Codec.Compression.Zlib (bestCompression, compressLevel, compressWith, decompress, defaultCompressParams)
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Monad.Except
import Control.Monad.Trans.Except (throwE)
import Data.ByteString (ByteString, drop, index, length, pack, take, unpack, reverse, length)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (toList)
import Data.Map (Map, assocs, elems, fromList, insert, keys, lookup)
import Data.Scientific (fromRationalRepetendUnlimited, isInteger, toBoundedInteger)
import Data.Semigroup (stimes)
import Data.Sequence.Internal (Seq, empty, fromList, index, length, reverse, (<|), (><), take, drop)
import Data.Text (Text, concat, cons, drop, dropEnd, index, length, pack, reverse, strip, toLower, toUpper, unpack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (UTCTime)
import Data.Time.Clock (addUTCTime, diffUTCTime)
import Data.Word
import GHC.Natural (Natural)
import HW3.Base
import Text.Read (readMaybe)

-- | evaluates expression. return (Left error) if failed, otherwise (Right result)
eval ::
 HiMonad m => 
 HiExpr ->    -- ^ expression to evaluate
 m (Either HiError HiValue)
eval expr = runExceptT (evalHelper expr)

-- | helper function for eval. for the convenience of error handling, we make calculations using ExceptT
evalHelper :: 
  HiMonad m => 
  HiExpr ->   -- ^ expression to evaluate
  ExceptT HiError m HiValue
evalHelper (HiExprValue value) = return value
-- invalid functions
evalHelper (HiExprApply (HiExprValue (HiValueNumber _)) _) = throwE HiErrorInvalidFunction
evalHelper (HiExprApply (HiExprValue (HiValueBool _)) _) = throwE HiErrorInvalidFunction
evalHelper (HiExprApply (HiExprValue (HiValueAction _)) _) = throwE HiErrorInvalidFunction
evalHelper (HiExprApply (HiExprValue (HiValueTime _)) _) = throwE HiErrorInvalidFunction
evalHelper (HiExprApply (HiExprValue HiValueNull) _) = throwE HiErrorInvalidFunction
-- binary functions
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) args) = applyBiFun add' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunMul)) args) = applyBiFun mul' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunSub)) args) = applyBiFun sub' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunDiv)) args) = applyBiFun div' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunGreaterThan)) args) = applyBiFun greaterThan' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunEquals)) args) = applyBiFun equals' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunNotLessThan)) args) = applyBiFun notLessThan' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunNotGreaterThan)) args) = applyBiFun notGreaterThan' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunNotEquals)) args) = applyBiFun notEquals' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunLessThan)) args) = applyBiFun lessThan' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunWrite)) args) = applyBiFun write' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunRange)) args) = applyBiFun range' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunRand)) args) = applyBiFun rand' args
-- unary functions
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunNot)) args) = applyOneArgFun not' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunPackBytes)) args) = applyOneArgFun packBytes' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunUnpackBytes)) args) = applyOneArgFun unpackBytes' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunEncodeUtf8)) args) = applyOneArgFun myEncodeUtf8 args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunDecodeUtf8)) args) = applyOneArgFun myDecodeUtf8 args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunZip)) args) = applyOneArgFun zip' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunUnzip)) args) = applyOneArgFun unzip' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunSerialise)) args) = applyOneArgFun serialise' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunDeserialise)) args) = applyOneArgFun deserialise' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunRead)) args) = applyOneArgFun read' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunMkDir)) args) = applyOneArgFun mkdir' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunChDir)) args) = applyOneArgFun chdir' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunParseTime)) args) = applyOneArgFun parseTime' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunCount)) args) = applyOneArgFun count' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunKeys)) args) = applyOneArgFun keys' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunValues)) args) = applyOneArgFun values' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunInvert)) args) = applyOneArgFun invert' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunEcho)) args) = applyOneArgFun echo' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunLength)) args) = applyOneArgFun length' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunToUpper)) args) = applyOneArgFun toUpper' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunToLower)) args) = applyOneArgFun toLower' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunReverse)) args) = applyOneArgFun reverse' args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunTrim)) args) = applyOneArgFun trim' args
-- short-circuit evaluation
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunAnd)) args) = applyAnd args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunOr)) args) = applyOr args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunIf)) args) = applyIf args
-- indexes and slices
evalHelper (HiExprApply (HiExprValue (HiValueString str)) args) = applyIndexOrSlice (HiValueString str) args
evalHelper (HiExprApply (HiExprValue (HiValueList s)) args) = applyIndexOrSlice (HiValueList s) args
evalHelper (HiExprApply (HiExprValue (HiValueBytes bytes)) args) = applyIndexOrSlice (HiValueBytes bytes) args
-- other functions
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunList)) args) = applyList args
evalHelper (HiExprApply (HiExprValue (HiValueFunction HiFunFold)) args) = applyFold args
evalHelper (HiExprApply (HiExprValue (HiValueDict d)) args) = applyField (HiValueDict d) args
-- expressions
evalHelper (HiExprRun (HiExprValue (HiValueAction act))) = do
  lift $ runAction act
evalHelper (HiExprRun (HiExprApply expr args)) = do
  res <- evalHelper (HiExprApply expr args)
  evalHelper (HiExprRun (HiExprValue res))
evalHelper (HiExprRun (HiExprRun act)) = do
  res <- evalHelper (HiExprRun act)
  evalHelper (HiExprRun (HiExprValue res))
evalHelper (HiExprRun _) = throwE HiErrorInvalidArgument
evalHelper (HiExprDict dict) = do
  d <- evalDict dict
  return $ HiValueDict $ Data.Map.fromList d
-- nested functions
evalHelper (HiExprApply expr args) = do
  expr1 <- evalHelper expr
  evalHelper $ HiExprApply (HiExprValue expr1) args

-- | apply some binary function.
-- throws HiErrorArityMismatch if number arguments /= 2
applyBiFun :: 
  HiMonad m => 
  (HiValue -> HiValue -> ExceptT HiError m HiValue) ->  -- ^ function
  [HiExpr] ->                                           -- ^ arguments
  ExceptT HiError m HiValue
applyBiFun fun args = do
  expLen args 2
  el <- evalHelper (head args)
  er <- evalHelper (args !! 1)
  res <- fun el er
  evalHelper (HiExprValue res)

-- | short-circuit evaluation or. if in A || B, A is null or false returns B, otherwise returns A, without evaluating B
-- throws HiErrorArityMismatch if number arguments /= 2
applyOr :: 
  HiMonad m =>
  [HiExpr] -> -- ^ arguments
  ExceptT HiError m HiValue
applyOr args = do
  expLen args 2
  first <- evalHelper (head args)
  case first of
    (HiValueBool False) -> do
      evalHelper (args !! 1)
    HiValueNull -> do
      evalHelper (args !! 1)
    _ -> return first

-- | short-circuit evaluation and. if in A && B, A is null or false returns A without evaluating B, otherwise returns B
-- throws HiErrorArityMismatch if number arguments /= 2
applyAnd :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
applyAnd args = do
  expLen args 2
  first <- evalHelper (head args)
  case first of
    (HiValueBool False) -> return first
    HiValueNull -> return first
    _ -> do
      evalHelper (args !! 1)

-- | short-circuit evaluation if. if in "if C then A else B", C is true returns A  without evaluating B, otherwise returns B without evaluating A
-- throws HiErrorArityMismatch if number arguments /= 3
applyIf ::
  HiMonad m => 
  [HiExpr] -> -- ^ arguments
  ExceptT HiError m HiValue
applyIf args = do
  expLen args 3
  cond <- evalHelper (head args)
  case cond of
    (HiValueBool True) -> evalHelper $ args !! 1
    (HiValueBool False) -> evalHelper $ args !! 2
    _ -> throwE HiErrorInvalidArgument

-- | apply some unary function.
-- throws HiErrorArityMismatch if number arguments /= 1
applyOneArgFun ::
 HiMonad m => 
 (HiValue -> ExceptT HiError m HiValue) -> -- ^ unary function
 [HiExpr] -> ExceptT HiError m HiValue
applyOneArgFun fun args = do
  expLen args 1
  res <- evalHelper (head args)
  res2 <- fun res
  evalHelper (HiExprValue res2)

-- | returns element with index of collection if number of argument == 1
-- returns slice of collection if number of argument == 2
-- if number of args /= 1 && /= 2 throws HiErrorArityMismatch
applyIndexOrSlice ::
 HiMonad m =>
 HiValue ->   -- ^ some collection (list, string or bytestring)
 [HiExpr] ->  -- ^ arguments
 ExceptT HiError m HiValue
applyIndexOrSlice str args = do
  if checkLen args 1
    then do
      ind <- evalHelper (head args)
      index' str ind
    else
      if checkLen args 2
        then do
          fr <- evalHelper (head args)
          to <- evalHelper (args !! 1)
          slice' str fr to
        else throwE HiErrorArityMismatch

-- | take any number of arguments and returns list of them
applyList ::
 HiMonad m =>
 [HiExpr] -> -- ^ arguments
 ExceptT HiError m HiValue
applyList args = do
  list <- helper args
  return $ HiValueList list
  where
    -- | helper function for applyList. eval all elements of list
    -- | if an error occurred on the calculation of any element, it throws an error
    helper ::
     HiMonad m => 
     [HiExpr] -> 
     ExceptT HiError m (Seq HiValue)
    helper (x : xs) = do
      res <- evalHelper x
      rest <- helper xs
      return (res <| rest)
    helper [] = return mempty

-- | takes dictionary and key. returns value if pair (key, value) exists in dictionary, null otherwise
-- if number of args /= 1 throws HiErrorArityMismatch
applyField ::
 HiMonad m => 
 HiValue ->   -- ^ dictionary
 [HiExpr] ->  -- ^ field
 ExceptT HiError m HiValue
applyField (HiValueDict dict) args = do
  expLen args 1
  res <- evalHelper (head args)
  case Data.Map.lookup res dict of
    Nothing -> return HiValueNull
    (Just val) -> return val
applyField _ _ = throwE HiErrorInvalidArgument

-- | takes some function and list of values. returns folding of list with function
applyFold ::
 HiMonad m => 
 [HiExpr] -> -- ^ arguments
 ExceptT HiError m HiValue
applyFold args = do
  expLen args 2
  case args of
    [expr, right] -> do
      rig <- evalHelper right
      case rig of
        (HiValueList s) -> do
          let list = Data.Foldable.toList s
          myfold expr list
        _ -> throwE HiErrorInvalidArgument
    _ -> throwE HiErrorInvalidArgument
  where
    -- | fold
    myfold :: 
      HiMonad m => 
      HiExpr ->     -- ^ function
      [HiValue] ->  -- ^ list of values
      ExceptT HiError m HiValue
    myfold _ [] = return HiValueNull
    myfold _ [x] = return x
    myfold expr (x : xs) = helper expr xs x
    -- | function that apply function to accumulated value and first element of list, then repeat operation with new accumulated value and rest of list
    helper ::
     HiMonad m => 
     HiExpr ->    -- ^ function
     [HiValue] -> -- ^ rest of args
     HiValue ->   -- ^ accumulated value
     ExceptT HiError m HiValue
    helper _ [] acc = return acc
    helper expr (x : xs) acc = do
      res <- evalHelper (HiExprApply expr [HiExprValue acc, HiExprValue x])
      helper expr xs res

-- | +
add' :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
add' (HiValueNumber a) (HiValueNumber b) = return $ HiValueNumber (a + b)
add' (HiValueString a) (HiValueString b) = return $ HiValueString (Data.Text.concat [a, b])
add' (HiValueList a) (HiValueList b) = return $ HiValueList (a >< b)
add' (HiValueBytes a) (HiValueBytes b) = return $ HiValueBytes (a <> b)
add' (HiValueTime a) (HiValueNumber b) = do
  i <- getIntFromRational b
  return $ HiValueTime (addUTCTime (fromIntegral i) a)
add' (HiValueNumber a) (HiValueTime b) = add' (HiValueTime b) (HiValueNumber a)
add' _ _ = throwE HiErrorInvalidArgument

-- | *
mul' :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
mul' (HiValueNumber a) (HiValueNumber b) = return $ HiValueNumber (a * b)
mul' (HiValueString a) (HiValueNumber b) = do
  i <- getIntFromRational b
  if i > 0
    then return $ HiValueString $ stimes i a
    else throwE HiErrorInvalidArgument
mul' (HiValueList a) (HiValueNumber b) = do
  i <- getIntFromRational b
  if i > 0
    then return $ HiValueList $ stimes i a
    else throwE HiErrorInvalidArgument
mul' (HiValueBytes a) (HiValueNumber b) = do
  i <- getIntFromRational b
  if i > 0
    then return $ HiValueBytes $ stimes i a
    else throwE HiErrorInvalidArgument
mul' _ _ = throwE HiErrorInvalidArgument

-- | -
sub' :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
sub' (HiValueNumber a) (HiValueNumber b) = return $ HiValueNumber (a - b)
sub' (HiValueTime a) (HiValueTime b) = return $ HiValueNumber (toRational $ diffUTCTime a b)
sub' _ _ = throwE HiErrorInvalidArgument

-- | /
div' :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
div' (HiValueNumber _) (HiValueNumber 0) = throwE HiErrorDivideByZero
div' (HiValueNumber a) (HiValueNumber b) = return $ HiValueNumber (a / b)
div' (HiValueString a) (HiValueString b) = return $ HiValueString (Data.Text.concat [a, "/", b])
div' _ _ = throwE HiErrorInvalidArgument

-- | !
not' :: Monad m => HiValue -> ExceptT HiError m HiValue
not' (HiValueBool a) = return $ HiValueBool (not a)
not' _ = throwE HiErrorInvalidArgument

-- | <
lessThan' :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
lessThan' (HiValueBool a) (HiValueBool b) = return $ HiValueBool (a < b)
lessThan' (HiValueNumber a) (HiValueNumber b) = return $ HiValueBool (a < b)
lessThan' (HiValueFunction a) (HiValueFunction b) = return $ HiValueBool (a < b)
lessThan' a b = return $ HiValueBool (a < b)

-- | >
greaterThan' :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
greaterThan' (HiValueBool a) (HiValueBool b) = return $ HiValueBool (a > b)
greaterThan' (HiValueNumber a) (HiValueNumber b) = return $ HiValueBool (a > b)
greaterThan' (HiValueFunction a) (HiValueFunction b) = return $ HiValueBool (a > b)
greaterThan' a b = return $ HiValueBool (a > b)

-- | ==
equals' :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
equals' (HiValueBool a) (HiValueBool b) = return $ HiValueBool (a == b)
equals' (HiValueNumber a) (HiValueNumber b) = return $ HiValueBool (a == b)
equals' (HiValueFunction a) (HiValueFunction b) = return $ HiValueBool (a == b)
equals' (HiValueString a) (HiValueString b) = return $ HiValueBool (a == b)
equals' HiValueNull HiValueNull = return $ HiValueBool True
equals' (HiValueList a) (HiValueList b) = return $ HiValueBool (a == b)
equals' _ _ = return $ HiValueBool False

-- | >=
notLessThan' :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
notLessThan' a b = do
  cond <- lessThan' a b
  case cond of
    (HiValueBool bool) -> return $ HiValueBool $ not bool
    _ -> throwE HiErrorInvalidArgument

-- | <= 
notGreaterThan' :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
notGreaterThan' a b = do
  cond <- greaterThan' a b
  case cond of
    (HiValueBool bool) -> return $ HiValueBool $ not bool
    _ -> throwE HiErrorInvalidArgument

-- | /=
notEquals' :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
notEquals' a b = do
  cond <- equals' a b
  case cond of
    (HiValueBool bool) -> return $ HiValueBool $ not bool
    _ -> throwE HiErrorInvalidArgument

-- | returns length of list, string or bytestring
length' :: Monad m => HiValue -> ExceptT HiError m HiValue
length' (HiValueString str) = return $ HiValueNumber (toRational $ Data.Text.length str)
length' (HiValueBytes bytes) = return $ HiValueNumber (toRational $ Data.ByteString.length bytes)
length' (HiValueList s) = return $ HiValueNumber (toRational $ Data.Sequence.Internal.length s)
length' _ = throwE HiErrorInvalidArgument

-- | takes some string and returns this string but uppercase
toUpper' :: Monad m => HiValue -> ExceptT HiError m HiValue
toUpper' (HiValueString str) = return $ HiValueString (Data.Text.toUpper str)
toUpper' _ = throwE HiErrorInvalidArgument

-- | takes some string and returns this string but lowercase
toLower' :: Monad m => HiValue -> ExceptT HiError m HiValue
toLower' (HiValueString str) = return $ HiValueString (Data.Text.toLower str)
toLower' _ = throwE HiErrorInvalidArgument

-- | returns reversed list, string or bytestring
reverse' :: Monad m => HiValue -> ExceptT HiError m HiValue
reverse' (HiValueString str) = return $ HiValueString (Data.Text.reverse str)
reverse' (HiValueList s) = return $ HiValueList (Data.Sequence.Internal.reverse s)
reverse' (HiValueBytes bytes) = return $ HiValueBytes (Data.ByteString.reverse bytes)
reverse' _ = throwE HiErrorInvalidArgument

-- | deletes spaces from begin and from end of string
trim' :: Monad m => HiValue -> ExceptT HiError m HiValue
trim' (HiValueString str) = return $ HiValueString (strip str)
trim' _ = throwE HiErrorInvalidArgument

-- | element by index of list, string or bytestring
index' :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
index' val (HiValueNumber i) = do
  ind <- getIntFromRational i
  len' <- length' val
  len <- getNum len'
  if ind >= 0 && ind < len then --
    case val of
      (HiValueString str) -> return $ HiValueString $ Data.Text.cons (Data.Text.index str ind) mempty
      (HiValueList list) -> return $ Data.Sequence.Internal.index list ind
      (HiValueBytes bytes) -> return $ HiValueNumber $ toRational $ Data.ByteString.index bytes ind
      _ -> throwE HiErrorInvalidFunction
  else return HiValueNull
index' _ _ = throwE HiErrorInvalidArgument

-- | slice of list, string or bytestring
slice' :: Monad m => HiValue -> HiValue -> HiValue -> ExceptT HiError m HiValue
slice' val HiValueNull HiValueNull = do
  len <- length' val
  slice' val (HiValueNumber 0) len
slice' val (HiValueNumber fromNum) HiValueNull = do
  len <- length' val
  slice' val (HiValueNumber fromNum) len
slice' val HiValueNull (HiValueNumber toNum) =
  slice' val (HiValueNumber 0) (HiValueNumber toNum)
slice' val (HiValueNumber fromNum) (HiValueNumber toNum) = do
    len' <- length' val
    len <- getNum len'
    from <- getIntFromRational fromNum
    to <- getIntFromRational toNum
    case val of
      (HiValueString str) -> sliceStr len str (indexHelper from len) (indexHelper to len)
      (HiValueList list) -> sliceList len list (indexHelper from len) (indexHelper to len)
      (HiValueBytes bytes) -> sliceBytes len bytes (indexHelper from len) (indexHelper to len)
      _ -> throwE HiErrorInvalidArgument
    where
      indexHelper :: Int -> Int -> Int
      indexHelper a len
        | a < 0 = a + len
        | otherwise = a

slice' _ _ _ = throwE HiErrorInvalidArgument

-- | slice of string
sliceStr :: Monad m => Int -> Text -> Int -> Int -> ExceptT HiError m HiValue
sliceStr len str from to
  | from < 0 = sliceStr len str 0 to
  | from >= to = return $ HiValueString mempty
  | to > len = sliceStr len str from len
  | otherwise = return $ HiValueString $ Data.Text.drop from (Data.Text.dropEnd (len - to) str)

-- | slice of list
sliceList :: Monad m => Int -> Seq HiValue -> Int -> Int -> ExceptT HiError m HiValue
sliceList len s from to
  | from < 0 = sliceList len s 0 to
  | from >= to = return $ HiValueList mempty
  | to > len = sliceList len s from len
  | otherwise = return $ HiValueList $ Data.Sequence.Internal.take (to - from) (Data.Sequence.Internal.drop from s)

-- | slice of bytestring
sliceBytes :: Monad m => Int -> ByteString -> Int -> Int -> ExceptT HiError m HiValue
sliceBytes len bytes from to
  | from < 0 = sliceBytes len bytes 0 to
  | from >= to = return $ HiValueBytes mempty
  | to > len = sliceBytes len bytes from len
  | otherwise = return $ HiValueBytes $ Data.ByteString.take (to - from) (Data.ByteString.drop from bytes)

-- | returns list containing elements from some range
range' :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
range' (HiValueNumber from) (HiValueNumber to) =
  return $ HiValueList $ foldr ((<|) . HiValueNumber) mempty [from .. to]
range' _ _ = throwE HiErrorInvalidArgument

-- | takes list of bytes and returns bytestring of it
packBytes' :: HiMonad m => HiValue -> ExceptT HiError m HiValue
packBytes' (HiValueList s) = do
  res <- myMapExceptT (evalHelper . HiExprValue) (Data.Foldable.toList s)
  bytes <- myMapExceptT getByte res
  return $ HiValueBytes $ Data.ByteString.pack bytes
  where
    getByte :: Monad m => HiValue -> ExceptT HiError m Word8
    getByte (HiValueNumber byte) = do
      i <- getIntFromRational byte
      if i >= 0 && i <= 255
        then return $ fromInteger (toInteger i)
        else throwE HiErrorInvalidArgument
    getByte _ = throwE HiErrorInvalidArgument
packBytes' _ = throwE HiErrorInvalidArgument

-- | takes bytestring and returns list of bytes of it
unpackBytes' :: Monad m => HiValue -> ExceptT HiError m HiValue
unpackBytes' (HiValueBytes bytes) = do
  let s = Data.ByteString.unpack bytes
  let res = map (HiValueNumber . toRational) s
  return $ HiValueList $ Data.Sequence.Internal.fromList res
unpackBytes' _ = throwE HiErrorInvalidArgument

-- | takes string and encoded it. returns encoded bytestring
myEncodeUtf8 :: Monad m => HiValue -> ExceptT HiError m HiValue
myEncodeUtf8 (HiValueString str) = return $ HiValueBytes $ encodeUtf8 str
myEncodeUtf8 _ = throwE HiErrorInvalidArgument

-- | takes bytestring and decoded it. returns decoded string
myDecodeUtf8 :: Monad m => HiValue -> ExceptT HiError m HiValue
myDecodeUtf8 (HiValueBytes bytes) = do
  case decodeUtf8' bytes of
    (Left _) -> return HiValueNull
    (Right txt) -> return $ HiValueString txt
myDecodeUtf8 _ = throwE HiErrorInvalidArgument

-- | zip bytestring
zip' :: Monad m => HiValue -> ExceptT HiError m HiValue
zip' (HiValueBytes bytes) =
  let a = compressWith (defaultCompressParams {compressLevel = bestCompression}) (fromStrict bytes)
   in return $ HiValueBytes (toStrict a)
zip' _ = throwE HiErrorInvalidArgument

-- | unzip bytestring
unzip' :: Monad m => HiValue -> ExceptT HiError m HiValue
unzip' (HiValueBytes bytes) =
  let a = decompress (fromStrict bytes)
   in return $ HiValueBytes (toStrict a)
unzip' _ = throwE HiErrorInvalidArgument

-- | serialise any value. returns bytestring
serialise' :: Monad m => HiValue -> ExceptT HiError m HiValue
serialise' value = return $ HiValueBytes $ toStrict $ serialise value

-- | deserialise bytestring
deserialise' :: Monad m => HiValue -> ExceptT HiError m HiValue
deserialise' (HiValueBytes bytes) = do
  let res = deserialiseOrFail (fromStrict bytes)
  case res of
    (Left _) -> throwE HiErrorInvalidArgument
    (Right val) -> return val
--return $ deserialise (fromStrict bytes)
deserialise' _ = throwE HiErrorInvalidArgument

-- | returns HiActionRead
read' :: Monad m => HiValue -> ExceptT HiError m HiValue
read' (HiValueString path) = return $ HiValueAction $ HiActionRead (Data.Text.unpack path)
read' _ = throwE HiErrorInvalidArgument

-- | returns HiActionWrite
write' :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
write' (HiValueString path) (HiValueString txt) = return $ HiValueAction $ HiActionWrite (Data.Text.unpack path) (encodeUtf8 txt)
--write' (HiValueString path) (HiValueBytes bytes) = return $ HiValueAction $ HiActionWrite (Data.Text.unpack path) bytes
write' _ _ = throwE HiErrorInvalidArgument

-- | returns HiActionMkDir
mkdir' :: Monad m => HiValue -> ExceptT HiError m HiValue
mkdir' (HiValueString path) = return $ HiValueAction $ HiActionMkDir (Data.Text.unpack path)
mkdir' _ = throwE HiErrorInvalidArgument

-- | returns HiActionChDir
chdir' :: Monad m => HiValue -> ExceptT HiError m HiValue
chdir' (HiValueString path) = return $ HiValueAction $ HiActionChDir (Data.Text.unpack path)
chdir' _ = throwE HiErrorInvalidArgument

-- | take string and try to parse time from it
parseTime' :: Monad m => HiValue -> ExceptT HiError m HiValue
parseTime' (HiValueString time) = do
  let parsed = readMaybe (Data.Text.unpack time) :: Maybe UTCTime
  case parsed of
    Nothing -> return HiValueNull
    (Just utc) -> return $ HiValueTime utc
parseTime' _ = throwE HiErrorInvalidArgument

-- | returns HiActionRand
rand' :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
rand' (HiValueNumber a) (HiValueNumber b) = do
  l <- getIntFromRational a
  r <- getIntFromRational b
  return $ HiValueAction $ HiActionRand l r
rand' _ _ = throwE HiErrorInvalidArgument

-- | returns HiActionEcho
echo' :: Monad m => HiValue -> ExceptT HiError m HiValue
echo' (HiValueString str) = return $ HiValueAction $ HiActionEcho str
echo' _ = throwE HiErrorInvalidArgument

-- | returns list of keys of dictionary
keys' :: Monad m => HiValue -> ExceptT HiError m HiValue
keys' (HiValueDict dict) = return $ HiValueList $ Data.Sequence.Internal.fromList $ keys dict
keys' _ = throwE HiErrorInvalidArgument

-- | returns list of values of dictionary
values' :: Monad m => HiValue -> ExceptT HiError m HiValue
values' (HiValueDict dict) = return $ HiValueList $ Data.Sequence.Internal.fromList $ elems dict
values' _ = throwE HiErrorInvalidArgument

-- | take collection and returns dictionary, where which key is element of collection and value is amount of entries of it
count' :: Monad m => HiValue -> ExceptT HiError m HiValue
count' (HiValueString s) = countHelper (toCharList (Data.Text.unpack s) []) mempty
count' (HiValueList l) = countHelper (Data.Foldable.toList l) mempty
count' (HiValueBytes b) = do
  list <- unpackBytes' (HiValueBytes b)
  count' list
count' _ = throwE HiErrorInvalidArgument

countHelper :: Monad m => [HiValue] -> Map HiValue HiValue -> ExceptT HiError m HiValue
countHelper [] acc = return $ HiValueDict acc
countHelper (c : cs) acc =
  case Data.Map.lookup c acc of
    Nothing -> do countHelper cs (insert c (HiValueNumber 1) acc)
    (Just (HiValueNumber n)) -> do
      cnt <- getIntFromRational n
      countHelper cs (insert c (HiValueNumber $ fromIntegral (cnt + 1)) acc)
    _ -> throwE HiErrorInvalidArgument

-- | takes dictionary and returns its inverted version
invert' :: Monad m => HiValue -> ExceptT HiError m HiValue
invert' (HiValueDict dict) = return $ HiValueDict $ helper (assocs dict) mempty
  where
    helper [] acc = acc
    helper ((k, v) : ms) acc =
      case Data.Map.lookup v acc of
        Nothing -> helper ms (insert v (HiValueList $ Data.Sequence.Internal.fromList [k]) acc)
        (Just (HiValueList list)) -> helper ms (insert v (HiValueList (k <| list)) acc)
invert' _ = throwE HiErrorInvalidArgument

-----------------------------------------------------------------------------
-- | takes string ang returns list of strings where each string is one char
-- "text" => ["t", "e", "x", "t"]
toCharList :: String -> [HiValue] -> [HiValue]
toCharList "" acc = acc
toCharList (c : cs) acc = toCharList cs (HiValueString (Data.Text.pack [c]) : acc)

-- | tries to get integer from rational number
getIntFromRational :: Monad m => Rational -> ExceptT HiError m Int
getIntFromRational rat = do
  let (sci, _) = fromRationalRepetendUnlimited rat
  if isInteger sci
    then case (toBoundedInteger sci :: Maybe Int) of
      (Just i) -> return i
      Nothing -> throwE HiErrorInvalidArgument
    else throwE HiErrorInvalidArgument

-- | check if list length satisfy to expected number
expLen :: Monad m => [a] -> Natural -> ExceptT HiError m ()
expLen args n = 
  if checkLen args n then
    return ()
  else 
    throwE HiErrorArityMismatch
    
-- | check if list length satisfy to expected number
checkLen :: [a] -> Natural -> Bool
checkLen [] 0 = True
checkLen [] _ = False
checkLen _ 0 = False
checkLen (_ : xs) n = checkLen xs (n - 1)

-- | apply function to list of values
myMapExceptT :: Monad m => (HiValue -> ExceptT HiError m a) -> [HiValue] -> ExceptT HiError m [a]
myMapExceptT _ [] = return []
myMapExceptT fun (x : xs) = do
  res <- fun x
  rest <- myMapExceptT fun xs
  return $ res : rest

-- | evaluates dictionary
evalDict :: HiMonad m => [(HiExpr, HiExpr)] -> ExceptT HiError m [(HiValue, HiValue)]
evalDict [] = return []
evalDict ((key, val) : xs) = do
  k <- evalHelper key
  v <- evalHelper val
  rest <- evalDict xs
  return $ (k, v) : rest

-- | tries to get integer number from (HiValueNumber n)
getNum :: Monad m => HiValue -> ExceptT HiError m Int
getNum (HiValueNumber n) = getIntFromRational n
getNum _ = throwE HiErrorInvalidArgument