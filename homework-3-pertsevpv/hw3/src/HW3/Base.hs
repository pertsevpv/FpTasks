{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module HW3.Base where

import Data.Text (Text)
import Data.Time (UTCTime(..))
import Data.Sequence.Internal (Seq)
import Data.ByteString (ByteString)
import Codec.Serialise
import GHC.Generics (Generic)
import Data.Map.Internal (Map)

-- | functions names
data HiFun
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert 
  deriving (Show, Eq, Ord, Enum, Generic, Serialise) -- function names (e.g. div, sort, length, ...)

-- | all possible values
data HiValue
  = HiValueNull                       -- ^ null
  | HiValueBool Bool                  -- ^ bool
  | HiValueFunction HiFun             -- ^ function
  | HiValueString Text                -- ^ string
  | HiValueNumber Rational            -- ^ num
  | HiValueList (Seq HiValue)         -- ^ list
  | HiValueBytes ByteString           -- ^ bytestring
  | HiValueAction HiAction            -- ^ action
  | HiValueTime UTCTime               -- ^ time
  | HiValueDict (Map HiValue HiValue) -- ^ map
  deriving (Show, Eq, Ord, Generic, Serialise) -- values (numbers, booleans, strings, ...)

-- | expressions
data HiExpr
  = HiExprValue HiValue           -- ^ just a value
  | HiExprApply HiExpr [HiExpr]   -- ^ expression applying to list of arguments
  | HiExprRun HiExpr              -- ^ action run
  | HiExprDict [(HiExpr, HiExpr)] -- ^ dictionary
  deriving (Show) -- expressions (literals, function calls, ...)

-- | errors
data HiError
  = HiErrorInvalidArgument  -- ^ unexpected argument
  | HiErrorInvalidFunction  -- ^ unexpected function
  | HiErrorArityMismatch    -- ^ wrong number of arguments
  | HiErrorDivideByZero     -- ^ division by zero
  deriving (Show) -- evaluation errors (invalid arguments, ...)

-- | actions that can be performed using '!'
data HiAction 
  = HiActionRead  FilePath              -- ^ read from file or from directory
  | HiActionWrite FilePath ByteString   -- ^ write to file
  | HiActionMkDir FilePath              -- ^ create new directory
  | HiActionChDir FilePath              -- ^ choose directory
  | HiActionCwd                         -- ^ get current directory
  | HiActionNow                         -- ^ get current time
  | HiActionRand Int Int                -- ^ get random integer from range
  | HiActionEcho Text                   -- ^ print text to console
  deriving (Show, Eq, Ord, Generic, Serialise)
  
class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
