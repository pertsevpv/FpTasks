module HW2.T5 where

import qualified Control.Monad
import           HW2.T1
import           HW2.T4        (Expr (..), Prim (..), getConstructor, getOp,
                                leftOp, rightOp)

data ExceptState e s a
  = ES {runES :: s -> Except e (Annotated s a)}

-- | map function to ExceptState if Success and if error throws Error e
mapExceptState ::
 (a -> b) ->          -- ^ map function 
 ExceptState e s a -> -- ^ ExceptState to map
 ExceptState e s b
mapExceptState f (ES st) = ES helper
  where
    helper s = case st s of
      (Success (a :# ss)) -> Success (f a :# ss)
      Error e             -> Error e

-- | wrap some value in ExceptState
wrapExceptState :: 
  a ->              -- ^ some value
  ExceptState e s a
wrapExceptState a = ES (\s -> Success (a :# s))

-- | joins nested ExceptStates
joinExceptState :: 
  ExceptState e s (ExceptState e s a) -> -- ^ nested ExceptStates
  ExceptState e s a
joinExceptState (ES st) = ES helper
  where
    helper i = case st i of
      (Success ((ES r) :# ss)) -> r ss
      (Error e)                -> Error e

-- | modifies ExceptState
modifyExceptState :: 
  (s -> s) ->       -- ^ modifying function
  ExceptState e s ()
modifyExceptState f = ES (\x -> Success (() :# f x))

-- | always returns ExceptState with Error
throwExceptState :: 
  e -> 
  ExceptState e s a
throwExceptState e = ES (\_ -> Error e)

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero

-- | it's like eval from HW2.T4, but this one returns ExceptState instead of State. If division vy zero occurs, it returns Error.
eval :: 
  Expr -> 
  ExceptState EvaluationError [Prim Double] Double
eval (Op (Abs x)) = do
  l <- eval x
  modifyExceptState (Abs l :)
  pure (abs l)
eval (Op (Sgn x)) = do
  l <- eval x
  modifyExceptState (Sgn l :)
  pure (signum l)
eval (Op op) =
  do
    l <- eval (leftOp op)
    r <- eval (rightOp op)
    let res = getOp op l r
    modifyExceptState (getConstructor op l r :)
    if isNaN res || isInfinite res
      then throwExceptState DivideByZero
      else pure res
eval (Val v) = wrapExceptState v
