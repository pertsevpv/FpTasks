module HW2.T2 where

import           HW2.T1

-- | All functions below take (F a, F b) as 1st argument and return F (a, b)

distOption ::
 (Option a, Option b) ->
 Option (a, b)
distOption (Some a, Some b) = Some (a, b)
distOption (_, None)        = None
distOption (None, _)        = None

distPair ::
 (Pair a, Pair b) ->
 Pair (a, b)
distPair (P a b, P c d) = P (a, c) (b, d)

distQuad ::
 (Quad a, Quad b) ->
 Quad (a, b)
distQuad (Q a1 a2 a3 a4, Q b1 b2 b3 b4) = Q (a1, b1) (a2, b2) (a3, b3) (a4, b4)

distAnnotated ::
 Semigroup e => (Annotated e a, Annotated e b) ->
 Annotated e (a, b)
distAnnotated (a :# ae, b :# be) = (a, b) :# (ae <> be)

distExcept ::
 (Except e a, Except e b) ->
 Except e (a, b)
distExcept (Success a, Success b) = Success (a, b)
distExcept (Error e, _)           = Error e
distExcept (_, Error e)           = Error e

distPrioritised ::
 (Prioritised a, Prioritised b) ->
 Prioritised (a, b)
distPrioritised (High a, High b)     = High (a, b)
distPrioritised (High a, Medium b)   = High (a, b)
distPrioritised (High a, Low b)      = High (a, b)
distPrioritised (Medium a, High b)   = High (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (Medium a, Low b)    = Medium (a, b)
distPrioritised (Low a, High b)      = High (a, b)
distPrioritised (Low a, Medium b)    = Medium (a, b)
distPrioritised (Low a, Low b)       = Low (a, b)

distStream ::
 (Stream a, Stream b) ->
 Stream (a, b)
distStream (a :> as, b :> bs) = (a, b) :> distStream (as, bs)


-- | from two Lists makes one, which is their Cartesian product
distList ::
 (List a, List b) ->
 List (a, b)
distList (a, b) = mkList (a, b) b

-- | helper function for distList
mkList ::
 (List a, List b) ->  -- ^ rest of List a and rest of List b
 List b ->            -- ^ full List b
 List (a, b)
mkList (Nil, Nil) _          = Nil
mkList (Nil, _) _            = Nil
mkList (a :. Nil, b :. bs) s = (a, b) :. mkList (a :. Nil, bs) s
mkList (_ :. as, Nil) s      = mkList (as, s) s
mkList (a :. as, b :. bs) s  = (a, b) :. mkList (a :. as, bs) s

distFun ::
 (Fun i a, Fun i b) ->
 Fun i (a, b)
distFun (F f, F g) = F (\i -> (f i, g i))

-- | all functions below takes one value and return a value wrapped in a certain type F

wrapOption ::
 a ->
 Option a
wrapOption = Some

wrapPair ::
 a ->
 Pair a
wrapPair a = P a a

wrapQuad ::
 a ->
 Quad a
wrapQuad a = Q a a a a

wrapAnnotated ::
 Monoid e => a ->
 Annotated e a
wrapAnnotated a = a :# mempty

wrapExcept ::
 a ->
 Except e a
wrapExcept = Success

wrapPrioritised ::
 a ->
 Prioritised a
wrapPrioritised = Low

wrapStream ::
 a ->
 Stream a
wrapStream a =
  let st = a :> st
   in st

wrapList ::
 a ->
 List a
wrapList a = a :. Nil

wrapFun ::
 a ->
 Fun i a
wrapFun a = F (const a)
