module HW0.T6 where

import HW0.T1 (distrib)
import Data.Char (isSpace)

a ::
 (Either [Char] b, Either [Char] c)
a = distrib (Left ("AB" ++ "CD" ++ "EF")) 

b ::
 [Bool]
b = map isSpace "Hello, World"

c :: 
  [Char]
c = if 1 > 0 || error "X" then "Y" else "Z"

a_whnf :: 
  (Either [Char] b, Either [Char] c)
a_whnf  = (Left ("AB" ++ "CD" ++ "EF"), Left ("AB" ++ "CD" ++ "EF")) -- Pair constructor

b_whnf :: 
  [Bool]
b_whnf = isSpace 'H' : (map isSpace "ello, World") -- List constructor

c_whnf :: 
  [Char]
c_whnf = "Y"  -- just a string