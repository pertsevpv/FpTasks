module Main where

import System.Console.Haskeline
import HW3.Base
import HW3.Parser
import HW3.Evaluator
import HW3.Pretty
import HW3.Action
import Control.Monad.IO.Class (liftIO)
import Data.Set.Internal(fromList)

main :: IO ()
main = runInputT defaultSettings loop
 where
 loop :: InputT IO ()
 loop = do
  minput <- getInputLine "hi> "
  case minput of
    Nothing -> return ()
    Just ":q" -> return ()
    Just input -> do
      let parsed = parse input
      case parsed of
        (Left e) -> outputStrLn (show e)
        (Right expr) -> do
          let val = eval expr :: HIO (Either HiError HiValue)
          value <- liftIO $ runHIO val (fromList [AllowRead, AllowWrite, AllowTime])
          case value of
            (Left e) -> outputStrLn (show e)
            (Right r) -> outputStrLn (show $ prettyValue r)
      loop