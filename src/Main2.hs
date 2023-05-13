
module Main(main) where

import qualified Action.Tmp
import System.Environment (getArgs)


main :: IO ()
main = do
  [fn] <- getArgs
  Action.Tmp.parsePrint fn
