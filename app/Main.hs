module Main where

import           Api                      (buildsForPage, fetch)
import           Control.Monad.Reader     (runReaderT)
import           Control.Monad.State.Lazy (runStateT)
import           System.Console.ArgParser (CmdLnInterface, mkDefaultApp,
                                           mkSubParser, optPos, parsedBy,
                                           runApp)

data Command =
  GetBuilds Int
  deriving (Eq, Show)

parser :: IO (CmdLnInterface Command)
parser =
  mkSubParser
    [("builds", mkDefaultApp (GetBuilds `parsedBy` optPos 0 "page") "builds")]

runCommand :: Command -> IO ()
runCommand (GetBuilds i) = print =<< builds i
  where
    builds = fetch . buildsForPage

main :: IO ()
main = parser >>= flip runApp runCommand
