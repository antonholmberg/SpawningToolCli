module Main where

import           Api                      (buildsForPage, fetch)
import           Control.Monad.Reader     (runReaderT)
import           Control.Monad.State.Lazy (runStateT)

main :: IO ()
main = do
  page <- fetch $ buildsForPage 0
  print page
