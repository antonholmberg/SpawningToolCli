{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Api                            (fetch, buildsForPage)
import           Control.Monad.State.Lazy       ( runStateT )
import           Control.Monad.Reader           ( runReaderT )

main :: IO ()
main = do
  page <- fetch $ buildsForPage 0
  putStrLn $ show page
