{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Api

import           Control.Monad.State.Lazy       ( runStateT )
import           Control.Monad.Reader           ( runReaderT )

main :: IO ()
main = do
  (pageData, _) <- runReaderT (runStateT fetchBuild 0) defaultConfig
  putStrLn $ show pageData
