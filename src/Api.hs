{-# LANGUAGE OverloadedStrings #-}

module Api
  ( Config
  , fetchBuild
  , nextPage
  , defaultConfig
  , fetchWithConfig
  , fetch
  , buildsForPage
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (ReaderT, ask, runReaderT)
import           Control.Monad.State.Lazy   (StateT, evalStateT, get, put)
import qualified Data.ByteString.Lazy.Char8 as S8
import           Network.HTTP.Simple

data Config =
  Config
    { baseUrl        :: String
    , buildsEndpoint :: String
    }

defaultConfig :: Config
defaultConfig =
  Config {baseUrl = "https://lotv.spawningtool.com", buildsEndpoint = "/build"}

buildsForPage :: Int -> ReaderT Config IO S8.ByteString
buildsForPage = evalStateT fetchBuild

fetch = fetchWithConfig defaultConfig

fetchWithConfig :: Config -> ReaderT Config IO S8.ByteString -> IO S8.ByteString
fetchWithConfig config m = runReaderT m config

fetchBuild :: StateT Int (ReaderT Config IO) S8.ByteString
fetchBuild = do
  config <- ask
  page <- get
  let url = baseUrl config ++ buildsEndpoint config ++ "?p=" ++ show page
  request <- parseRequest url
  response <- liftIO $ httpLBS request
  let html = getResponseBody response
  return html

incrementPage :: (Monad m) => StateT Int m ()
incrementPage = get >>= put . (+ 1)

nextPage :: StateT Int (ReaderT Config IO) S8.ByteString
nextPage = do
  incrementPage
  fetchBuild
