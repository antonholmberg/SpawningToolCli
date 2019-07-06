{-# LANGUAGE OverloadedStrings #-}
module Api
  ( Config
  , fetchBuild
  , nextPage
  , initialBuildsState
  , defaultConfig
  )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.State.Lazy       ( StateT
                                                , get
                                                , put
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , ask
                                                )
import           Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8    as S8

data Config = Config
  { baseUrl :: String
  , buildsEndpoint :: String
  }

defaultConfig :: Config
defaultConfig = Config { baseUrl        = "https://lotv.spawningtool.com"
                       , buildsEndpoint = "/build"
                       }

initialBuildsState :: (Int, ())
initialBuildsState = (0, ())

fetchBuild :: StateT Int (ReaderT Config IO) S8.ByteString
fetchBuild = do
  config <- ask
  page   <- get
  let url = (baseUrl config) ++ (buildsEndpoint config) ++ "?p=" ++ (show page)
  request  <- parseRequest url
  response <- liftIO $ httpLBS request
  html     <- return $ getResponseBody response
  return html

incrementPage :: (Monad m) => StateT Int m ()
incrementPage = get >>= put . (+ 1)

nextPage :: StateT Int (ReaderT Config IO) S8.ByteString
nextPage = do
  incrementPage
  fetchBuild
