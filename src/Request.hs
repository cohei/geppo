{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Request (request)  where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import Data.ByteString.Char8 (pack)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Time (Day)
import Network.HTTP.Req

import Time (getLocalToday, lastMonth, beginningOfMonth, endOfMonth)
import Setting (Setting(..))

reportsApiBaseUrl :: Url 'Https
reportsApiBaseUrl = https "toggl.com" /: "reports" /: "api" /: "v2"

instance MonadHttp IO where
  handleHttpException = throwIO

request :: MonadHttp m => Setting -> m Value
request setting = do
  today <- liftIO getLocalToday
  response <- req GET (reportsApiBaseUrl /: "summary") NoReqBody jsonResponse (query setting today <> auth (apiToken setting))
  return $ responseBody response

userAgent :: String
userAgent = "geppo"

query :: Setting -> Day -> Option 'Https
query setting today = mconcat
  [ "user_agent"   =: userAgent
  , "workspace_id" =: workspaceId setting
  , "since"        =: beginningOfMonth (lastMonth today)
  , "until"        =: endOfMonth (lastMonth today)
  , "client_ids"   =: intercalate "," (clientIds setting)
  , "order_field"  =: ("duration" :: String)
  , "order_desc"   =: ("on" :: String)
  ]

auth :: String -> Option 'Https
auth apiToken = basicAuth (pack apiToken) "api_token"
