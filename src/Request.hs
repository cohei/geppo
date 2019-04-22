{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Request (request)  where

import           Control.Exception      (throwIO)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (Value)
import           Data.ByteString.Char8  (pack)
import           Data.List              (intercalate)
import           Data.Monoid            ((<>))
import           Data.Time              (Day)
import           Network.HTTP.Req

import           Project                (Project, extractProjects)
import           Setting                (Setting (..))
import           Time                   (YearMonth, beginningOfMonth,
                                         endOfMonth)

reportsApiBaseUrl :: Url 'Https
reportsApiBaseUrl = https "toggl.com" /: "reports" /: "api" /: "v2"

instance MonadHttp IO where
  handleHttpException = throwIO

request :: MonadHttp m => YearMonth -> Setting -> m [Project]
request ym setting =
  extractProjects . responseBody <$> req GET (reportsApiBaseUrl /: "summary") NoReqBody jsonResponse (options ym setting)

userAgent :: String
userAgent = "geppo"

options :: YearMonth -> Setting -> Option 'Https
options ym setting = mconcat
  [ basicAuth (pack $ apiToken setting) "api_token"
  , "user_agent"   =: userAgent
  , "workspace_id" =: workspaceId setting
  , "since"        =: beginningOfMonth ym
  , "until"        =: endOfMonth ym
  , "client_ids"   =: intercalate "," (clientIds setting)
  , "order_field"  =: ("duration" :: String)
  , "order_desc"   =: ("on" :: String)
  ]
