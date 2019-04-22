module Setting (Setting(..), getSettingFromEnv) where

import           Control.Arrow      (second)
import           Data.ByteString    (ByteString)
import           Data.List          (unfoldr)
import           System.Environment (getEnv)

data Setting =
  Setting
  { apiToken    :: String
  , workspaceId :: String
  , clientIds   :: [String]
  } deriving Show

getSettingFromEnv :: IO Setting
getSettingFromEnv = Setting <$> getApiToken <*> getWorkspaceId <*> getClientIds

getApiToken :: IO String
getApiToken = getEnv "GEPPO_API_TOKEN"

getWorkspaceId :: IO String
getWorkspaceId = getEnv "GEPPO_WORKSPACE_ID"

getClientIds :: IO [String]
getClientIds = split ',' <$> getEnv "GEPPO_CLIENT_IDS"

split :: Eq a => a -> [a] -> [[a]]
split x = unfoldr $ \xs -> if null xs then Nothing else Just $ second (drop 1) $ break (x ==) xs
