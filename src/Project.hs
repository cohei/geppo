{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Project (Project(title, entries), extractProjects) where

import Control.Lens ((^..), to, (^?!))
import Data.Aeson (Value)
import Data.Aeson.Lens (key, _Array, _String)
import Data.Text (Text)

data Project = Project { title :: Text, entries :: [Entry] }
type Entry = Text

extractProjects :: Value -> [Project]
extractProjects response =
  response ^..
  key "data" .
  _Array .
  traverse .
  to (\o ->
        (o ^?! key "title" . key "project" . _String,
         o ^.. key "items" . _Array . traverse . key "title" . key "time_entry" . _String)) .
  to (uncurry Project)
