{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Project (extractProjects, toMarkdown) where

import Control.Lens ((^..), to, (^?!))
import Data.Aeson (Value)
import Data.Aeson.Lens (key, _Array, _String)
import Data.Default (def)
import Data.Monoid (mconcat, (<>))
import Data.Text (Text, unpack)
import qualified Data.Text as T (unlines)
import qualified Data.Text.IO as T (putStr)
import Text.Pandoc.Definition (Pandoc(Pandoc), Block(Header, BulletList, Plain), Inline(Str), nullMeta)
import Text.Pandoc.Writers.Markdown (writeMarkdown)

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

toMarkdown :: [Project] -> String
toMarkdown = writeMarkdown def . Pandoc nullMeta . concatMap projectToMarkdown

projectToMarkdown :: Project -> [Block]
projectToMarkdown Project {..} =
  [ Header 2 ("", [], []) [Str (unpack title)]
  , BulletList $ map (\entry -> [Plain [Str $ unpack entry]]) entries
  ]
