module Main where

import Project (extractProjects, toMarkdown)
import Request (request)
import Setting (getSettingFromEnv)
import Time    (getLocalToday)

main :: IO ()
main = do
  setting <- getSettingFromEnv
  today <- getLocalToday
  response <- request today setting
  putStr $ toMarkdown $ extractProjects response
