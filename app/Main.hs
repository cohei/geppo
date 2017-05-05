module Main where

import Project (extractProjects, toMarkdown)
import Request (request)
import Setting (getSettingFromEnv)

main :: IO ()
main = do
  setting <- getSettingFromEnv
  response <- request setting
  putStr $ toMarkdown $ extractProjects response
