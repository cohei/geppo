module Main where

import Project (toMarkdown)
import Request (request)
import Setting (getSettingFromEnv)
import Time    (getLocalToday)

main :: IO ()
main = do
  setting <- getSettingFromEnv
  today <- getLocalToday
  projects <- request today setting
  putStr $ toMarkdown projects
