module Main where

import Project (toMarkdown)
import Request (request)
import Setting (getSettingFromEnv)
import Time    (getLocalToday, lastMonth, toYearMonth)

main :: IO ()
main = do
  setting <- getSettingFromEnv
  ym <- toYearMonth . lastMonth <$> getLocalToday
  projects <- request ym setting
  putStr $ toMarkdown projects
