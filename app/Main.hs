module Main where

import Pandoc  (toMarkdown, template)
import Request (request)
import Setting (getSettingFromEnv)
import Time    (getLocalToday, lastMonth, toYearMonth)

main :: IO ()
main = do
  setting <- getSettingFromEnv
  ym <- toYearMonth . lastMonth <$> getLocalToday
  projects <- request ym setting
  putStr $ toMarkdown $ template ym projects
