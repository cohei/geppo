module Main (main) where

import           Control.Exception (throwIO)
import qualified Data.Text.IO      as T (putStr)

import           Pandoc            (template, toMarkdown)
import           Request           (request)
import           Setting           (getSettingFromEnv)
import           Time              (getLocalToday, lastMonth, toYearMonth)

main :: IO ()
main = do
  setting <- getSettingFromEnv
  ym <- toYearMonth . lastMonth <$> getLocalToday
  projects <- request ym setting
  either throwIO T.putStr $ toMarkdown $ template ym projects
