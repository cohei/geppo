module Pandoc (toMarkdown, template) where

import           Data.Default                 (def)
import           Data.Text                    (unpack)
import           Text.Pandoc.Definition       (Attr, Block (BulletList, Header, HorizontalRule, Plain),
                                               Inline (Link, Str),
                                               Pandoc (Pandoc), nullMeta)
import           Text.Pandoc.Writers.Markdown (writeMarkdown)

import           Project                      (Project (entries, title))
import           Time                         (YearMonth (month, year))

toMarkdown :: Pandoc -> String
toMarkdown = writeMarkdown def

template :: YearMonth -> [Project] -> Pandoc
template ym projects =
  Pandoc nullMeta $
    [ header 1 $ show (year ym) ++ "年" ++ show (month ym) ++ "月月報"
    , header 2 "今月の実績"
    ] ++
    concatMap fromProject projects ++
    [ header 2 "来月の予定" ] ++
    map (head . fromProject) projects ++
    [ header 2 "その他"
    , HorizontalRule
    , Plain [Str "Generated by ", Link nullAttr [Str "geppo"] ("https://github.com/cohei/geppo", "")]
    ]

fromProject :: Project -> [Block]
fromProject project =
  [ header 3 $ unpack $ title project
  , BulletList $ map (\entry -> [Plain [Str $ unpack entry]]) $ entries project
  ]

header :: Int -> String -> Block
header n s = Header n nullAttr [Str s]

nullAttr :: Attr
nullAttr = ("", [], [])
