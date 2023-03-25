{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad ((>>), (>>=), mapM)
import Data.Attoparsec.Text
import Data.Char (isSpace)
import Data.List (sortBy, group, sort, foldr, foldr1, map, concat, head, (++))
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Prelude (Eq, Ord, IO, Either(..), (==), compare, ($), return, (/=), (<$>), (.))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Html = Html [Section]
data Section = Section { name :: T.Text , items :: [Item] }
data Item = Item { url :: T.Text , text :: T.Text } deriving (Eq, Ord)

instance Eq Section where
    (Section a _) == (Section b _) = a == b

instance Ord Section where
    compare (Section a _) (Section b _) = compare a b

skipStrSpace s = asciiCI s >> skipSpace 

parseItem = do
    mapM skipStrSpace [ "<li>", "<a", "href=\""]
    url <- takeWhile (/= '"')
    skipStrSpace "\">"
    text <- takeWhile (/= '<')
    skipStrSpace "</a>"
    return $ Item url text

parseSection = do
    skipStrSpace "<h2>"
    name <- takeWhile (/= '<')
    mapM skipStrSpace [ "</h2>", "<ul>" ]
    items <- many' parseItem
    mapM skipStrSpace
        [ "<!--End of section (do not delete this comment)-->" , "</ul>" ]
    return $ Section name $ sort items

parseHtml = do
    mapM skipStrSpace
        [ "<html>" , "<head>" , "<title>Bookmarks</title>" , "</head>"
        , "<body>" , "<h1>Bookmarks</h1>"
        ]
    sections <- many' parseSection
    mapM skipStrSpace [ "</body>", "</html>" ]
    return $ Html $ sort sections

parseBookmarks = many1 parseHtml

buildItemText (Item url text)
    = fromText "<li><a href=\""
    <> fromText url
    <> fromText "\">"
    <> fromText text
    <> fromText "</a>\n"

buildSectionText (Section name items)
    = fromText "<h2>"
    <> fromText name
    <> fromText "</h2>\n<ul>\n"
    <> (foldr (<>) (fromLazyText "") $ map buildItemText items)
    <> fromText "<!--End of section (do not delete this comment)-->\n</ul>\n"

buildHtmlText (Html sections)
    = fromText "<html><head><title>Bookmarks</title></head>\n<body>\n<h1>Bookmarks</h1>\n"
    <> (foldr (<>) (fromLazyText "") $ map buildSectionText sections)
    <> fromText "</body>\n</html>\n"

getOutput contents = case parseOnly parseBookmarks contents of
        (Left err) -> T.pack $ concat [ "Error parsing: ", err, "\n"]
        (Right htmls) -> toStrict $ toLazyText $ buildHtmlText $ foldr1 joinBookmarks htmls

mergeSections :: Section -> Section -> Section
mergeSections (Section n ia) (Section _ ib) =
    Section n $ map head $ Data.List.group $ sort $ ia ++ ib 

joinBookmarks :: Html -> Html -> Html
joinBookmarks (Html sa) (Html sb) =
    Html $ map (foldr1 mergeSections) $ group $ sort $ sa ++ sb 

main :: IO ()
main = getOutput <$> TIO.getContents >>= TIO.putStr
