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
import Prelude (Eq, Ord, IO, Either(..), (==), compare, ($), return, (/=), (<$>), (.), (*>), (<*), (<*>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Html = Html [Section]
data Section = Section { name :: T.Text , items :: [Item] }
data Item = Item { url :: T.Text , text :: T.Text } deriving (Eq, Ord)

instance Eq Section where
    (Section a _) == (Section b _) = a == b

instance Ord Section where
    compare (Section a _) (Section b _) = compare a b

skipStrSpace s = asciiCI s <* skipSpace 
skipHead = skipStrSpace "<head>"
         *> skipStrSpace "<title>Bookmarks</title>"
         *> skipStrSpace "</head>"

parseAnchorUrl = skipStrSpace "<a"
               *> skipStrSpace "href=\""
               *> takeWhile (/= '"')
               <* skipStrSpace "\">"

parseH2 = skipStrSpace "<h2>" *> takeWhile (/= '<') <* skipStrSpace "</h2>"
parseUl = skipStrSpace "<ul>" 
        *> (sort <$> many' parseItem)
        <* skipStrSpace "<!--End of section (do not delete this comment)-->" 
        <* skipStrSpace "</ul>"

parseItem = Item
         <$> (skipStrSpace "<li>" *> parseAnchorUrl)
         <*> (takeWhile (/= '<') <* skipStrSpace "</a>")

parseSection = Section <$> parseH2 <*> parseUl

parseHtml = Html
         <$> (skipStrSpace "<html>" 
              *> skipHead 
              *> skipStrSpace "<body>"
              *> skipStrSpace "<h1>Bookmarks</h1>"
              *> (sort <$> many' parseSection)
              <* skipStrSpace "</body>"
              <* skipStrSpace "</html>")

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
    = fromText "<html><head><title>Bookmarks</title></head>\n\
               \<body>\n<h1>Bookmarks</h1>\n"
    <> (foldr (<>) (fromLazyText "") $ map buildSectionText sections)
    <> fromText "</body>\n</html>\n"

getOutput contents = case parseOnly parseBookmarks contents of
        (Left err) -> T.pack $ concat [ "Error parsing: ", err, "\n"]
        (Right htmls) -> toStrict
                       $ toLazyText
                       $ buildHtmlText
                       $ foldr1 joinBookmarks htmls

mergeSections :: Section -> Section -> Section
mergeSections (Section n ia) (Section _ ib) =
    Section n $ map head $ Data.List.group $ sort $ ia ++ ib 

joinBookmarks :: Html -> Html -> Html
joinBookmarks (Html sa) (Html sb) =
    Html $ map (foldr1 mergeSections) $ group $ sort $ sa ++ sb 

main :: IO ()
main = getOutput <$> TIO.getContents >>= TIO.putStr
