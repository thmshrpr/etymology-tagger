module EtymologyTagger where

import Data.List (groupBy)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Network.HTTP
import Text.HTML.TagSoup

type Etymology = String

etymOnlineURL :: String
etymOnlineURL = "http://etymonline.com/index.php?term="

main :: IO ()
main = do args <- getArgs
          let file = head args
          text <- readFile file
          taggedWords <- mapM tagWord (words text)
          print taggedWords


tagWord :: String -> IO (String, String)
tagWord w = do response <- lookupWord w
               let entries = getEntries response
               let etym = getEtymology (head entries)
               return (w, etym)

getEntries :: String -> [String]
getEntries raw = let rawEntries = (groupBy (\ _ y -> TagOpen "dt" [("class","highlight")] ~/= y) .
                                    takeWhile (/=TagComment " DICTIONARY ") .
                                    drop 4 .
                                    dropWhile (/= TagOpen "div" [("id","dictionary")]) .
                                    parseTags) raw
                 in (map (snd . parseEntry) rawEntries)

parseEntry :: [Tag String] -> (String, String)
parseEntry entry = (word, desc)
  where
    word = fromTagText (entry !! 2)
    desc = (concat . mapMaybe maybeTagText . takeWhile (TagClose "dd" ~/=) . dropWhile (TagOpen "dd" [("class","highlight")] ~/=)) entry



getEtymology :: String -> String
getEtymology []   = "Unknown"
getEtymology desc = (dropWhile (/= "from") (words desc)) !! 1

lookupWord w = do res <- simpleHTTP (getRequest (etymOnlineURL ++ w))
                  body <- (getResponseBody res)
                  return body
