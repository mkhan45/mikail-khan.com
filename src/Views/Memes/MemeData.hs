{-# LANGUAGE OverloadedStrings #-}

module Views.Memes.MemeData where

import qualified Data.Text                      as T
import qualified Text.Blaze.Html5               as H

data MemeType = Image | MP4 | Youtube deriving Show

data Meme = Meme { ty :: MemeType, title :: T.Text, url :: T.Text }

readMeme :: T.Text -> Meme
readMeme line = readSplit $ T.splitOn " " $ line
    where readTy :: T.Text -> MemeType
          readTy "image" = Image 
          readTy "MP4" = MP4
          readTy "Youtube" = Youtube
          readTy _ = Image
          readSplit :: [T.Text] -> Meme
          readSplit [ty, title, url] = Meme {ty=readTy $ T.strip $ ty,title=title, url=url}

readMemes :: [T.Text] -> Int -> Int -> [Meme]
readMemes ls start num
  | start >= 0 = map readMeme (take num $ drop start $ ls)
  | otherwise = map readMeme (take num $ drop (-start - num) $ reverse ls)

readMemeFile :: Int -> IO [Meme]
readMemeFile page = do
    contents <- readFile "static/Assets/memes.txt"
    return $ readMemes (map T.pack $ lines contents) (page * 12) 12

tToHTML = H.toHtml . T.pack
