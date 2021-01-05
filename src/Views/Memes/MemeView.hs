{-# LANGUAGE OverloadedStrings #-}

module Views.Memes.MemeView where

import qualified Data.Text                      as T
import           Text.Blaze.Html5               ((!))
import qualified Text.Blaze.Html5               as H
import           Text.Blaze.Html5.Attributes    (style, href, class_, src)
import qualified Text.Blaze.Html5.Attributes    as A

import           Control.Monad                  (forM_)

import           Views.Memes.MemeData

navButtons :: Int -> H.Html
navButtons pageNum =
    H.div ! style "display: flex; justify-content: space-between; width: 98vw" $ do
        H.a ! href prevPage ! class_ "btn" $ tToHTML "Previous Page"
        H.a ! href nextPage ! class_ "btn" $ tToHTML "Next Page"
    where nextPage = H.toValue $ "/memes/" <> (show $ pageNum + 1)
          prevPage = H.toValue $ "/memes/" <> (show $ pageNum - 1)

memeContentHTML :: Meme -> H.Html

memeContentHTML meme =
    case ty of
      Image -> 
          H.a ! A.id title ! href url $ do
              H.img ! class_ "meme" ! src url ! A.alt "meme"

      MP4 -> 
          H.video ! A.id title ! class_ "meme" ! A.controls "bar" $ do
              H.source ! src url ! A.type_ "video/mp4"
              tToHTML "Unsupported browser for videos"

      Youtube ->
          H.iframe ! A.id title ! A.title "Youtube Video" ! class_ "meme" ! src url $ do
              tToHTML "Youtube Video"
    where 
        ty = memeTy meme
        url = H.toValue $ memeURL meme
        title = (H.toValue $ memeTitle meme) <> "_meme"

memeGrid :: Meme -> H.Html
memeGrid meme@(Meme { memeTitle=title }) =
    H.div ! class_ "grid-entry" $ do
        H.h2 $ H.a ! A.id titleVal ! href ("#" <> titleVal) $ H.toHtml title
        memeContentHTML meme
    where titleVal = H.toValue $ memeTitle meme

memeHTML :: Int -> [Meme] -> H.Html
memeHTML pageNum memes =
    H.html $ do
        H.head $ do
            H.title $ tToHTML "bad memes"
            H.link ! A.rel "stylesheet" ! href "/CSS/Memes.css"
        H.body $ do
            navButtons pageNum
            H.div ! class_ "grid" $ forM_ memes memeGrid
            navButtons pageNum
