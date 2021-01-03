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

memeContentHTML Meme { ty=ty, title=title, url=url } =
    case ty of
      Image -> 
          H.a ! A.id titleVal ! href urlVal $ do
              H.img ! class_ "meme" ! src urlVal ! A.alt "meme"

      MP4 -> 
          H.video ! A.id titleVal ! class_ "meme" ! A.controls "bar" $ do
              H.source ! src urlVal ! A.type_ "video/mp4"
              tToHTML "Unsupported browser for videos"

      Youtube ->
          H.iframe ! A.id titleVal ! A.title "Youtube Video" ! class_ "meme" ! src urlVal $ do
              tToHTML "Youtube Video"
    where 
        urlVal = H.toValue url
        titleVal = (H.toValue title) <> "_meme"

memeGrid :: Meme -> H.Html
memeGrid meme@(Meme { ty=ty, title=title, url=url }) =
    H.div ! class_ "grid-entry" $ do
        H.h2 $ H.a ! A.id titleVal ! href ("#" <> titleVal) $ H.toHtml title
        memeContentHTML meme
    where titleVal = H.toValue title

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
