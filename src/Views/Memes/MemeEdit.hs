{-# LANGUAGE OverloadedStrings #-}

module Views.Memes.MemeEdit where

import qualified Data.Text                      as T
import           Text.Blaze.Html5               ((!))
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A

import           Control.Monad                  (forM_)

import qualified System.IO.Strict               as S
import qualified Data.ByteString.Char8          as BS
import qualified Data.List                      as L
import           Crypto.BCrypt

import           Views.Memes.MemeData

memeRowHTML :: Meme -> H.Html
memeRowHTML (Meme {ty=ty, title=title, url=url}) =
    H.tr $ do
        H.th $ H.toHtml $ show ty
        H.th $ H.toHtml title
        H.th $ H.a ! A.href (H.toValue url) $ H.toHtml url

headerRow :: H.Html
headerRow =
    H.tr $ do
        H.th $ tToHTML "Type"
        H.th $ tToHTML "Title"
        H.th $ tToHTML "URL"

inputRow :: H.Html
inputRow =
    H.tr $ do
        H.th $ H.input ! A.type_ "text" ! A.name "type"
        H.th $ H.input ! A.type_ "text" ! A.name "title"
        H.th $ H.input ! A.type_ "text" ! A.name "url"

memeEditHTML :: [Meme] -> H.Html
memeEditHTML memes = 
    H.html ! A.style "font-family: sans-serif" $ do
        H.h1 $ tToHTML "Memes"
        H.form ! A.action "/memes/meme_submit" ! A.method "post" $ do
            H.table ! A.style "width: 100vw; text-align: left" $ do
                headerRow
                forM_ memes memeRowHTML
                inputRow
            H.input ! A.type_ "password" ! A.name "password"
            H.input ! A.type_ "submit" ! A.value "Add Meme"

addMeme :: T.Text -> T.Text -> T.Text -> IO ()
addMeme ty title url = do
    let memeLS = map T.unpack [ty, title, url]
    let memeStr = L.intercalate " " memeLS
    file <- S.readFile "static/Assets/memes.txt"
    writeFile "static/Assets/memes.txt" (concat [memeStr, "\n", file])

-- use through `stach ghci` to make new password
genHash :: T.Text -> IO (Maybe BS.ByteString)
genHash p = hashPasswordUsingPolicy (policy) (BS.pack $ T.unpack p)
            where policy = HashingPolicy 11 $ BS.pack "$2y$"

checkPass :: String -> Bool
checkPass = validatePassword passHash . BS.pack
            where passHash = BS.pack "***REMOVED***"
