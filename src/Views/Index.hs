module Views.Index where

import           Text.Blaze.Html5               ((!))
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A

index :: Int -> H.Html
index visitCount =
    H.html $ do
        H.head $ do
            H.title $ H.toHtml "Mikail Khan"
        H.body $ do
            H.p $ do
                H.toHtml "i'm mikail"
                H.br
                H.toHtml "Visit Count: " <> H.toHtml visitCount
            H.ul $ do
                H.li $ H.a ! A.href (H.toValue "/memes") $ H.toHtml "Memes"
                H.li $ H.a ! A.href (H.toValue "https://github.com/mkhan45") $ H.toHtml "Github"
