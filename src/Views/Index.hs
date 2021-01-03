module Views.Index where

import           Text.Blaze.Html5               (html, title, p, ul, li, a, (!))
import qualified Text.Blaze.Html5               as H
import           Text.Blaze.Html5.Attributes    (href)
import qualified Text.Blaze.Html5.Attributes    as A

index :: Int -> H.Html
index visitCount =
    html $ do
        H.head $ do
            title $ H.toHtml "Mikail Khan"
        H.body $ do
            p $ do
                H.toHtml "i'm mikail"
                H.br
                H.toHtml "Visit Count: " <> H.toHtml visitCount
            ul $ do
                li $ a ! href (H.toValue "/memes") $ H.toHtml "Memes"
                li $ a ! href (H.toValue "/resume") $ H.toHtml "Resume"
                li $ a ! href (H.toValue "https://github.com/mkhan45") $ H.toHtml "Github"
