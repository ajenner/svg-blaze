{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
  
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Text as R

import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Text (renderSvg)


import Data.Text.Lazy

main = scotty 3000 $ do
  get "/" $ do
    html "Hello World!"

  get "/greet" $ do
      html $ "Yo"

  get (literal "/greet/") $ do
      html $ "Oh, wow!"

  get "/greet/:name" $ do
      html $ renderSvg (svgDoc1 0 "50" "blue")

svgDoc :: S.Svg
svgDoc = S.docTypeSvg ! A.version "1.1" ! A.width "800" ! A.height "600" ! A.viewbox "0 0 2 4" $ do
    S.g ! A.transform makeTransform $ do
      S.rect ! A.width "2" ! A.height "2" ! A.fill "#008d46"
      S.rect ! A.width "1" ! A.height "2" ! A.fill "#ffffff"
      S.rect ! A.width "1" ! A.height "2" ! A.fill "#d2232c"
      S.path ! A.d makePath

svgDoc1 :: Int -> S.AttributeValue -> S.AttributeValue -> S.Svg
svgDoc1 0 s c = S.docTypeSvg ! A.version "1.1" ! A.viewbox "0 0 200 200" $ do
    S.g $ do
      S.rect ! A.width s ! A.height s ! A.fill c
      S.path ! A.d makePath
svgDoc1 1 s c = S.docTypeSvg ! A.version "1.1" ! A.viewbox "0 0 200 200" $ do
    S.g $ do
      S.circle ! A.cx s ! A.cy s ! A.r s ! A.fill c
      S.path ! A.d makePath


--svgGen :: Drawing -> S.Svg
--svgGen [] = makePath
--svgGen d = parse d 
--svgGen (d:ds) = parse d svgGen ds

--parse :: Drawing -> S.Svg
--parse (x, y, z) = makePath3

makePath :: S.AttributeValue
makePath = mkPath $ do
  l 2 3
  m 4 5

makeTransform :: S.AttributeValue
makeTransform = rotate 50

response :: Text -> Text
response n = do R.renderHtml $ do
                  H.h1 ( "Hello " >> H.toHtml n)

longresponse :: Text -> Text
longresponse n = do
  R.renderHtml $ do
    H.head $ H.title "Welcome page"
    H.body $ do
      H.h1 "Welcome!"
      H.p ("Welcome to my Scotty app " >> H.toHtml n)
      
    
