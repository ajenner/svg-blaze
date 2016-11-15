{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Shapes
  
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
      html $ renderSvg (svgGenDoc ([(Identity, Square, (Style 0 Red Red)), (Translate (Vector 10 10), Circle, (Style 0 Blue Blue))]))

  get "/greet/:name" $ do
      html $ renderSvg (svgDoc1 0 "50" "green")

svgDoc1 :: Int -> S.AttributeValue -> S.AttributeValue -> S.Svg
svgDoc1 0 s c = S.docTypeSvg ! A.version "1.1" ! A.viewbox "0 0 200 200" $ do
    S.g $ do
      S.rect ! A.width s ! A.height s ! A.fill c
svgDoc1 1 s c = S.docTypeSvg ! A.version "1.1" ! A.viewbox "0 0 200 200" $ do
    S.g $ do
      S.circle ! A.cx s ! A.cy s ! A.r s ! A.fill c

svgGenDoc :: Drawing -> S.Svg
svgGenDoc d = S.docTypeSvg ! A.version "1.1" ! A.viewbox "0 0 200 200" $ do
  svgGen d

svgGen :: Drawing -> S.Svg
svgGen [d] = parse d 
svgGen (d:ds) = parse d >> svgGen ds

parse :: (Transform, Shape, Style) -> S.Svg
parse (Translate (Vector x y), Circle, _) = S.g $ do
                                            S.circle ! A.cx (S.stringValue (show x))  ! A.cy (S.stringValue (show y))  ! A.r "10"  ! A.fill "blue" 
parse (Translate (Vector x y), Square, _) = S.g $ do
                                            S.rect ! A.width (S.stringValue (show x)) ! A.height (S.stringValue (show y)) ! A.fill "red"
parse (_, Circle, _)                      = S.g $ do
                                            S.circle ! A.cx "50" ! A.cy "50"  ! A.r "50"  ! A.fill "green" 
parse (_, Square, _)                      = S.g $ do
                                            S.rect ! A.width "50" ! A.height "50" ! A.fill "green"                          


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
      
    
