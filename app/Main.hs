{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Shapes
  
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Text as R

import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Text (renderSvg)


import qualified Data.Text.Lazy as L

main = scotty 3000 $ do
-- Load the index by default
  get "/" $ file "index.html"
-- Extract the post parameter and convert it into an SVG to render
  get "/:postParam" $ do
    parseParam <- param "postParam" 
    let d = read parseParam -- read the param as a drawing
    html $ renderSvg (svgGenDoc d)

-- Generate the default SVG doc
svgGenDoc :: Drawing -> S.Svg
svgGenDoc d = S.docTypeSvg ! A.version "1.1" ! A.viewbox "0 0 100 100" $ do
  svgGen d

-- Recursively parse the input drawings into SVGs, and build one big SVG at the end to put into the doc
svgGen :: Drawing -> S.Svg
svgGen [d] = parse d 
svgGen (d:ds) = parse d >> svgGen ds

-- Parse the drawings into usuable chunks, apply transforms
parse :: (Transform, Shape, Style) -> S.Svg
parse (ts, s, (sw,sc,fc)) = shape ! strokeWidth ! stroke ! fill ! trans ! H.customAttribute "vector-effect" "non-scaling-stroke"
  where shape = shapeToSvg s
        strokeWidth = A.strokeWidth (S.toValue sw)
        stroke = A.stroke $ S.stringValue (show sc)
        fill = A.fill $ S.stringValue (show fc)
        trans = A.transform (S.matrix a b c d e f)
        (a,b,c,d,e,f) = getMatTransVals m
        m = transform ts

-- turn a shape into an SVG Shape
shapeToSvg :: Shape -> S.Svg
shapeToSvg shape
  | shape == empty = S.rect
  | shape == circle = S.circle ! A.cx "1" ! A.cy "1" ! A.r "5" 
  | shape == square = S.rect ! A.width "10" ! A.height "10"
  | shape == ellipse = S.ellipse ! A.cx "1" ! A.cy "1" ! A.rx "3" ! A.ry "2"

