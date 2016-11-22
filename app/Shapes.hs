module Shapes(
  Shape(..), Transform(..), Drawing, Style(..), Colour(..),
  empty, circle, square, ellipse,
  getMatTransVals, transform)  where

import Data.Matrix

-- Utilities

getMatTransVals :: Matrix Double -> (Double,Double,Double,Double,Double,Double)
getMatTransVals m = (getElem 1 1 m, getElem 2 1 m, getElem 1 2 m,
                     getElem 2 2 m, getElem 1 3 m, getElem 2 3 m)

transToMatrix :: Double -> Double -> Double -> Double -> Double -> Double -> Matrix Double
transToMatrix a b c d e f = fromLists [ [a,c,e], [b,d,f], [0,0,1] ]

degToRad :: Double -> Double
degToRad deg = deg * (pi / 180)
        
-- Shapes

data Shape = Empty 
           | Circle 
           | Square
           | Ellipse 
             deriving (Show, Read, Eq)

empty, circle, square, ellipse :: Shape

empty = Empty
circle = Circle
square = Square
ellipse = Ellipse

-- Transformations

data Transform = Identity
           | Translate Double Double
           | Scale Double Double
           | Compose Transform Transform
           | Rotate Double
             deriving (Show, Read)


transform :: Transform -> Matrix Double
transform Identity                     = (transToMatrix 1 0 0 1 0 0)
transform (Translate tx ty)            = (transToMatrix 1 0 0 1 tx ty)
transform (Scale sx sy)                = (transToMatrix sx 0 0 sy 0 0)
transform (Rotate an)                  = (transToMatrix (cos a) (sin a) (-sin a) (cos a) 0 0)
  where a = degToRad an
transform (Compose t1 t2)              =  multStd (transform t1) (transform t2)

-- Colours

data Colour = Red
            | Green
            | Blue
            | Orange
            | Purple
            | Yellow
            | Black
            | White
            | Cyan
            | Greenyellow
            | Orangered
              deriving (Show, Read)

red, green, blue, orange, purple, yellow, black, white, cyan, greenyellow, orangered:: Colour

red = Red
green = Green
blue = Blue
orange = Orange
purple = Purple
yellow = Yellow
black = Black
white = White
cyan = Cyan
greenyellow = Greenyellow
orangered = Orangered

-- Styles (strokewidth fill strokecolour)

type StrokeWidth = Double
type StrokeColour = Colour
type FillColour = Colour

type Style = (StrokeWidth,StrokeColour,FillColour)

-- Drawings

type Drawing = [(Transform,Shape,Style)]

