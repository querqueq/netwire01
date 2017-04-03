module Lib where

import Prelude hiding ((.)) -- To use (.) in the scope of Categories instead
import FRP.Netwire 
import FRP.Netwire.Input
import FRP.Netwire.Input.GLFW
import Control.Wire.Core
import Numeric 
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Data.List
import System.Random

type FT = GL.GLdouble
type InputWire s a b = Wire s () (GLFWInputT IO) a b
type Point = (FT,FT)

type Polygon = [Point]

nGon :: FT -> Int -> Polygon
nGon r n = take n $ spin (0,r)
    where a = (2/(fromIntegral n)) * pi
          spin p = p : spin (rotatePoint (0,0) a p)

move :: Point -> Point -> Point
move (xo,yo) (x,y) = (xo+x,yo+y)

stretch :: Point -> Polygon -> Polygon
stretch (sx,sy) = map (\(x,y) -> (x*sx,y*sy))

dent :: Point -> Polygon -> Polygon
dent d@(x,y) p = undefined

pairs [] = []
pairs (x:[]) = error "Missing an element for pairs"
pairs (x':(x'':xs)) = (x',x'') : pairs xs

randomRTuples bounds g = pairs $ randomRs bounds g 

comet :: RandomGen g => g -> FT -> Int -> Polygon
comet g r n = 
      (\p -> foldr dent p $ take (n `div` 3) $ randomRTuples (-r,r) g)
    $ stretch (head $ randomRTuples (0.9,1.2) g)
    $ nGon r (n `div` 3 * 2)

mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple f (x,y) = (f x,f y)

alternateByInhibit w1 w2 = w1 --> w2 --> alternateByInhibit w1 w2

format x = showFFloat (Just 2) x ""

renderPoint :: (FT, FT) -> IO ()
renderPoint (x, y) = GL.vertex $ GL.Vertex3 (realToFrac x :: GL.GLfloat) (realToFrac y :: GL.GLfloat) (-100)

rotatePoint :: (FT, FT) -> FT -> (FT, FT) -> (FT, FT)
rotatePoint (xo,yo) θ (x,y) = (x' * (cos θ) - y' * (sin θ) + xo
                              ,y' * (cos θ) + x' * (sin θ) + yo)
    where x' = x - xo
          y' = y - yo

snail :: [[Int]] -> [Int]
snail [] = []
snail (xs:xss) = xs ++ (snail . reverse . transpose) xss

