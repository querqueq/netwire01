-- Desktop-side utilities. The pure geometry (FT, Point, Polygon, rotatePoint,
-- nGon, move, stretch, rocket, pairs, mapTuple) lives in netwire01-core's
-- Core.Geometry, shared with the web frontend, and is re-exported here so
-- existing desktop code keeps importing Lib.
module Lib (module Core.Geometry, module Lib) where

import Prelude hiding ((.)) -- To use (.) in the scope of Categories instead
import FRP.Netwire
import FRP.Netwire.Input
import FRP.Netwire.Input.GLFW
import Control.Wire.Core
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Data.List
import System.Random
import Core.Geometry

type InputWire s a b = Wire s () (GLFWInputT IO) a b

dent :: Point -> Polygon -> Polygon
dent d@(x,y) p = undefined

randomRTuples bounds g = pairs $ randomRs bounds g

comet :: RandomGen g => g -> FT -> Int -> Polygon
comet g r n =
      (\p -> foldr dent p $ take (n `div` 3) $ randomRTuples (-r,r) g)
    $ stretch (head $ randomRTuples (0.9,1.2) g)
    $ nGon r (n `div` 3 * 2)

renderPoint :: (FT, FT) -> IO ()
renderPoint (x, y) = GL.vertex $ GL.Vertex3 (realToFrac x :: GL.GLfloat) (realToFrac y :: GL.GLfloat) (-100)

snail :: [[Int]] -> [Int]
snail [] = []
snail (xs:xss) = xs ++ (snail . reverse . transpose) xss
