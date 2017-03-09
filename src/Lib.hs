module Lib where

import Prelude hiding ((.)) -- To use (.) in the scope of Categories instead
import FRP.Netwire 
import FRP.Netwire.Input
import FRP.Netwire.Input.GLFW
import Control.Wire.Core
import Numeric 
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

type InputWire s a b = Wire s () (GLFWInputT IO) a b
type Point = (Float,Float)

mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple f (x,y) = (f x,f y)

alternateByInhibit w1 w2 = w1 --> w2 --> alternateByInhibit w1 w2

format x = showFFloat (Just 2) x ""

renderPoint :: (Float, Float) -> IO ()
renderPoint (x, y) = GL.vertex $ GL.Vertex3 (realToFrac x :: GL.GLfloat) (realToFrac y :: GL.GLfloat) (-100)

rotatePoint :: (Float, Float) -> Float -> (Float, Float) -> (Float, Float)
rotatePoint (xo,yo) θ (x,y) = (x' * (cos θ) - y' * (sin θ) + xo
                              ,y' * (cos θ) + x' * (sin θ) + yo)
    where x' = x - xo
          y' = y - yo

