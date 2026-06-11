-- Pure 2D geometry shared by the desktop (OpenGL) and web (wasm) frontends,
-- moved here from src/Lib.hs. FT is Double; the desktop's GL.GLdouble is a
-- type synonym for Double (OpenGLRaw >= 3), so GL code can use these values
-- without conversion.
module Core.Geometry where

import Numeric

type FT = Double
type Point = (FT,FT)

type Polygon = [Point]

rotatePoint :: Point -> FT -> Point -> Point
rotatePoint (xo,yo) θ (x,y) = (x' * (cos θ) - y' * (sin θ) + xo
                              ,y' * (cos θ) + x' * (sin θ) + yo)
    where x' = x - xo
          y' = y - yo

nGon :: FT -> Int -> Polygon
nGon r n = take n $ spin (0,r)
    where a = (2/(fromIntegral n)) * pi
          spin p = p : spin (rotatePoint (0,0) a p)

move :: Point -> Point -> Point
move (xo,yo) (x,y) = (xo+x,yo+y)

stretch :: Point -> Polygon -> Polygon
stretch (sx,sy) = map (\(x,y) -> (x*sx,y*sy))

-- The player's ship, a stretched heptagon pointing along +y, parameterized by
-- base radius (the frontends render at different scales).
rocket :: FT -> Polygon
rocket r = stretch (1.05,1.95) $ nGon r 7

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:[]) = error "Missing an element for pairs"
pairs (x':(x'':xs)) = (x',x'') : pairs xs

mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple f (x,y) = (f x,f y)

format x = showFFloat (Just 2) x ""
