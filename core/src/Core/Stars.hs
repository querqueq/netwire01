{-# LANGUAGE RecordWildCards #-}

-- The game starfield (moved from app/Main.hs) and the pulsating-sky demo
-- wires (moved from src/Stars.hs). Rendering stays with the frontends: the
-- desktop projects the stars through its GL perspective camera
-- (app/Main.hs), the web mirrors that projection on the canvas
-- (web/index.html). This module must build on both GHC 8.0.2 (stack) and
-- GHC 9.12 (wasm32-wasi).
module Core.Stars where

import Prelude hiding ((.)) -- To use (.) in the scope of Categories instead
import Control.Wire
import FRP.Netwire
import System.Random
import qualified Data.MemoCombinators as Memo
import Core.Geometry

-- The game starfield ----------------------------------------------------------

type Star2 = (FT,FT,FT)

stars :: (RandomGen g) => g -> Point -> FT -> [Star2]
stars g (xOffset,yOffset) range = zipWith (\(x,y) z -> (x+xOffset,y+yOffset,z))
    (pairs $ randomRs (-range,range) g)
    (randomRs (starNear,starFar) g)

-- The 3x3 block of 4-unit chunks around a position. Each chunk's stars are
-- generated from a seed derived from its grid cell and memoized, so the
-- field is endless but revisited regions always look the same.
starsAt :: Point -> [Star2]
starsAt (x,y) = foldl (\ss (xo,yo) -> starsAt' (xo+x,yo+y) ++ ss) []
    [(-r,r ),(0 ,r ),(r ,r )
    ,(-r,0 ),(0 ,0 ),(r ,0 )
    ,(-r,-r),(0 ,-r),(r ,-r)
    ]
    where r = 4

starsAt' :: Point -> [Star2]
starsAt' (x,y) = hundredStarsAt (f x,f y) where f = fromIntegral . fst . withinRange 4

hundredStarsAt :: (Int,Int) -> [Star2]
hundredStarsAt = (Memo.pair Memo.integral Memo.integral) f
    where f (x,y) = take 111 $ stars (mkStdGen $ truncate $ fromIntegral $ x+y) (fromIntegral x,fromIntegral y) 4

withinRange :: Integer -> FT -> (Integer,Integer)
withinRange j x = (x'-a,x'-a+j)
    where a = x' `mod` j
          x' = truncate x

-- The grid cell anchoring the chunk block at a position. The visible star
-- set only changes when this changes, so frontends that marshal the field to
-- a renderer (web/Main.hs) re-send it only on a cell change.
starsChunk :: Point -> (Integer,Integer)
starsChunk (x,y) = (fst $ withinRange 4 x, fst $ withinRange 4 y)

starNear :: FT
starNear = -200
starFar :: FT
starFar = -500

-- The pulsating-sky demo wires --------------------------------------------------

data Star = Star
    { starX :: FT
    , starY :: FT
    , starZ :: FT
    , starBrightness :: !FT
    }

instance Show Star where
    show (Star {..}) = "(" ++ format starX ++ "," ++ format starY ++ ") " ++ format starBrightness ++ "*"

skyWire :: (Fractional t, Monad m, HasTime t s, RandomGen g) => g -> Int -> Wire s () m a [Star]
skyWire g starsN = do
    let xys = zip (take starsN rands) (drop starsN rands)
    let gs = map mkStdGen $ randoms g
    sequenceA $ zipWith pulsarWire gs xys
    where rands = take (starsN * 2) $ randomRs (-1 :: FT,1) g

starPosWire :: Point -> Wire s e m a Point
starPosWire pos = WConst $ Right pos

randStarPosWire :: (RandomGen g) => g -> Wire s e m a Point
randStarPosWire g = do
    let (x:(y:_)) = randoms g
    starPosWire (x,y)

pulsarWire :: (Monad m, RandomGen g, HasTime t s, Fractional t) => g -> Point -> Wire s () m a Star
pulsarWire g (x,y) = Star
         <$> pure x
         <*> pure y
         <*> pure 0
         <*> alternateByInhibit (wackelkontakt 0.4137 0.992 g . pure base) (pulsateWire base peak pulsateT 0.5)
         where base = fst $ randomR (0.001,0.002) g
               peak = fst $ randomR (0.005,0.025) g -- 0.01
               pulsateT = 2.5

pulsateWire :: (Fractional t, Monad m, HasTime t s) => FT -> FT -> t -> t -> Wire s () m a FT
pulsateWire base peak totalT holdT = for totalT . integral base .
    (   for changeT . integral target . pure (-target)
    --> for holdT . pure 0
    --> for changeT . integral (-target) . pure target
    --> pure 0
    )
    where changeT = (totalT - holdT) / 2
          target = peak - base

alternateByInhibit w1 w2 = w1 --> w2 --> alternateByInhibit w1 w2
