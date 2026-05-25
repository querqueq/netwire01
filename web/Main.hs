{-# LANGUAGE JavaScriptFFI #-}

-- Step B (first cut): render the actual game scene in the browser -- the
-- rocket (the stretched heptagon from the desktop version) in a procedural
-- starfield -- driven by the netwire FRP loop. There is no input or camera
-- yet; the ship simply rotates so the scene is visibly alive. Geometry is
-- computed in Haskell (ported from src/Lib.hs) and handed to the JS canvas
-- renderer (index.html) as a compact "x,y;x,y;..." string.
module Main where

import Prelude hiding ((.), id)
import Control.Wire
import FRP.Netwire
import Data.IORef
import Data.List (intercalate)
import System.Random
import GHC.JS.Prim (JSVal, toJSString)
import GHC.JS.Foreign.Callback (Callback, syncCallback, OnBlocked(ContinueAsync))

foreign import javascript unsafe "((f) => { globalThis.requestAnimationFrame(f); })"
  requestAnimationFrame :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "((s) => { globalThis.setStars(s); })"
  js_setStars :: JSVal -> IO ()

foreign import javascript unsafe "((s) => { globalThis.drawFrame(s); })"
  js_drawFrame :: JSVal -> IO ()

foreign import javascript unsafe "(() => { return globalThis.performance.now(); })"
  jsNow :: IO Double

-- Geometry, ported from src/Lib.hs (GLdouble -> Double) ----------------------

type Point = (Double, Double)

rotatePoint :: Point -> Double -> Point -> Point
rotatePoint (xo, yo) theta (x, y) =
    ( x' * cos theta - y' * sin theta + xo
    , y' * cos theta + x' * sin theta + yo )
  where
    x' = x - xo
    y' = y - yo

nGon :: Double -> Int -> [Point]
nGon r n = take n (go (0, r))
  where
    a    = (2 / fromIntegral n) * pi
    go p = p : go (rotatePoint (0, 0) a p)

stretch :: Point -> [Point] -> [Point]
stretch (sx, sy) = map (\(x, y) -> (x * sx, y * sy))

-- The player rocket: an elongated 7-gon, rotated to its heading. Sized up
-- from the desktop's 0.02 so it's clearly visible without the perspective
-- camera (which isn't ported yet).
rocket :: Double -> [Point]
rocket heading =
      map (rotatePoint (0, 0) heading)
    $ stretch (1.05, 1.95)
    $ nGon 0.08 7

-- Procedural starfield: a fixed set of stars with per-star brightness.
stars :: Int -> [(Double, Double, Double)]
stars n = take n (go (mkStdGen 42))
  where
    go g = let (x, g1) = randomR (-1, 1)     g
               (y, g2) = randomR (-1, 1)     g1
               (b, g3) = randomR (0.2, 1.0)  g2
           in (x, y, b) : go g3

-- Marshalling to the JS renderer --------------------------------------------

polyToString :: [Point] -> String
polyToString = intercalate ";" . map (\(x, y) -> show x ++ "," ++ show y)

starsToString :: [(Double, Double, Double)] -> String
starsToString = intercalate ";" . map (\(x, y, b) -> show x ++ "," ++ show y ++ "," ++ show b)

-- FRP: heading integrates a constant angular velocity (0.6 rad/s).
headingWire :: (HasTime t s, Monad m) => Wire s () m a Double
headingWire = integral 0 . pure 0.6

main :: IO ()
main = do
  js_setStars (toJSString (starsToString (stars 150)))
  t0      <- jsNow
  prevRef <- newIORef t0
  wireRef <- newIORef headingWire
  cbRef   <- newIORef (error "callback not yet initialised")
  cb <- syncCallback ContinueAsync $ do
    now  <- jsNow
    prev <- readIORef prevRef
    writeIORef prevRef now
    let dt = (now - prev) / 1000
    wire <- readIORef wireRef
    (result, wire') <- stepWire wire (Timed dt ()) (Right ())
    writeIORef wireRef wire'
    case result of
      Right heading -> js_drawFrame (toJSString (polyToString (rocket heading)))
      Left _        -> pure ()
    readIORef cbRef >>= requestAnimationFrame
  writeIORef cbRef cb
  requestAnimationFrame cb
