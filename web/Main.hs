-- Web port, GHC WebAssembly backend (wasm32-wasi). The ship physics are
-- ported faithfully from the desktop src/app (Main.hs shipWire): thrust is
-- integrated relative to the ship's heading, with no friction, so the ship
-- drifts (Newtonian). Six controls feed a per-frame acceleration triple
-- into the netwire ship wire. Input comes from a JS-side bitmask driven by
-- on-screen buttons (touch) and/or the keyboard (desktop). The camera
-- follows the ship, and the starfield wraps around the camera so motion is
-- visible.
--
-- The module is linked as a wasm32-wasi *reactor*: JS instantiates the
-- module, runs WASI _initialize, then calls the exported hs_start, which
-- registers a requestAnimationFrame callback that keeps the RTS alive
-- across frames.
module Main where

import Prelude hiding ((.), id)
import Control.Wire
import FRP.Netwire
import Control.Arrow (arr)
import Data.Bits ((.&.))
import Data.IORef
import Data.List (intercalate)
import System.Random
import GHC.Wasm.Prim

foreign export javascript "hs_start" main :: IO ()

-- Turn a Haskell IO action into a JS function (created once, reused for
-- every frame; never freed because the rAF loop lives forever).
foreign import javascript "wrapper"
  mkCallback :: IO () -> IO JSVal

foreign import javascript unsafe "requestAnimationFrame($1)"
  js_requestAnimationFrame :: JSVal -> IO ()

foreign import javascript unsafe "setStars($1)"
  js_setStars :: JSString -> IO ()

-- Render a frame: camera position (world) + the rocket polygon (camera space).
foreign import javascript unsafe "drawFrame($1, $2, $3)"
  js_drawFrame :: Double -> Double -> JSString -> IO ()

foreign import javascript unsafe "performance.now()"
  jsNow :: IO Double

-- Current control bitmask (see bit assignments below).
foreign import javascript unsafe "controlBits()"
  jsControlBits :: IO Int

-- Geometry, ported from src/Lib.hs --------------------------------------------

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

rocket :: Double -> [Point]
rocket heading =
      map (rotatePoint (0, 0) heading)
    $ stretch (1.05, 1.95)
    $ nGon 0.08 7

-- Physics constants, ported from the desktop version -------------------------

accelScale :: Double
accelScale = 1.5

mainAccel, maneuverAccel, rotAccel :: Double
mainAccel     = 0.5 / accelScale
maneuverAccel = mainAccel / 2
rotAccel      = 2

-- Controls --------------------------------------------------------------------

-- Bit layout shared with the JS side (web/index.html).
bitRotLeft, bitRotRight, bitForward, bitReverse, bitStrafeLeft, bitStrafeRight :: Int
bitRotLeft     = 1
bitRotRight    = 2
bitForward     = 4
bitReverse     = 8
bitStrafeLeft  = 16
bitStrafeRight = 32

-- Decode the control bitmask into (rotational, strafe, forward) accelerations,
-- matching the desktop thrust wiring.
type Accels = (Double, Double, Double)

decodeControls :: Int -> Accels
decodeControls b = (racc, xacc, yacc)
  where
    on m  = (b .&. m) /= 0
    racc  = (if on bitRotLeft     then rotAccel       else 0)
          + (if on bitRotRight    then negate rotAccel else 0)
    xacc  = (if on bitStrafeLeft  then negate maneuverAccel else 0)
          + (if on bitStrafeRight then maneuverAccel        else 0)
    yacc  = (if on bitForward     then mainAccel            else 0)
          + (if on bitReverse     then negate maneuverAccel else 0)

-- Ship physics ----------------------------------------------------------------

-- Input: per-frame (rotational, strafe-x, forward-y) accelerations.
-- Output: (posX, posY, heading, velX, velY).
type ShipOut = (Double, Double, Double, Double, Double)

shipWire :: (HasTime t s, Monad m) => Wire s () m Accels ShipOut
shipWire = (,,,,) <$> px <*> py <*> heading <*> vx <*> vy
  where
    racc    = arr (\(r, _, _) -> r)
    xacc    = arr (\(_, x, _) -> x)
    yacc    = arr (\(_, _, y) -> y)
    heading = integral 0 . racc
    -- Thrust is applied relative to the ship's heading.
    rxy     = (\h x y -> rotatePoint (0, 0) h (x, y)) <$> heading <*> xacc <*> yacc
    vx      = integral 0 . (fst <$> rxy)
    vy      = integral 0 . (snd <$> rxy)
    px      = integral 0 . vx
    py      = integral 0 . vy

-- Starfield -------------------------------------------------------------------

stars :: Int -> [(Double, Double, Double)]
stars n = take n (go (mkStdGen 42))
  where
    go g = let (x, g1) = randomR (-1, 1)    g
               (y, g2) = randomR (-1, 1)    g1
               (b, g3) = randomR (0.2, 1.0) g2
           in (x, y, b) : go g3

-- Marshalling -----------------------------------------------------------------

polyToString :: [Point] -> String
polyToString = intercalate ";" . map (\(x, y) -> show x ++ "," ++ show y)

starsToString :: [(Double, Double, Double)] -> String
starsToString = intercalate ";" . map (\(x, y, b) -> show x ++ "," ++ show y ++ "," ++ show b)

-- Main loop -------------------------------------------------------------------

main :: IO ()
main = do
  js_setStars (toJSString (starsToString (stars 150)))
  t0      <- jsNow
  prevRef <- newIORef t0
  wireRef <- newIORef shipWire
  cbRef   <- newIORef (error "callback not yet initialised")
  cb <- mkCallback $ do
    now  <- jsNow
    prev <- readIORef prevRef
    writeIORef prevRef now
    let dt = (now - prev) / 1000
    bits <- jsControlBits
    wire <- readIORef wireRef
    (result, wire') <- stepWire wire (Timed dt ()) (Right (decodeControls bits))
    writeIORef wireRef wire'
    case result of
      Right (px, py, heading, _vx, _vy) ->
        js_drawFrame px py (toJSString (polyToString (rocket heading)))
      Left _ -> pure ()
    readIORef cbRef >>= js_requestAnimationFrame
  writeIORef cbRef cb
  js_requestAnimationFrame cb
