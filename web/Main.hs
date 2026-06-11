-- Web port, GHC WebAssembly backend (wasm32-wasi). The game logic (geometry,
-- ship physics, thruster exhaust particles, starfield) comes from the shared
-- netwire01-core package — the same wires the desktop OpenGL frontend runs.
-- This module only maps the JS control bitmask to a Thrust value, steps the
-- wires once per animation frame, and marshals the results to the canvas
-- renderer in web/index.html.
--
-- The module is linked as a wasm32-wasi *reactor*: JS instantiates the
-- module, runs WASI _initialize, then calls the exported hs_start, which
-- registers a requestAnimationFrame callback that keeps the RTS alive
-- across frames.
module Main where

import Prelude hiding ((.), id)
import Control.Wire
import FRP.Netwire
import Data.Bits ((.&.))
import Data.IORef
import Data.List (intercalate)
import GHC.Wasm.Prim

import Core.Geometry
import Core.Particles
import Core.Ship
import Core.Stars

foreign export javascript "hs_start" main :: IO ()

-- Turn a Haskell IO action into a JS function (created once, reused for
-- every frame; never freed because the rAF loop lives forever).
foreign import javascript "wrapper"
  mkCallback :: IO () -> IO JSVal

foreign import javascript unsafe "requestAnimationFrame($1)"
  js_requestAnimationFrame :: JSVal -> IO ()

foreign import javascript unsafe "setStars($1)"
  js_setStars :: JSString -> IO ()

-- Render a frame: camera position (world), the rocket polygon (camera
-- space), and the exhaust particles (world space).
foreign import javascript unsafe "drawFrame($1, $2, $3, $4)"
  js_drawFrame :: Double -> Double -> JSString -> JSString -> IO ()

foreign import javascript unsafe "performance.now()"
  jsNow :: IO Double

-- Current control bitmask (see bit assignments below).
foreign import javascript unsafe "controlBits()"
  jsControlBits :: IO Int

-- Controls --------------------------------------------------------------------

-- Bit layout shared with the JS side (web/index.html).
bitRotLeft, bitRotRight, bitForward, bitReverse, bitStrafeLeft, bitStrafeRight :: Int
bitRotLeft     = 1
bitRotRight    = 2
bitForward     = 4
bitReverse     = 8
bitStrafeLeft  = 16
bitStrafeRight = 32

-- Decode the control bitmask into the thrust levels of the shared ship wire,
-- mirroring the desktop's key wires (app/Main.hs).
decodeControls :: Int -> Thrust
decodeControls b = Thrust
    { thrustFront    = ifOn bitForward     mainAcceleration
    , thrustBack     = ifOn bitReverse     (negate maneuveringAcceleration)
    , thrustLeft     = ifOn bitStrafeLeft  (negate maneuveringAcceleration)
    , thrustRight    = ifOn bitStrafeRight maneuveringAcceleration
    , thrustRotLeft  = ifOn bitRotLeft     rotationalAcceleration
    , thrustRotRight = ifOn bitRotRight    (negate rotationalAcceleration)
    }
  where
    ifOn m a = if (b .&. m) /= 0 then a else 0

-- Game wire -------------------------------------------------------------------

-- Ship state plus its exhaust particles, both from netwire01-core. As in the
-- desktop frameWire, each use of shipWire is an independent instance of the
-- same deterministic integrator, stepped with the same input and time delta.
gameWire :: (HasTime t s, Fractional t, Monad m) => Wire s () m Thrust (Ship, [Particle])
gameWire = (,) <$> shipWire <*> (exhaustWire . shipWire)

-- Marshalling -----------------------------------------------------------------

polyToString :: [Point] -> String
polyToString = intercalate ";" . map (\(x, y) -> show x ++ "," ++ show y)

starsToString :: [(Double, Double, Double)] -> String
starsToString = intercalate ";" . map (\(x, y, b) -> show x ++ "," ++ show y ++ "," ++ show b)

particlesToString :: [Particle] -> String
particlesToString = intercalate ";" . map (\p -> show (particleX p) ++ "," ++ show (particleY p))

-- Main loop -------------------------------------------------------------------

main :: IO ()
main = do
  js_setStars (toJSString (starsToString (uniformStars 150)))
  t0      <- jsNow
  prevRef <- newIORef t0
  wireRef <- newIORef gameWire
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
      Right (ship, particles) ->
        js_drawFrame (posX ship) (posY ship)
                     (toJSString (polyToString (map (rotatePoint (0, 0) (dR ship)) (rocket 0.08))))
                     (toJSString (particlesToString particles))
      Left _ -> pure ()
    readIORef cbRef >>= js_requestAnimationFrame
  writeIORef cbRef cb
  js_requestAnimationFrame cb
