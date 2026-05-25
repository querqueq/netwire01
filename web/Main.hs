{-# LANGUAGE JavaScriptFFI #-}

-- Milestone A2 of the web port: drive the render loop with a netwire FRP
-- wire stepped by a real wall-clock time delta, instead of a hardcoded
-- per-frame increment. This proves netwire and its dependency tree compile
-- and run under the GHC JavaScript backend.
--
-- The rotation angle is produced by `integral` (a netwire wire) integrating
-- a constant angular velocity over time. Time deltas come from
-- performance.now(); we use `Timed Double ()` as the session step so we
-- avoid the `time` package's clock FFI.
module Main where

import Prelude hiding ((.), id)
import Control.Wire
import FRP.Netwire
import Data.IORef
import GHC.JS.Foreign.Callback (Callback, syncCallback, OnBlocked(ContinueAsync))

foreign import javascript unsafe "((f) => { globalThis.requestAnimationFrame(f); })"
  requestAnimationFrame :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "((a) => { globalThis.drawScene(a); })"
  drawScene :: Double -> IO ()

foreign import javascript unsafe "(() => { return globalThis.performance.now(); })"
  jsNow :: IO Double

-- Rotation angle in radians, integrating a constant 1.2 rad/s.
spinWire :: (HasTime t s, Monad m) => Wire s () m a Double
spinWire = integral 0 . pure 1.2

main :: IO ()
main = do
  t0      <- jsNow
  prevRef <- newIORef t0
  wireRef <- newIORef spinWire
  cbRef   <- newIORef (error "callback not yet initialised")
  cb <- syncCallback ContinueAsync $ do
    now  <- jsNow
    prev <- readIORef prevRef
    writeIORef prevRef now
    let dt = (now - prev) / 1000          -- milliseconds -> seconds
    wire <- readIORef wireRef
    (result, wire') <- stepWire wire (Timed dt ()) (Right ())
    writeIORef wireRef wire'
    case result of
      Right angle -> drawScene angle
      Left _      -> pure ()
    readIORef cbRef >>= requestAnimationFrame
  writeIORef cbRef cb
  requestAnimationFrame cb
