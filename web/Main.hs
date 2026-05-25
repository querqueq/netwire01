{-# LANGUAGE JavaScriptFFI #-}

-- Milestone A1 of the web port: a requestAnimationFrame-driven loop in
-- Haskell. This proves the per-frame Haskell <-> JS callback mechanism that
-- the game loop is built on, before the netwire FRP core is introduced.
-- Haskell holds the rotation angle, advances it each frame, and hands it to
-- the JS canvas renderer (index.html). Visible result: a spinning polygon.
module Main where

import Data.IORef
import GHC.JS.Foreign.Callback (Callback, asyncCallback)

foreign import javascript unsafe "((f) => { globalThis.requestAnimationFrame(f); })"
  requestAnimationFrame :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "((a) => { globalThis.drawScene(a); })"
  drawScene :: Double -> IO ()

-- Radians advanced per frame (~60fps -> ~1.2 rad/s).
step :: Double
step = 0.02

main :: IO ()
main = do
  angleRef <- newIORef 0
  cbRef    <- newIORef (error "callback not yet initialised")
  cb <- asyncCallback $ do
    angle <- (+ step) <$> readIORef angleRef
    writeIORef angleRef angle
    drawScene angle
    readIORef cbRef >>= requestAnimationFrame
  writeIORef cbRef cb
  requestAnimationFrame cb
