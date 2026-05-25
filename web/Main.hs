{-# LANGUAGE JavaScriptFFI #-}

-- Toolchain spike for the web port: prove that Haskell compiled with the
-- GHC JavaScript backend runs in the browser and can drive an HTML5 canvas.
-- The real game logic (netwire FRP) is ported in a later step; here Haskell
-- just decides how many sides the polygon has and hands it to a JS routine
-- defined in index.html. If the on-page label updates, Haskell's main ran.
module Main where

foreign import javascript unsafe "((n) => { globalThis.drawScene(n); })"
  drawScene :: Int -> IO ()

sides :: Int
sides = 7

main :: IO ()
main = drawScene sides
