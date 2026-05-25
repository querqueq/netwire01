{-# LANGUAGE OverloadedStrings #-}

-- Build probe: does ghcjs-dom compile and link on the GHC JavaScript
-- backend, and is its DOM monad usable directly from IO? If this builds,
-- we refactor the whole game onto ghcjs-dom; if not, we fall back to raw
-- FFI. This file is intentionally throwaway.
module Main where

import GHCJS.DOM (currentDocument)
import GHCJS.DOM.Types (Document)

main :: IO ()
main = do
  doc <- currentDocument :: IO (Maybe Document)
  case doc of
    Just _  -> putStrLn "ghcjs-dom: have document"
    Nothing -> putStrLn "ghcjs-dom: no document"
