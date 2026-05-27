{-# LANGUAGE OverloadedStrings #-}

-- Canvas API probe for the ghcjs-dom refactor: have Haskell create the
-- <canvas>, get its 2D context, and draw a background + a triangle. This
-- locks the exact ghcjs-dom canvas API (module/function names and arg
-- types) before the full scene + loop + input are ported. index.html is
-- reduced to just CSS + the script tag; Haskell builds the DOM.
module Main where

import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.Document (createElement, getBodyUnchecked)
import GHCJS.DOM.Node (appendChild_)
import GHCJS.DOM.HTMLCanvasElement (setWidth, setHeight, getContextUnchecked)
import GHCJS.DOM.Types
  ( HTMLCanvasElement(..), CanvasRenderingContext2D(..), JSString, unsafeCastTo )
import GHCJS.DOM.CanvasFillStrokeStyles (setFillStyle)
import GHCJS.DOM.CanvasRect (fillRect)
import GHCJS.DOM.CanvasDrawPath (beginPath, fill)
import GHCJS.DOM.CanvasPath (moveTo, lineTo, closePath)

main :: IO ()
main = do
  doc      <- currentDocumentUnchecked
  body     <- getBodyUnchecked doc
  canvasEl <- createElement doc ("canvas" :: JSString)
  canvas   <- unsafeCastTo HTMLCanvasElement canvasEl
  setWidth  canvas 600
  setHeight canvas 600
  appendChild_ body canvas

  rc  <- getContextUnchecked canvas ("2d" :: JSString)
  ctx <- unsafeCastTo CanvasRenderingContext2D rc

  setFillStyle ctx ("#000" :: JSString)
  fillRect ctx 0 0 600 600

  beginPath ctx
  moveTo ctx 300 150
  lineTo ctx 420 420
  lineTo ctx 180 420
  closePath ctx
  setFillStyle ctx ("#fff" :: JSString)
  fill ctx
