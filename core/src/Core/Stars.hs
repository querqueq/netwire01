-- Starfield generation for the web frontend (moved from web/Main.hs). The
-- desktop uses its own memoized, chunked 3D starfield in app/Main.hs that is
-- tied to its perspective camera.
module Core.Stars where

import System.Random
import Core.Geometry

-- n stars as (x, y, brightness): positions uniform in [-1,1]^2 (one viewport
-- tile; the renderer wraps the tile around the camera), brightness in
-- [0.2,1].
uniformStars :: Int -> [(FT, FT, FT)]
uniformStars n = take n (go (mkStdGen 42))
  where
    go g = let (x, g1) = randomR (-1, 1)    g
               (y, g2) = randomR (-1, 1)    g1
               (b, g3) = randomR (0.2, 1.0) g2
           in (x, y, b) : go g3
