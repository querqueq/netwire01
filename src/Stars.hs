{-# LANGUAGE RecordWildCards #-}

-- Desktop-side (OpenGL/GLFW) rendering and demo runner for the pulsating
-- sky. The sky wires themselves (Star, skyWire, pulsarWire, ...) live in
-- netwire01-core's Core.Stars, shared with the rest of the game logic, and
-- are re-exported here so existing desktop code keeps importing Stars.
module Stars (skyWire,renderStar,Star) where

import Prelude hiding ((.)) -- To use (.) in the scope of Categories instead
import Control.Wire
import FRP.Netwire
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.FTGL as FTGL
import System.Random
import Control.Monad
import Lib
import Core.Stars

renderStar :: Star -> IO ()
renderStar (Star {..}) = GL.renderPrimitive GL.Lines
    $ mapM_ (\p -> do
        GL.color $ GL.Color3 1 1 (1 :: GL.GLfloat)
        renderPoint p)
    $ [(x-b,y),(x+b,y)
      ,(x,y-b),(x,y+b)
      ,(x-b',y+b'),(x+b',y-b')
      ,(x+b',y+b'),(x-b',y-b')
      ]
    where x = starX; y = starY; b = starBrightness; b' = starBrightness / 2

runStars :: GLFW.Window -> IO ()
runStars window = do
    g <- getStdGen
    runNetwork clockSession_ $ skyWire g 678
    where runNetwork sess wire = do
            GLFW.pollEvents
            (st,sess') <- stepSession sess
            (stars,wire') <- stepWire wire st $ Right undefined
            shouldClose <- GLFW.windowShouldClose window
            if shouldClose
            then return ()
            else case stars of
                Left _ -> return ()
                Right stars -> do
                    GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
                    GL.clear [GL.ColorBuffer]
                    mapM_ renderStar stars
                    GL.flush
                    GLFW.swapBuffers window
                    runNetwork sess' wire'

testStars :: IO ()
testStars = do
    GLFW.init
    (Just window) <- GLFW.createWindow 1080 1080 "Stars Demo" Nothing Nothing
    GLFW.makeContextCurrent (Just window)
    font <- FTGL.createBitmapFont "DroidSansMono.ttf"
    FTGL.setFontFaceSize font 24 72
    runStars window
    GLFW.destroyWindow window
    GLFW.terminate
