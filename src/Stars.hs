{-# LANGUAGE RecordWildCards #-}

module Stars (skyWire,renderStar,Star) where

import Prelude hiding ((.)) -- To use (.) in the scope of Categories instead
import Control.Wire 
import FRP.Netwire 
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.FTGL as FTGL
import System.Random
import Control.Monad
import Numeric 
import Lib

data Star = Star 
    { starX :: FT
    , starY :: FT
    , starZ :: FT
    , starBrightness :: !FT
    } 

instance Show Star where
    show (Star {..}) = "(" ++ format starX ++ "," ++ format starY ++ ") " ++ format starBrightness ++ "*"

skyWire :: (Fractional t, Monad m, HasTime t s, RandomGen g) => g -> Int -> Wire s () m a [Star]
skyWire g starsN = do
    let xys = zip (take starsN rands) (drop starsN rands)
    let gs = map mkStdGen $ randoms g
    sequenceA $ zipWith pulsarWire gs xys
    where rands = take (starsN * 2) $ randomRs (-1 :: FT,1) g

starPosWire :: Point -> Wire s e m a Point
starPosWire pos = WConst $ Right pos

randStarPosWire :: (RandomGen g) => g -> Wire s e m a Point
randStarPosWire g = do
    let (x:(y:_)) = randoms g
    starPosWire (x,y)

pulsarWire :: (Monad m, RandomGen g, HasTime t s, Fractional t) => g -> Point -> Wire s () m a Star 
pulsarWire g (x,y) = Star
         <$> pure x
         <*> pure y
         <*> pure 0
         <*> alternateByInhibit (wackelkontakt 0.4137 0.992 g . pure base) (pulsateWire base peak pulsateT 0.5) 
         where base = fst $ randomR (0.001,0.002) g
               peak = fst $ randomR (0.005,0.025) g -- 0.01
               pulsateT = 2.5

pulsateWire :: (Fractional t, Monad m, HasTime t s) => FT -> FT -> t -> t -> Wire s () m a FT
pulsateWire base peak totalT holdT = for totalT . integral base .
    (   for changeT . integral target . pure (-target)
    --> for holdT . pure 0 
    --> for changeT . integral (-target) . pure target 
    --> pure 0
    )
    where changeT = (totalT - holdT) / 2
          target = peak - base


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
