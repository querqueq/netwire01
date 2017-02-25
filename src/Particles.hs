{-# LANGUAGE RecordWildCards #-}

module Particles where

import Prelude hiding ((.)) -- To use (.) in the scope of Categories instead
import Control.Wire 
import FRP.Netwire 
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.FTGL as FTGL
import System.Random
import Control.Monad hiding (unless,when)
import Numeric 
import Lib
import FRP.Netwire.Input
import FRP.Netwire.Input.GLFW
import Debug.Trace

data Particle = Particle
    { particleX :: Float
    , particleY :: Float
    , particleV :: Float
    } deriving Show

explosion :: (HasTime t s, Monad m) => Float -> Float -> Wire s () m a Float
explosion speed accel = when (>0) . integral speed . pure accel

randDelayWiresWith :: (Fractional t, HasTime t s, Monoid e, Monad m) => (Float,Float) -> Wire s e m a b -> [Wire s e m a b] -> [Wire s e m a b]
randDelayWiresWith (f,t) placeholder wires = zipWith 
    (\w t -> for t . placeholder --> w) 
    wires 
    $ map (fromRational . toRational) $ randomRs (f,t) $ mkStdGen 3

--particles :: (HasTime t s, Monad m) => (Float, Float) -> Float -> Float -> Wire s () m a [Point]
--particles origin speed accel = sequenceA $ map (particle origin $ explosion speed accel) [0..360]

thruster origin speed r = 
      particleCone origin (randDelayWiresWith (0,10) (pure 0) 
    $ replicate (truncate $ 1000 * speed ) 
    $ explosion speed (-speed)) r (0.3)

particleCone :: (HasTime t s, Monad m) => (Float, Float) -> [Wire s () m a Float] -> Float -> Float -> Wire s () m a [Particle]
particleCone origin speedWires offsetR r = sequenceA $ map recycle $ zipWith (particle origin) speedWires $ randomRs range (mkStdGen 1)
    where range = (offsetR-r/2,offsetR+r/2)

particle :: (HasTime t s, Monad m) => (Float, Float) -> Wire s () m a Float -> Float -> Wire s () m a Particle
particle (x,y) speedWire r = Particle <$> (fst <$> pos) <*> (snd <$> pos) <*> speedWire
    where pos = (\(x',y') -> (x+x',y+y')) . (\d -> rotatePoint (0,0) r (d, 0)) <$> integral 0 . speedWire

recycle :: Monad m => Wire s e m a b -> Wire s e m a b
recycle p = p --> recycle p

renderParticles :: [Particle] -> IO ()
renderParticles = GL.renderPrimitive GL.Points . mapM_ renderPoint . map (\Particle {..} -> (particleX, particleY))

renderThrustParticles :: [Particle] -> IO ()
renderThrustParticles = GL.renderPrimitive GL.Quads . mapM_ (\Particle {..} -> do
    GL.color $ GL.Color4 1 (0.6 - particleV) (0 :: GL.GLfloat) 0.5
    renderPoint (particleX,particleY)
    renderPoint (particleX+0.005,particleY)
    renderPoint (particleX+0.005,particleY+0.005)
    renderPoint (particleX,particleY+0.005)
    )

runParticle :: GLFW.Window -> Wire (Timed NominalDiffTime ()) () IO a [Particle] -> IO ()
runParticle window particleWire = do
    g <- getStdGen
    runNetwork clockSession_ particleWire
    where runNetwork sess wire = do
            GLFW.pollEvents
            (st,sess') <- stepSession sess
            (particles,wire') <- stepWire wire st $ Right undefined
            shouldClose <- GLFW.windowShouldClose window
            if shouldClose
            then return ()
            else case particles of
                Left _ -> return ()
                Right particles -> do
                    GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
                    GL.clear [GL.ColorBuffer]
                    renderThrustParticles particles 
                    GL.flush
                    GLFW.swapBuffers window
                    runNetwork sess' wire'

testThrust = testParticle $ thruster (0,0) (0.5) pi

testExplosion speed = testParticle 
    $ particleCone (0,0) 
        (randDelayWiresWith (1,200) (pure 0) 
        $ replicate (truncate $ 1000 * speed ) 
        $ explosion speed (-speed) 
        )
    0 (pi*2)

testParticle :: Wire (Timed NominalDiffTime ()) () IO a [Particle] -> IO ()
testParticle wire = do
    GLFW.init
    (Just window) <- GLFW.createWindow 800 800 "Particle Demo" Nothing Nothing
    GLFW.makeContextCurrent (Just window)
    runParticle window wire
    GLFW.destroyWindow window
    GLFW.terminate
