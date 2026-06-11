{-# LANGUAGE RecordWildCards #-}

-- Desktop-side (OpenGL/GLFW) particle rendering and interactive demos. The
-- particle wires themselves (Particle, particle, expel, thruster, explosion,
-- boom, ...) live in netwire01-core's Core.Particles, shared with the web
-- frontend, and are re-exported here so existing desktop code keeps importing
-- Particles.
module Particles (module Core.Particles, module Particles) where

import Prelude hiding ((.)) -- To use (.) in the scope of Categories instead
import Control.Wire
import FRP.Netwire
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import System.Random
import Control.Monad hiding (unless,when)
import Lib
import Core.Particles

--testExpel = testParticle $ expel 4 200 (randomRs (2*pi-0.3,2*pi+0.3) $ mkStdGen 11) thrustParticleSpeed [] . circling (-1)
testFlyCircle = testParticle $ thruster . circling (-1)
testFlyUp = testParticle $ thruster . swirlUp (0,-1)

testExplosion v = testParticle $ boom (0,0) v

renderParticles :: [Particle] -> IO ()
renderParticles = GL.renderPrimitive GL.Points . mapM_ renderPoint . map (\Particle {..} -> (particleX, particleY))

renderThrustParticles :: [Particle] -> IO ()
renderThrustParticles = GL.renderPrimitive GL.Points . mapM_ (\Particle {..} -> do
    GL.color $ GL.Color4 1 0.7 (0 :: GL.GLfloat) 0.5
    renderPoint (particleX,particleY)
    --renderPoint (particleX+size,particleY)
    --renderPoint (particleX+size,particleY+size)
    --renderPoint (particleX,particleY+size)
    )
    where size = 0.007

renderXYcoloredParticles :: [Particle] -> IO ()
renderXYcoloredParticles = GL.renderPrimitive GL.Points . mapM_ (\Particle {..} -> do
    let r = (particleX + 1) / 1.7
        g = (particleY + 1) / 1.7
        b = (r + g) / 2
    GL.color $ GL.Color4 r g b 1
    renderPoint (particleX,particleY)
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
                Left _ -> runNetwork sess' wire'
                Right particles -> do
                    GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
                    GL.clear [GL.ColorBuffer]
                    renderXYcoloredParticles particles
                    GL.loadIdentity
                    GL.perspective 1 1 1 100000
                    GL.flush
                    GLFW.swapBuffers window
                    runNetwork sess' wire'

testParticle :: Wire (Timed NominalDiffTime ()) () IO a [Particle] -> IO ()
testParticle wire = do
    GLFW.init
    (Just window) <- GLFW.createWindow 800 800 "Particle Demo" Nothing Nothing
    GLFW.makeContextCurrent (Just window)
    runParticle window wire
    GLFW.destroyWindow window
    GLFW.terminate
