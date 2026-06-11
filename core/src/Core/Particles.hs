-- Particle wires (thruster exhaust, explosions, demo movements), moved here
-- from src/Particles.hs. Rendering stays with the frontends: the desktop
-- draws GL points (src/Particles.hs), the web draws canvas dots
-- (web/index.html). This module must build on both GHC 8.0.2 (stack) and
-- GHC 9.12 (wasm32-wasi).
module Core.Particles where

import Prelude hiding ((.)) -- To use (.) in the scope of Categories instead
import Control.Wire
import FRP.Netwire
import System.Random
import Data.Either (isRight)
import Core.Geometry

data Particle = Particle
    { particleX  :: FT
    , particleY  :: FT
    , particleV  :: FT
    } deriving Show

swirlUp :: (HasTime t s, Monad m) => (FT,FT) -> Wire s () m a (Point,Point,FT,FT)
swirlUp (x,y) = (,,,) <$> pos <*> pure (0,0) <*> r <*> pure 0.5
    where pos = (,) <$> ((/4) . sin <$> integral x . pure (pi/2)) <*> integral y . pure 0.4
          r = -pi/2

circling :: (HasTime t s, Monad m) => FT -> Wire s () m a (Point,Point,FT,FT)
circling raccel = (\r' pos'@(x,y) v -> (rotatePoint (0,0) r' pos',(0,0),r',v)) <$> r <*> pos <*> pure 0.5
    where r = integral 0 . pure raccel
          pos = (,) <$> 0 <*> (-0.8)

explosion :: (HasTime t s, Fractional t, Monad m) => FT -> FT -> Wire s () m a FT
explosion speed accel = when (if accel > 0 then (<0) else (>0)) . integral speed . pure accel --> for 0.3 . pure 1000 --> pure 0
                                                                 -- FIXME replace workaround with inhibit
                                                                 -- and findout how to remove inhibted
                                                                 -- wires from a list

thrustParticleSpeed :: (HasTime t s, Fractional t, Monad m) => FT -> Wire s () m a FT
thrustParticleSpeed v = pure v--explosion v (-v*2)

randDelayWiresWith :: (Fractional t, HasTime t s, Monoid e, Monad m) => (FT,FT) -> Wire s e m a b -> [Wire s e m a b] -> [Wire s e m a b]
randDelayWiresWith (f,t) placeholder wires = zipWith
    (\w t -> for t . placeholder --> w)
    wires
    $ map (fromRational . toRational) $ randomRs (f,t) $ mkStdGen 3

thruster :: (HasTime t s, Monad m, Fractional t) => Wire s () m (Point,Point,FT,FT) [Particle]
thruster = expel 3 0.3 (mkStdGen 12) thrustParticleSpeed []

expel :: (HasTime t s, Monad m, Fractional t, RandomGen g)
                => Int
                -> FT
                -> g
                -> (FT -> Wire s () m a FT)
                -> [Wire s () m a Particle]
                -> Wire s () m (Point,Point,FT,FT) [Particle]
expel newN angleMax seeder speedWire particleWires = mkGen $ \ds (origin,speed,r,a) -> do
    let (angleSeed,g') = random seeder
        angles = take newN $ randomRs (-angleMax,angleMax) $ mkStdGen angleSeed
        newParticle angle = for (fromRational $ toRational $ 1 - abs angle)  . particle origin speed (speedWire a) (angle+r)
        newParticles = map newParticle angles
        updatedParticleWires = if a /= 0 then newParticles ++ particleWires else particleWires
    (particles,particleWires') <- fmap (unzip.(filter (\(p,_) -> isRight p))) $ sequenceA $ map (\w -> stepWire w ds $ Right undefined) updatedParticleWires
    return (sequenceA particles, expel newN angleMax g' speedWire particleWires')

particleCone :: (HasTime t s, Monad m) => (FT, FT) -> [Wire s () m a FT] -> FT -> FT -> Wire s () m a [Particle]
particleCone origin speedWires offsetR r = sequenceA $ zipWith (particle origin (0,0)) speedWires $ randomRs range (mkStdGen 1)
    where range = (offsetR-r/2,offsetR+r/2)

particle :: (HasTime t s, Monad m) => (FT, FT) -> (FT,FT) -> Wire s () m a FT -> FT -> Wire s () m a Particle
particle (x,y) (vX,vY) speedWire r = Particle <$> posX <*> posY <*> speedWire
    where vs   = (\(vX',vY') -> (vX+vX',vY+vY')) . (\d -> rotatePoint (0,0) r (d, 0)) <$> speedWire
          posX = integral x . (fst <$> vs)
          posY = integral y . (snd <$> vs)

recycle :: Monad m => Wire s e m a b -> Wire s e m a b
recycle p = p --> recycle p

boom :: (HasTime t s, Fractional t, Monad m) => (FT,FT) -> FT -> Wire s () m a [Particle]
boom (x,y) v = mconcat $ map cone
    [randDelayWiresWith (0.2,0.6) 0 $ map (\v -> explosion v (-v*3)) $ vs 100
    ,randDelayWiresWith (0.0,0.3) 0 $ map (\v -> explosion v (-v*1)) $ vs 150
    ,randDelayWiresWith (0.0,0.4) 0 $ map (\v -> explosion (v*1.5) (-v)) $ vs 300
    ]
    where --delays = randDelayWiresWith (0.2,3) 0
          cone ws = particleCone (x,y) ws 0 (pi*2)
          vs n = take n $ randomRs (v-v/2,v+v/2) $ mkStdGen 10
