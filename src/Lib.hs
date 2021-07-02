module Lib
    ( mainNewton
    , mainVerlet
    , mainVerletSquare
    ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Picture
import Linear.V2
import Linear.Vector
import Linear.Metric
import System.Random

type Position       = V2 Float
type Velocity       = V2 Float
type Acceleration   = V2 Float
type Force          = V2 Float
type Index          = Int
type TimeStep       = Float


data Particle = Particle {
    idx :: Index,
    pos :: Position,
    vel :: Velocity
}

type Model = [Particle]
instance Eq Particle where
    ballA == ballB = idx ballA == idx ballB


windowDisplay :: Display
windowDisplay = InWindow "Md in Haskell" (800, 800) (100, 100)

simulationRate :: Int
simulationRate = 60


drawParticle :: Particle -> Picture
drawParticle (Particle _ (V2 x y) _) =
    translate x' y' $ color (circleSolid $ toPixels dotSize)
    where
        x' = toPixels x
        y' = toPixels y
        color = Color (withAlpha 0.8 blue)

toPixels :: Float -> Float
toPixels = (* 100.0)

dotSize :: Float
dotSize = 0.1

drawWalls :: Picture
drawWalls = lineLoop $ rectanglePath (toPixels aLength) (toPixels bLength)


drawingFunc :: Model -> Picture
drawingFunc = pictures . (:) drawWalls . fmap drawParticle



boundaryCondition :: Particle -> V2 Float
boundaryCondition (Particle _ (V2 x y) _)
    | (x' > aLength/2) && (y' > bLength/2) = V2 (-1) (-1)
    |  x' > aLength/2                      = V2 (-1)   1
    |  y' > bLength/2                      = V2   1 (-1)
    | otherwise                            = V2   1    1
    where
        x' = abs x + dotSize
        y' = abs y + dotSize

aLength, bLength :: Float
aLength = 7.0
bLength = 7.0

newton :: TimeStep -> [Particle]-> [Particle]
newton dt [] = []
newton dt [Particle idx pos vel] = [Particle idx pos' vel]
    where
        pos' = pos + vel^*dt
newton dt (p@(Particle idx pos vel):particle) = newp : newton dt particle
    where
        newp = Particle idx (pos + vel^*dt) vel


newtonBounce :: Float -> [Particle] -> [Particle]
newtonBounce dt [] = []
newtonBounce dt [particle@(Particle idx pos vel)] = [Particle idx pos' vel']
    where
        transVec = boundaryCondition particle
        vel' = transVec * vel
        pos' = pos + vel' ^* dt
newtonBounce dt (p:articles) = head(newtonBounce dt [p]) : newtonBounce dt articles



verletStep :: TimeStep -> Model -> Model
verletStep dt particles =
    let
        oldF = calcForces particles
        oldA = fmap (^/ m) oldF
        newPos = updatePositions dt particles oldA  
        newF = calcForces newPos
        newA = fmap (^/ m) newF
        addedF = oldA    ^+^ newA
        newParts = updateVelocities dt newPos addedF
    in newParts

updatePosition :: TimeStep -> Particle -> Acceleration ->Particle
updatePosition dt (Particle idx pos vel) acc = Particle idx newPos vel
    where
        newPos = pos ^+^ velPart ^+^ accPart
        velPart = vel ^* dt
        accPart = acc ^* (0.5* dt**2)

updateVelocity :: TimeStep -> Particle -> Acceleration -> Particle
updateVelocity dt particle acc = Particle idx pos vel'
    where
        (Particle idx pos vel) = particle
        transVec = boundaryCondition particle
        vel' = transVec * (vel + (0.5*dt) *^ acc)

updatePositions, updateVelocities :: TimeStep -> [Particle] -> [Acceleration] -> [Particle]
updatePositions dt = zipWith (updatePosition dt)
updateVelocities dt = zipWith (updateVelocity dt)

calcForceBetween :: Particle -> Particle -> Force
calcForceBetween particleA particleB
    | particleA == particleB = V2 0.0 0.0
    | otherwise = rep - att
        where
            rep = repulsion posA posB
            att = attraction posA posB
            posA = pos particleA
            posB = pos particleB

m, epsilon, sigma, sigma6, sigma12 :: Float
m = 18
epsilon = 12.57
sigma = 0.335
sigma6 = sigma**6
sigma12 = sigma **12

repulsion, attraction :: Position -> Position -> Force
repulsion posA posB = (epsilon * 48.0 * sigma12/divisor) *^ r
    where
        divisor = norm r^14
        r = posB ^-^ posA
attraction posA posB = (epsilon * 24.0 * sigma6/divisor) *^ r
    where
        divisor = norm r^8
        r = posB ^-^ posA

calcForceOnOne :: Particle -> [Particle] -> [Force]
calcForceOnOne particle = fmap (calcForceBetween particle)

calcForceAcc :: [Particle] -> [Particle] -> [Force]
calcForceAcc [] _ = []
calcForceAcc [particle] particles = calcForceOnOne particle particles
calcForceAcc (p:articles) particles = calcForceOnOne p particles ^+^ calcForceAcc articles particles


calcForces :: [Particle] -> [Force]
calcForces particles = calcForceAcc particles particles

mainNewton = simulate windowDisplay white simulationRate initialModel drawingFunc updateFunc
    where
        initialModel :: Model
        initialModel = [Particle 1 (V2 0.0 0.0) (V2 1.0 0.0)]

        updateFunc :: ViewPort  -> TimeStep -> Model -> Model
        updateFunc _ dt = newtonBounce dt

mainVerlet :: IO()
mainVerlet = simulate windowDisplay white simulationRate initialModel drawingFunc updateFunc
    where
        initialModel = [
            Particle 1 (V2 0.3 0.0) (V2 0.0 0.0)
          , Particle 2 (V2 (-0.3) 0.0) (V2 0.0 0.0)]
        updateFunc _ dt = verletStep dt

squareLatticeModel :: Int -> [Particle]
squareLatticeModel n = zipWith3 Particle idxs poss vels
    where
        idxs = [1..(n^2)]
        poss = squareLattice n n
        vels = replicate (n^2) (V2 0.0 0.0)


squareLattice :: Int -> Int -> [Position]
squareLattice _ 0 = []
squareLattice dim acc = latticeRow dim dim yPos ++ squareLattice dim (acc -1)
    where
        dy = bLength/fromIntegral (dim+1)
        yPos = bLength/2 - (fromIntegral acc*dy)

latticeRow :: Int -> Int -> Float -> [Position]
latticeRow _ 0 _ = []
latticeRow dim acc yPos = V2 xPos yPos : latticeRow dim (acc-1) yPos
    where
        dx = aLength/fromIntegral( dim+1)
        xPos = aLength/2 - fromIntegral acc*dx

mainVerletSquare :: IO()
mainVerletSquare = simulate windowDisplay white simulationRate initialModel drawingFunc updateFunc
    where
        initialModel = squareLatticeModel 4
        updateFunc _ dt = verletStep dt
