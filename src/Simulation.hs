module Simulation where

import Data.Vec.Packed
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Simulate hiding (scale)
import Debug.Trace

data Atom = Atom
  { pos :: Vec2F,
    speed :: Vec2F
  } deriving Show

world =
  [ Atom (Vec2F (-1) 0) (Vec2F 1 (-1)),
    Atom (Vec2F 2 3) (Vec2F 1 1),
    Atom (Vec2F 2 4) (Vec2F 1 1),
    Atom (Vec2F 2 4) (Vec2F 2 1),
    Atom (Vec2F 2 4) (Vec2F 1 (-2)),
    Atom (Vec2F 1 (-2)) (Vec2F (-2) 0)
  ]

window = InWindow "Simulation" (800, 800) (600, 200)

render :: [Atom] -> IO Picture
render world = return $ pictures $ renderAtom <$> (trace (show world) world)

renderAtom :: Atom -> Picture
renderAtom (Atom (Vec2F x y) _ ) = translate x y $ color white $ circleSolid 1

step :: (ViewPort -> Float -> [Atom] -> IO [Atom])
step vp time world = return $ addVelocity <$> (calculateVelocity world) <$> world

calculateVelocity :: [Atom] -> Atom -> Atom
calculateVelocity world atom@(Atom pos speed) = Atom pos $ speed - (Vec2F dragForce dragForce  * worldDrag)
  where worldDrag = sum $ toVectors atom <$> world
        dragForce =  (1 / lengthVec worldDrag) :: Float

lengthVec :: Vec2F -> Float
lengthVec (Vec2F x y) = sqrt ((x * x) + (y * y))

--scaleVec2F :: Float -> Vec2F -> Vec2F
--scaleVec2F s v

toVectors :: Atom -> Atom -> Vec2F
toVectors (Atom pos1 _ ) (Atom pos2 _ ) = (traceShow (pos1 - pos2) (pos1 - pos2))

addVelocity :: Atom -> Atom
addVelocity (Atom pos speed) = Atom (pos + speed) speed

startSimulation :: IO ()
startSimulation = simulateIO window black 60 world render step
