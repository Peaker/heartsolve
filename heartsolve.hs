-- Solve level 15 of Valentine's day floors of the 100 floors game:

{-# OPTIONS -Wall -O2 #-}
import Control.Monad.State
import Data.Map (Map, (!))
import Vector2 (Vector2(..))
import qualified Data.Map as Map

data Heart = Yellow | Brown
  deriving (Eq, Ord, Show)

rotateRight :: Vector2 Int -> Vector2 Int
rotateRight = (Map.fromList (zip rotations (tail rotations)) !)
  where
    rotations =
      [ Vector2    1    0, Vector2    1   1, Vector2    0    1
      , Vector2 (-1)    1, Vector2 (-1)   0, Vector2 (-1) (-1)
      , Vector2 0    (-1), Vector2    1 (-1), Vector2   1    0
      ]

positions :: [Vector2 Int]
positions =
  [ Vector2 0 0             , Vector2 2 0
  , Vector2 0 1             , Vector2 2 1
  , Vector2 0 2, Vector2 1 2, Vector2 2 2
  ]

nextDir :: Vector2 Int -> Vector2 Int -> Map (Vector2 Int) Heart -> Vector2 Int
nextDir dir pos heartMap =
  case Map.lookup pos heartMap of
  Nothing -> dir
  Just Yellow -> rotateRight $ rotateRight dir
  Just Brown -> rotateRight $ rotateRight $ rotateRight dir

isSolution :: Map (Vector2 Int) Heart -> Bool
isSolution initHeartMap =
  evalState loop (Vector2 1 0, Vector2 0 1, initHeartMap)
  where
    loop = do
      (pos, dir, heartMap) <- get
      let newDir = nextDir dir pos heartMap
          newPos@(Vector2 x y) = pos + newDir
          newHeartMap = Map.delete pos heartMap
      put (newPos, newDir, newHeartMap)
      if x == 1 && y == 3
        then return True
        else
          if x < 0 || x > 2 || y < 0 || y > 2
          then return False
          else loop

choosePermutations :: Int -> Int -> [[Heart]]
choosePermutations 0 b = [replicate b Brown]
choosePermutations y 0 = [replicate y Yellow]
choosePermutations y b = do
  heart <- [Yellow, Brown]
  rest <- case heart of
    Yellow -> choosePermutations (y-1) b
    Brown -> choosePermutations y (b-1)
  return (heart : rest)

main :: IO ()
main = do
  let perms = choosePermutations 4 3
      heartMaps = [Map.fromList (zip positions perm) | perm <- perms]
  mapM_ (mapM_ print . Map.keys . Map.filter (== Brown)) $ filter isSolution heartMaps
