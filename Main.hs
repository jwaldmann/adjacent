-- | http://www2.stetson.edu/~efriedma/mathmagic/0315.html

import System.Environment

import Constraint
import Render

test = work King [2,5] 7 Nothing

-- | cmd line arguments:
-- first: King or Knight, then: numbers
main = do
  neigh : ns <- getArgs
  control (read neigh) $ map read ns


-- | search for solution on square board.
-- first increase board size until solution is found,
-- then reduce number of occupied positions.
-- then increase board size.
control neigh ns = do
  let f :: Int -> IO ()
      f w = do
        ok <- work neigh ns w Nothing
        case ok of
          Nothing -> f (w+1)
          Just c -> g w c 
      g :: Int -> Int -> IO ()    
      g w have = do
        ok <- work neigh ns w $ Just (have - 1)
        case ok of
          Nothing -> g (w+1) have
          Just c -> g w c
  f 1

