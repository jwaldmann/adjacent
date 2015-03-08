-- | http://www2.stetson.edu/~efriedma/mathmagic/0315.html

import Constraint
import Render
import Config

test = work config0 7 Nothing

-- | cmd line arguments:
-- first: King or Knight, then: numbers
main = do
  config <- parse 
  control config

control config | not (global config) = do
  let f w = do
        ok <- work config w Nothing
        case ok of
          Nothing -> do
            putStrLn "no local configuration => unsolvable"
          Just p -> do
            f (w+1)
  f (width config)

-- | search for solution on square board.
-- first increase board size until solution is found,
-- then reduce number of occupied positions.
-- then increase board size.
control config | global config = do
  let f :: Int -> IO ()
      f w = do
        ok <- work config w Nothing
        case ok of
          Nothing -> f (w+1)
          Just c -> g w c 
      g :: Int -> Int -> IO ()    
      g w have = do
        ok <- work config w $ Just (have - 1)
        case ok of
          Nothing -> g (w+1) have
          Just c -> g w c
  f (width config)

