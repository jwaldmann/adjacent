-- | http://www2.stetson.edu/~efriedma/mathmagic/0315.html

import qualified Satchmo.Counting.Binary as C
-- import qualified Satchmo.Counting.Unary as C

import qualified Satchmo.Array as A
import qualified Data.Array as DA
import Satchmo.SAT.Mini ( solve )
import Satchmo.Code
import Satchmo.Boolean
import Control.Monad ( void, forM, guard, when )
import qualified Data.Map as M
import Data.List ( tails )
import System.Environment

-- | cmd line arguments:
-- first: King or Knight, then: numbers
main = do
  neigh : ns <- getArgs
  control (read neigh) $ map read ns

data Neigh = King | Knight deriving ( Read, Show )

-- | search for solution on square board.
-- first increase board size until solution is found,
-- then reduce number of occupied positions.
control neigh ns = do
  let f :: Int -> IO ()
      f w = do
        ok <- work neigh ns w Nothing
        case ok of
          Nothing -> f (w+1)
          Just c -> g w (c-1)
      g :: Int -> Int -> IO ()    
      g w c = do
        ok <- work neigh ns w $ Just c
        case ok of
          Nothing -> return ()
          Just c -> g w $ c-1
  f 1

test = work King [2,5] 7 Nothing

king :: [(Int,Int)]
king = do
    dx <- [ -1 .. 1 ] ; dy <- [-1 .. 1]
    guard $ dx /= 0 Prelude.|| dy /= 0
    return (dx,dy)

knight :: [(Int,Int)]
knight = do
    dx <- [ -2 .. 2 ] ; dy <- [-2 .. 2]
    guard $ 5 == dx^2 + dy^2
    return (dx,dy)

neighbours neigh bnd (x,y) = do
  (dx,dy) <- case neigh  of
    King -> king
    Knight -> knight
  let pos = (x+dx, y+dy)
  guard $ DA.inRange bnd pos
  return (x+dx,y+dy)

work neigh ns w mtotal = do
  print (neigh,ns,w,mtotal)
  out <- solve $ do
    let bnd = ((1,1),(w,w))
    ps <- forM ns $ \ n -> A.unknown bnd boolean
    assert $ ps >>= A.elems
    case mtotal of
      Nothing -> return ()
      Just total -> do
        ok <- C.atmost total $ ps >>= A.elems ; assert [ ok ]
    void $ forM (A.range bnd) $ \ i -> do
      let vs = for ps $ \ p -> p A.! i 
      void $ sequence $ do
        (v:us) <- tails vs ; u <- us
        return $ assert $ map Satchmo.Boolean.not [ v, u]
    void $ forM (zip3 ns ps $ tail ps ++ [head ps]) $ \ (n,p,q) -> 
      void $ forM (A.assocs p) $ \ ((x,y),v) -> do
        -- number of neighbours:
        ok <- C.exactly n $ do
          pos <- neighbours neigh bnd (x,y)
          return $ q A.! pos
        assert [ Satchmo.Boolean.not v, ok ]
        -- for minimality: each position that is occupied,
        -- should have a reason to be 
        when False $ assert $ Satchmo.Boolean.not ( q A.! (x,y)) : do
          pos <- neighbours neigh bnd (x,y)
          return $ p A.! pos
    return $ decode ps
  case out of
    Just ps -> do
      when False $ print (ps :: [ DA.Array (Int,Int) Bool ] )
      let m = M.fromList $ do
            (c,p) <- zip [ 'A' .. ] ps ; (i,True) <- DA.assocs p; return (i, c)
      let c = length $ filter id $ ps >>= DA.elems
      putStrLn $ unlines $ ( "occupied positions: " ++ show c ) : do
        y <- [1..w]
        return $ do
          x <- [1..w]
          [ M.findWithDefault '.' (x,y) m , ' ' ]
      return $ Just c
    Nothing -> return Nothing
          
    
for = flip map
