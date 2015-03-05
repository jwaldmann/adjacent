-- | http://www2.stetson.edu/~efriedma/mathmagic/0315.html

import qualified Satchmo.Counting.Binary as C

import qualified Satchmo.Array as A
import qualified Data.Array as DA
import Satchmo.SAT.Mini ( solve )
import Satchmo.Code
import Satchmo.Boolean
import Control.Monad ( void, forM, guard, when )
import qualified Data.Map as M
import System.Environment

main = do
  neigh : ns <- getArgs
  control (read neigh) $ map read ns

data Neigh = King | Knight deriving ( Read, Show )

control neigh ns = do
  let f :: Int -> IO ()
      f w = do
        ok <- work neigh ns w Nothing
        case ok of
          False -> f (w+1)
          True -> g w (0, w^2)
      g :: Int -> (Int,Int) -> IO ()    
      g w (lo,hi) = if hi - lo <= 1 then return () else do
        let mid = div (lo+hi) 2
        ok <- work neigh ns w (Just mid)
        case ok of
          False -> g w (mid,hi)
          True -> g w (lo, mid)
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
      ok <- C.atmost 1 $ for ps $ \ p -> p A.! i ; assert [ ok ]    
    void $ forM (zip3 ns ps $ tail ps ++ [head ps]) $ \ (n,p,q) -> 
      void $ forM (A.assocs p) $ \ ((x,y),v) -> do
        ok <- C.exactly n $ do
              (dx,dy) <- case neigh  of
                King -> king
                Knight -> knight
              let pos = (x+dx, y+dy)
              guard $ DA.inRange bnd pos
              return $ q A.! pos
        assert [ Satchmo.Boolean.not v, ok ]      
    return $ decode ps
  case out of
    Just ps -> do
      when False $ print (ps :: [ DA.Array (Int,Int) Bool ] )
      let m = M.fromList $ do
            (c,p) <- zip [ 'A' .. ] ps ; (i,True) <- DA.assocs p; return (i, c)
      putStrLn $ unlines $ do
        y <- [1..w]
        return $ do
          x <- [1..w]
          [ M.findWithDefault '.' (x,y) m , ' ' ]
      return True
    Nothing -> return False
          
    
for = flip map
