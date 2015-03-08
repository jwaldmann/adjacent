module Constraint where

import qualified Satchmo.Counting.Binary as C
-- import qualified Satchmo.Counting.Unary as C

import qualified Satchmo.Array as A
import qualified Data.Array as DA
import Satchmo.SAT.Mini ( solve )
import Satchmo.Code
import Prelude hiding (and, or, not)
import Satchmo.Boolean
import Control.Monad ( void, forM, guard, when, zipWithM )
import qualified Data.Map as M
import Data.List ( tails, intersperse, isPrefixOf )

import Render
import System.Directory


-- | We can also use other definitions of adjacent. Instead of king
-- moves, we could use moves of another piece with 8 symmetric moves:
-- * a knight,
-- * congo elephant (which moves 1 or 2 squares horizontally or vertically),
-- * phoenix (which moves 1 square horizontally or vertically or 2 squares diagonally),
-- * frog (which moves 3 squares horizontally or vertically or 1 square diagonally),
-- * or zebra (which moves 2 squares horizontally or vertically, and 3 squares in a perpendicular direction).

data Neigh = Congo
           | Frog
           | King
           | Knight
           | Phoenix  
           | Zebra deriving ( Read, Show )


neighbours neigh bnd (x,y) = do
  (dx,dy) <- case neigh of
    Congo -> do
      dx <- [-2..2] ; dy <- [-2..2]
      guard $ (dx == 0) /= (dy == 0)
      return (dx,dy)
    Frog -> do
      dx <- [ -1 .. 1 ] ; dy <- [-1 .. 1]
      guard $ dx /= 0 Prelude.|| dy /= 0
      return $ if abs dx == abs dy
               then (dx,dy) else (3*dx,3*dy)
    King -> do
      dx <- [ -1 .. 1 ] ; dy <- [-1 .. 1]
      guard $ dx /= 0 Prelude.|| dy /= 0
      return (dx,dy)
    Knight -> do
      dx <- [ -2 .. 2 ] ; dy <- [-2 .. 2]
      guard $ 5 == dx^2 + dy^2
      return (dx,dy)
    Phoenix -> do
      dx <- [ -1 .. 1 ] ; dy <- [-1 .. 1]
      guard $ dx /= 0 Prelude.|| dy /= 0
      return $ if abs dx == abs dy
               then (2*dx,2*dy) else (dx,dy)
    Zebra -> do
      dx <- [ -3 .. 3 ] ; dy <- [-3 .. 3 ]
      guard $ 13 == dx^2 + dy^2
      return (dx,dy)
  let pos = (x+dx, y+dy)
  guard $ DA.inRange bnd pos
  return (x+dx,y+dy)

rotate p = A.array (A.bounds p) $ do
  let ((1,1),(w,w')) {- | w == w' -} = A.bounds p
  ((x,y),v) <- A.assocs p
  return ((w+1-y,x),v)

mirror_diag p = A.array (A.bounds p) $ do
  let ((1,1),(w,w')) | w == w' = A.bounds p
  ((x,y),v) <- A.assocs p
  return ((y, x),v)

mirror_antidiag p = A.array (A.bounds p) $ do
  let ((1,1),(w,w')) {- | w == w' -} = A.bounds p
  ((x,y),v) <- A.assocs p
  return ((w+1-y, w+1-x),v) 

work neigh ns w mtotal = do
  print (neigh,ns,w,mtotal)
  out <- solve $ do
    let bnd = ((1::Int,1::Int),(w,w))
    ps <- forM ns $ \ n -> A.unknown bnd boolean
    assert $ ps >>= A.elems
    forM ps $ \ p -> do
      -- ok <- equalsA p $ mirror_diag p ; assert [ ok ]
      ok <- equalsA p $ rotate p ; assert [ ok ]
      -- ok <- equalsA p $ rotate $ rotate p ; assert [ ok ]
      return ()
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
        when True $ assert $ Satchmo.Boolean.not ( q A.! (x,y)) : do
          pos <- neighbours neigh bnd (x,y)
          return $ p A.! pos
    return $ decode ps
  case out of
    Just ps -> do
      let c = length $ filter id $ ps >>= DA.elems
      let info = [ "neighbours:", show neigh, "degrees:", show ns
                 , "width:", show w, "occupied:", show c ]
      when False $ print (ps :: [ DA.Array (Int,Int) Bool ] )
      render_ascii info ps
      let prefix = concat ( intersperse "-" ( show neigh : map show ns  ) ) ++ "."
          fname = prefix ++ show c ++ ".html"
      fs <- getDirectoryContents "."
      forM fs $ \ f -> when (isPrefixOf prefix f) $ do
        putStrLn $ unwords [ "remove", f ]
        removeFile f
      render_html fname info ps
      return $ Just c
    Nothing -> return Nothing

equalsA a b | A.bounds a == A.bounds b = 
  zipWithM equals2 (A.elems a) (A.elems b) >>= and
  

    
for = flip map
