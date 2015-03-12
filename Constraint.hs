module Constraint where

import qualified Satchmo.Counting.Binary as BC
import qualified Satchmo.Counting.Unary as UC
import qualified Satchmo.Counting.Direct as DC

import qualified Satchmo.Array as A
import qualified Data.Array as DA
import Satchmo.SAT.Mini ( solve )
import Satchmo.Code
import Prelude hiding (and, or, not)
import qualified Prelude
import Satchmo.Boolean
import Control.Monad ( void, forM, guard, when, zipWithM )
import qualified Data.Map as M
import Data.List ( tails, intersperse, isPrefixOf )

import Config
import Render

import System.Directory


-- | We can also use other definitions of adjacent. Instead of king
-- moves, we could use moves of another piece with 8 symmetric moves:
-- * a knight,
-- * congo elephant (which moves 1 or 2 squares horizontally or vertically),
-- * phoenix (which moves 1 square horizontally or vertically or 2 squares diagonally),
-- * frog (which moves 3 squares horizontally or vertically or 1 square diagonally),
-- * or zebra (which moves 2 squares horizontally or vertically, and 3 squares in a perpendicular direction).


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

transform f p = A.array (A.bounds p) $ do
  let ((1,1),(w,w')) = A.bounds p
  (i,v) <- A.assocs p
  return (f i,v)

sym s p = do
  let ((1,1),(h,w)) = A.bounds p
      flip x = w + 1 - x
  ok <- equalsA p $ transform ( \(x,y) -> case s of
      Anti -> (flip y, flip x)
      Diag -> (y,x)
      Vert -> (x, flip y)
      Hor  -> (flip x, y)
      Rot4 -> (flip y, x)
      Rot2 -> (flip x, flip y)
    ) p
  assert [ ok ]

work config w mtotal = do
  print (config, w, mtotal)
  let ns = degrees config
  out <- solve $ do
    let bnd = ((1::Int,1::Int),(w,w))
    ps <- forM ns $ \ n -> A.unknown bnd boolean
    assert $ ps >>= A.elems
    forM ps $ \ p -> forM (symmetries config) $ \ s -> sym s p
    case mtotal of
      Nothing -> return ()
      Just total -> do
        ok <- BC.atmost total $ ps >>= A.elems ; assert [ ok ]
    void $ forM (A.range bnd) $ \ i -> do
      let vs = for ps $ \ p -> p A.! i 
      void $ sequence $ do
        (v:us) <- tails vs ; u <- us
        return $ assert $ map Satchmo.Boolean.not [ v, u]

    when (Prelude.not $ global config) $ do
      let m = (div w 2, div w 2)
      assert $ do p <- ps ; return $ p A.! m
        
    void $ forM (zip3 ns ps $ tail ps ++ [head ps]) $ \ (n,p,q) -> 
      void $ forM (A.assocs p) $ \ ((x,y),v) -> do
        -- check number of neighbours.
        -- treat positions at the border specially
        let qs = map (\i -> q A.! i)
               $ neighbours (neigh config) bnd (x,y)
            -- FIXME: hard-coded number
            inside = length qs == 8
        let exactly = case counter config of
              Binary -> BC.exactly ; Unary -> UC.exactly ; Direct -> DC.exactly
        let atmost = case counter config of
              Binary -> BC.atmost ; Unary -> UC.atmost ; Direct -> DC.atmost
        if inside
           then do
             case counter config of
                  Binary -> do ok <- BC.exactly n qs ; assert [ not v, ok ]
                  Unary -> do ok <- BC.exactly n qs ; assert [ not v, ok ]
                  Direct -> DC.assert_implies_exactly [ v ] n qs
           else case x <= 2 Prelude.||
                     y <= 2 Prelude.||
                     global config of
             True -> do
               ok <- exactly n qs ; assert [ not v, ok ]
             False -> do
               ok <- atmost n qs ; assert [ not v, ok ]
        -- for minimality: each position that is occupied,
        -- should have a reason to be 
        when (minimal config) $ do
          when (global config Prelude.|| inside) $ do
            assert $ Satchmo.Boolean.not ( q A.! (x,y)) : do
              pos <- neighbours (neigh config) bnd (x,y)
              return $ p A.! pos
    return $ decode ps
  case out of
    Just ps -> do
      let c = length $ filter id $ ps >>= DA.elems
      let info = [ "neighbours:", show (neigh config)
                 , "degrees:", show ns
                 , "width:", show w
                 , "occupied:", show c
                 ]
      when False $ print (ps :: [ DA.Array (Int,Int) Bool ] )
      render_ascii info ps
      let prefix = concat ( intersperse "-" ( show (neigh config) : map show ns  ) ) ++ "."
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
