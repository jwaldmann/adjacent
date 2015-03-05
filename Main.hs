-- | http://www2.stetson.edu/~efriedma/mathmagic/0315.html

{-# LANGUAGE QuasiQuotes #-}

import qualified Satchmo.Counting.Binary as C
-- import qualified Satchmo.Counting.Unary as C

import qualified Satchmo.Array as A
import qualified Data.Array as DA
import Satchmo.SAT.Mini ( solve )
import Satchmo.Code
import Satchmo.Boolean
import Control.Monad ( void, forM, guard, when )
import qualified Data.Map as M
import Data.List ( tails, intersperse )
import System.Environment

import Text.Hamlet (shamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)

-- | cmd line arguments:
-- first: King or Knight, then: numbers
main = do
  neigh : ns <- getArgs
  control (read neigh) $ map read ns

data Neigh = King | Knight | Zebra deriving ( Read, Show )

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
          Just c -> g w (have - 1 )
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

zebra :: [(Int,Int)]
zebra = do
    dx <- [ -3 .. 3 ] ; dy <- [-3 .. 3 ]
    guard $ 13 == dx^2 + dy^2
    return (dx,dy)

neighbours neigh bnd (x,y) = do
  (dx,dy) <- case neigh  of
    King -> king
    Knight -> knight
    Zebra -> zebra
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
      let c = length $ filter id $ ps >>= DA.elems
      let info = [ "neighbours:", show neigh, "degrees:", show ns
                 , "width:", show w, "occupied:", show c ]
      when False $ print (ps :: [ DA.Array (Int,Int) Bool ] )
      render_ascii info ps
      let fname = concat ( intersperse "-" ( show neigh : map show ns ++ [ show c ] ) )
                  ++ ".html"
      render_html fname info ps
      return $ Just c
    Nothing -> return Nothing

render_ascii info ps = do
  let ((1,1),(h,w)) = DA.bounds $ head ps
  let m = M.fromList $ do
        (c,p) <- zip [ 'A' .. ] ps ; (i,True) <- DA.assocs p; return (i, c)
  putStrLn $ unwords info
  putStrLn $ unlines $ do
    y <- [1..h]
    return $ do
      x <- [1..w]
      [ M.findWithDefault '.' (x,y) m , ' ' ]

render_html fname info ps = do
  let ((1,1),(h,w)) = DA.bounds $ head ps
      xrange = [1..w] ; yrange = [1..h]
      m = M.fromList $ do
        (c,p) <- zip [ ("red","A"), ("blue","B"), ("lime","C"), ("violet","D") ] ps
        (i,True) <- DA.assocs p
        return (i, c)
      col x y = M.lookup (x,y) m
  putStrLn $ unwords ["output to file", fname ]    
  writeFile fname $ renderHtml [shamlet|
<h2>
  $forall i <- info
        #{i} &nbsp;
<table cellspacing="0" cellpadding="0" border="0">
  $forall x <- xrange
    <col style="width:18"/>
  $forall y <- yrange
    <tr align=center>
      $forall x <- xrange
        <td>
           $maybe c <- col x y
              <font color=#{fst c}>#{snd c}
|]
          
    
for = flip map
