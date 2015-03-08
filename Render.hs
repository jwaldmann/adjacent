{-# LANGUAGE QuasiQuotes #-}

module Render where

import qualified Data.Array as DA
import qualified Data.Map as M

import Text.Hamlet (shamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)

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
          
