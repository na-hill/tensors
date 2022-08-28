{-# LANGUAGE BlockArguments #-}

module Tests where

import Data.Bool (bool)
import Data.Functor.Identity (runIdentity)
import Data.Map (fromList)
import Data.List (transpose)
import Data.Monoid (Sum(..))
import qualified Data.Set as S

import Test.HUnit

import Einstein
import Combinator
import If

tests = test [
  "Merges" ~: [

    assertEqual "Applicative []"
      [ (1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
      $ merge [1,2,3] [4,5,6]

   ,assertEqual "Applicative ((->) a), Applicative []"
      [ (0,0),(0,0),(0,0),(0,0),(0,0),(0,0)]
      $ merge2 (replicate 2) (replicate 3) 0

   ,assertEqual "Applicative ((->) a)"
      ( 2,0)
      $ merge2 (+) (*) 0 2
   ]
 ,"Tensor Products" ~: [
    "Plain" ~: [

      assertEqual "Mixed types"
        [ fromList [
            (0,[[("x0","y0","z0",1),("x0","y0","z0",1)],[("x0","y0","z1",1),("x0","y0","z1",1)]])
           ,(1,[[("x0","y1","z0",1),("x0","y1","z0",1)],[("x0","y1","z1",1),("x0","y1","z1",1)]])]
         ,fromList [
            (0,[[("x1","y0","z0",1),("x1","y0","z0",1)],[("x1","y0","z1",1),("x1","y0","z1",1)]])
           ,(1,[[("x1","y1","z0",1),("x1","y1","z0",1)],[("x1","y1","z1",1),("x1","y1","z1",1)]])]]
        $ cur4 id `b0` fmap /. ["x0", "x1"] `x1` fmap /. fromList [(0,"y0"), (1,"y1")] `x1` fmap /. ["z0", "z1"] `x1` fmap /. [1, 1]

     ,assertEqual "Matrix multiplication (manual)"
        [ [22,28],[49,64]]
        $ zipWith (*) `b0` [[1,2,3],[4,5,6]] `xt` transpose [[1,2],[3,4],[5,6]] `xt` sum

     ,assertEqual "Cross product"
        [-1,0,2]
        $ let
            b=[1..3]
            u=[2,0,1]
            v=[0,1,0]
          in b `xt` zip b u `ft` zip b v `ft` Sum
          $ \k      (i,ui)       (j,vj)    -> levciv [i,j,k] * ui * vj
    ]
   ,"Sequenced" ~: [

      assertEqual "Applicative Identity"
        [ [[(1,4,6),(1,4,7),(1,4,8),(1,4,9)],[(1,5,6),(1,5,7),(1,5,8),(1,5,9)]]
         ,[[(2,4,6),(2,4,7),(2,4,8),(2,4,9)],[(2,5,6),(2,5,7),(2,5,8),(2,5,9)]]
         ,[[(3,4,6),(3,4,7),(3,4,8),(3,4,9)],[(3,5,6),(3,5,7),(3,5,8),(3,5,9)]]]
        $ (runIdentity $ cur3 id `b0` [1,2,3] `st` [4,5] `st` [6,7,8,9] `st` pure)

     ,assertEqual "v0"
        [ [[(),(),(),()],[(),(),(),()]],[[(),(),(),()],[(),(),(),()]],[[(),(),(),()],[(),(),(),()]]]
        $ runIdentity $ cur3 id `b0` [1,2,3] `st` [4,5] `st` [6,7,8,9] `st` v0.pure

     ,assertEqual "vt"
        [ [(),()],[(),()],[(),()]]
        $ runIdentity $ cur3 id `b0` [1,2,3] `st` [4,5] `st` [6,7,8,9] `vt` pure

     ,assertEqual "outside vt"
        ( replicate (2^(2*2*2)) ())
        $ cur3 id `b0` [1,2] `vt` [3,4] `st` [5,6] `st` replicate 2

     ,assertEqual "v0 is id under vt"
        ( replicate (2^(2*2*2)) ())
        $ cur3 id `b0` [1,2] `vt` [3,4] `vt` [5,6] `vt` v0.replicate 2
     ]
   ,"Folded" ~: [

      assertEqual "Monoid ([] a)"
        ( cur4 id `f1` [1,2] `a1` [3,4] `a1` [5,6] `a1` [7,8])
        $ cur4 id `b0` [1,2] `ft` [3,4] `ft` [5,6] `ft` [7,8] `xt` id
     ]
   ,"Joined" ~: [

      assertEqual "Monad ((->) a)"
        ( (/) `f1` (*2) `a1` (+1)         $ 3)
        $ (/) `b0` (*2) `jt` (+1) `xt` id $ 3

     ,assertEqual "Monad []"
        ( cur4 id `f1` [1,2] `a1` [3,4] `a1` [5,6] `a1` [7,8])
        $ cur4 id `b0` [1,2] `jt` [3,4] `jt` [5,6] `jt` [7,8] `xt` id
     ]
   ]
 ]

main = runTestTT tests
