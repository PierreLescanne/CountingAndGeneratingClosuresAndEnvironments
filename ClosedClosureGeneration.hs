module ClosedClosureGeneration where

import NaturalSize
import TermUnranking
import ClosedClosure

import Control.Monad.State
import System.Random

type Environment = [Closure]
                   
data Closure = Clos Term Environment

instance Show Closure where
  show (Clos t e) = "<" ++ show t ++ "," ++ show e ++ ">"

----- WORK IN PROGRESS
-------------------------------
-- Efficient version
------------------------------

-- take a list of term and a list of lists of closures
-- (as a list of environments) and make a list of closure
distClos :: [Term] -> [[Closure]] -> [Closure]
distClos [] _ = []
distClos (t:lT) lClosure = (map (Clos t) lClosure) ++ distClos lT lClosure

-- list of closed closures of size n
clg :: Int -> [Closure]
clg 0 = []
clg n = foldr (++) [] [foldr (++) []
                             (zipWith distClos
                                      (reverse [list_terms k p | k<-[1..n]])
                                       [allUplg !! p !! k | k<-[0..(n-1)]])
                        | p<-[0..n]]

distcons :: [a] -> [[a]] -> [[a]]
distcons [] _ = []
distcons (c:lc) l = map ((:) c) l ++ distcons lc l

-- lists of p-uples of closed closures of size n 
uplg :: Int -> Int -> [[Closure]]
uplg 0 0 = [[]]
uplg 0 n = []
uplg p n = foldr (++) []
            [distcons (allClg !! i) (allUplg !! (p-1) !! (n-i))
              | i<-[1..n]]

allClg :: [[Closure]]
allClg = [clg n | n<-[0..]]

allUplg :: [[[[Closure]]]]
allUplg = [[uplg p n | n<-[0..]] | p<-[0..]]

-- =====================================================
-- Generating random closed closure
-- =====================================================

type Gen = State StdGen

rand :: Gen Double
rand = do generator <- get
          let (value, newGenerator) = randomR (0,1) generator
          put newGenerator
          return value

randomClosedClosure :: Int -> Gen Closure
randomClosedClosure i =
  do randomDouble <- rand
     let randomIndex = round ((fromInteger (allCl !! i)) * randomDouble)
     return ((clg i) !! randomIndex)

aClosedClosure :: Int -> Int -> Closure
aClosedClosure sz seed =  evalState (randomClosedClosure sz) (mkStdGen seed)

--- Local Variables:
--- mode: haskell
--- mode: haskell-indentation
--- End:
