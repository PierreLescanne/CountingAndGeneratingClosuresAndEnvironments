module ClosedClosure where
import NaturalSize

cl' :: Int -> Integer
cl' 0 = 0
cl' n = sum [sum (zipWith (*) (reverse [natSizeNb p k | k<-[1..n]])
                             [upl' p k | k<-[0..(n-1)]])
           | p<-[0..n]]

upl' :: Int -> Int -> Integer
upl' 0 0 = 1
upl' 0 n = 0
upl' p n = sum (zipWith (*) [cl' i | i<-[0..n]]
                           (reverse [upl' (p-1) i | i<-[0..n]]))

allCl' = [cl' n |n<-[0..]]

-------------------------------
-- Efficient version
------------------------------
cl :: Int -> Integer
cl 0 = 0
cl n = sum [sum (zipWith (*) (reverse [natSizeNb p k | k<-[1..n]])
                             [allUpl !! p !! k | k<-[0..(n-1)]])
           | p<-[0..n]]

upl :: Int -> Int -> Integer
upl 0 0 = 1
upl 0 n = 0
upl p n = sum (zipWith (*) [allCl !! i | i<-[0..n]]
                           (reverse [allUpl !! (p-1) !! i | i<-[0..n]]))


allCl = [cl n | n<-[0..]]
allUpl = [[upl p n | n<-[0..]] | p<-[0..]]

ratios :: [Double]
ratios = [(fromIntegral$ allCl !! (n+1)) /(fromIntegral$ allCl !! n)| n<-[0..]]

-- [0,0,1,2,6]
-- That is [{},{},{λ0<>},{λλ0<>,0<λ0<>>}
--          {λλλ0<>,λλS0<>,λ(00)<>,λ0<λ0<>>,0<λλ0<>>,0<0,λ0>>}


--- Local Variables:
--- mode: haskell
--- mode: haskell-indentation
--- End:
