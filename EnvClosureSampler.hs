-- | Compiler: Boltzmann brain v1.3.1
-- | Singularity: 0.165476494014686
-- | System type: algebraic
-- | System size: 4
-- | Constructors: 8
module EnvClosureSampler
       (Closure(..), Env(..), Nat(..), Term(..), genRandomClosure,
        genRandomEnv, genRandomNat, genRandomTerm, sampleClosure,
        sampleEnv, sampleNat, sampleTerm, sampleClosureIO, sampleEnvIO,
        sampleNatIO, sampleTermIO)
       where
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Random
       (RandomGen(..), Rand, getRandomR, evalRandIO)

data Closure = Couple Term Env
             deriving Show

data Env = Cons Closure Env
         | Empty
         deriving Show

data Nat = Z
         | S Nat
         deriving Show

data Term = Index Nat
          | Abs Term
          | App Term Term
          deriving Show

randomP :: RandomGen g => MaybeT (Rand g) Double
randomP = lift (getRandomR (0, 1))

genRandomClosure ::
                   RandomGen g => Int -> MaybeT (Rand g) (Closure, Int)
genRandomClosure ub
  = do guard (ub > 0)
       (x0, w0) <- genRandomTerm ub
       (x1, w1) <- genRandomEnv (ub - 0 - w0)
       return (Couple x0 x1, w1 + w0)

genRandomEnv :: RandomGen g => Int -> MaybeT (Rand g) (Env, Int)
genRandomEnv ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.500000000001416 then
         do (x0, w0) <- genRandomClosure ub
            (x1, w1) <- genRandomEnv (ub - 0 - w0)
            return (Cons x0 x1, w1 + w0)
         else return (Empty, 0)

genRandomNat :: RandomGen g => Int -> MaybeT (Rand g) (Nat, Int)
genRandomNat ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.8345235059842309 then return (Z, 1) else
         do (x0, w0) <- genRandomNat (ub - 1)
            return (S x0, 1 + w0)

genRandomTerm :: RandomGen g => Int -> MaybeT (Rand g) (Term, Int)
genRandomTerm ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.793154382480613 then
         do (x0, w0) <- genRandomNat ub
            return (Index x0, w0)
         else
         if p < 0.958630876495299 then
           do (x0, w0) <- genRandomTerm (ub - 1)
              return (Abs x0, 1 + w0)
           else
           do (x0, w0) <- genRandomTerm (ub - 1)
              (x1, w1) <- genRandomTerm (ub - 1 - w0)
              return (App x0 x1, 1 + w1 + w0)

sampleClosure :: RandomGen g => Int -> Int -> Rand g Closure
sampleClosure lb ub
  = do sample <- runMaybeT (genRandomClosure ub)
       case sample of
           Nothing -> sampleClosure lb ub
           Just (x, s) -> if lb <= s && s <= ub then return x else
                            sampleClosure lb ub

sampleEnv :: RandomGen g => Int -> Int -> Rand g Env
sampleEnv lb ub
  = do sample <- runMaybeT (genRandomEnv ub)
       case sample of
           Nothing -> sampleEnv lb ub
           Just (x, s) -> if lb <= s && s <= ub then return x else
                            sampleEnv lb ub

sampleNat :: RandomGen g => Int -> Int -> Rand g Nat
sampleNat lb ub
  = do sample <- runMaybeT (genRandomNat ub)
       case sample of
           Nothing -> sampleNat lb ub
           Just (x, s) -> if lb <= s && s <= ub then return x else
                            sampleNat lb ub

sampleTerm :: RandomGen g => Int -> Int -> Rand g Term
sampleTerm lb ub
  = do sample <- runMaybeT (genRandomTerm ub)
       case sample of
           Nothing -> sampleTerm lb ub
           Just (x, s) -> if lb <= s && s <= ub then return x else
                            sampleTerm lb ub

sampleClosureIO :: Int -> Int -> IO Closure
sampleClosureIO lb ub = evalRandIO (sampleClosure lb ub)

sampleEnvIO :: Int -> Int -> IO Env
sampleEnvIO lb ub = evalRandIO (sampleEnv lb ub)

sampleNatIO :: Int -> Int -> IO Nat
sampleNatIO lb ub = evalRandIO (sampleNat lb ub)

sampleTermIO :: Int -> Int -> IO Term
sampleTermIO lb ub = evalRandIO (sampleTerm lb ub)
