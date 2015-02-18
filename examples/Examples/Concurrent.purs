module Examples.Concurrent where

  import Concurrent (IVar(..), IVarContents(..), fork, get, new, put, spawnEff, spawnPure, runConcurrent)
  import Concurrent.Scheduler (nonPreemptive)

  import Control.Monad (replicateM)
  import Control.Monad.Eff.Ref (writeRef)

  import Data.Maybe

  import Debug.Trace

  import Math

  -- We can specify explicitly the way things should compute.
  pythag :: Number -> Number -> Maybe Number
  pythag a b = runConcurrent nonPreemptive do
    a2i   <- spawnPure $ a * a
    b2i   <- spawnPure $ b * b
    a2    <- get a2i
    b2    <- get b2i
    a2b2i <- spawnPure $ a2 + b2
    a2b2  <- get a2b2i
    pure $ sqrt a2b2

  -- Or just let things resolve themselves.
  -- This example forms a diamond pattern.
  --   a
  --  / \
  -- b   c
  --  \ /
  --   d
  -- But we don't have to care about how we describe the computation.
  diamond :: Maybe Number
  diamond = runConcurrent nonPreemptive do
    [a, b, c, d] <- replicateM 4 new
    fork $ get a >>= \x -> put b (x + 1)
    fork $ get a >>= \x -> put c (x + 2)
    fork $ get b >>= \x -> get c >>= \y -> put d (x + y)
    fork $ put a 3
    get d

  greet :: String -> Maybe String
  greet name = runConcurrent nonPreemptive do
    greeting <- new
    put greeting $ "Hello " ++ name
    get greeting

  main = do
    print $ pythag 3 4
    print diamond
    print $ greet "World"
