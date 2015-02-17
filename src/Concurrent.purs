module Concurrent where

  import Control.Apply
  import Control.Monad
  import Control.Monad.Cont.Trans
  import Control.Monad.Eff
  import Control.Monad.Eff.Ref
  import Control.Monad.Eff.Unsafe

  import Data.Array
  import Data.Exists
  import Data.Identity
  import Data.Function (on)
  import Data.Traversable

  import Debug.Trace

  foreign import undefined :: forall a. a

  type Concurrent a = ContT Step Identity a

  data IVarContents a = Blocked [a -> Step]
                      | Empty
                      | Full a

  newtype IVar a = IVar (RefVal (IVarContents a))

  data Pair a b = Pair a b

  data Step = Get (Exists GetExists)
            | Put (Exists PutExists)
            | New (Exists NewExists)
            | Fork Step Step
            | Stop

  data GetExists a = GetExists (IVar a) (a -> Step)
  data PutExists a = PutExists (IVar a) a Step
  data NewExists a = NewExists (IVarContents a) (IVar a -> Step)

  type Scheduler = forall eff. [Step] -> Step -> Eff (ref :: Ref | eff) Unit

  runExists' :: forall r f. Exists f -> (forall a. f a -> r) -> r
  runExists' ex f = runExists f ex

  fork :: Concurrent Unit -> Concurrent Unit
  fork c = ContT \k ->
    Identity $ Fork (runIdentity $ runContT c (\_ -> Identity Stop))
                    (runIdentity $ k unit)

  get :: forall a. IVar a -> Concurrent a
  get i = ContT \k ->
    Identity $ Get $ mkExists $ GetExists i (runIdentity <<< k)

  put :: forall a. IVar a -> a -> Concurrent Unit
  put i a = ContT \k ->
    Identity $ Put $ mkExists $ PutExists i a (runIdentity $ k unit)

  new :: forall a. Concurrent (IVar a)
  new = ContT \k ->
    Identity $ New $ mkExists $ NewExists Empty (runIdentity <<< k)

  stop :: forall a. Concurrent a
  stop = ContT \k -> Identity Stop

  spawnP :: forall a. a -> Concurrent (IVar a)
  spawnP a = do
    i <- new
    put i a
    pure i

  reschedule :: forall eff. [Step] -> Eff (ref :: Ref | eff) Unit
  reschedule []     = pure unit
  reschedule (s:ss) = nonPreemptive ss s

  nonPreemptive :: Scheduler
  nonPreemptive ss (New ex)            = runExists' ex \(NewExists i f) -> do
    ref <- newRef i
    nonPreemptive ss (f (IVar ref))
  nonPreemptive ss s@(Get ex)          = runExists' ex \(GetExists (IVar ref) f) -> do
    i <- readRef ref
    case i of
      Blocked fs -> do
        writeRef ref $ Blocked (f:fs)
        reschedule ss
      Empty  -> reschedule (ss ++ [s])
      Full a -> nonPreemptive ss (f a)
  nonPreemptive ss (Put ex)            = runExists' ex \(PutExists (IVar ref) a s) -> do
    fs <- modifyRef' ref \ic -> case ic of
      Blocked fs -> {newState: Full a, retVal: fs}
      Empty      -> {newState: Full a, retVal: []}
      Full    a  -> undefined
    let ss' = ($ a) <$> fs
    nonPreemptive (ss' ++ ss) s
  nonPreemptive ss (Fork child parent) = nonPreemptive (child:ss) parent
  nonPreemptive ss Stop                = reschedule ss

  runConcurrent :: forall a. Scheduler -> Concurrent a -> a
  runConcurrent scheduler c = runPure (unsafeInterleaveEff do
    ref <- newRef $ Blocked []
    scheduler [] $ runIdentity $ runContT (c >>= put (IVar ref))
                                          (const $ Identity Stop)
    r <- readRef ref
    case r of
      Full a -> return a
      _      -> undefined)

  -- We can specify explicitly the way things should compute.
  pythag :: Number -> Number -> Number
  pythag a b = runConcurrent nonPreemptive do
    a2i   <- spawnP $ a * a
    b2i   <- spawnP $ b * b
    a2    <- get a2i
    b2    <- get b2i
    a2b2i <- spawnP $ a2 + b2
    a2b2  <- get a2b2i
    pure $ Math.sqrt a2b2

  -- Or just let things resolve themselves.
  diamond :: Number
  diamond = runConcurrent nonPreemptive do
    [a, b, c, d] <- replicateM 4 new
    fork $ get a >>= \x -> put b (x + 1)
    fork $ get a >>= \x -> put c (x + 2)
    fork $ get b >>= \x -> get c >>= \y -> put d (x + y)
    fork $ put a 3
    get d

  main = do
    print $ pythag 3 4
    print diamond
