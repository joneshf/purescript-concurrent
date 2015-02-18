module Concurrent.Scheduler (nonPreemptive) where

  import Concurrent

  import Control.Monad.Eff (Eff())
  import Control.Monad.Eff.Ref (Ref(), modifyRef', newRef, readRef, writeRef)

  import Data.Exists (Exists(), runExists)

  foreign import undefined :: forall a. a

  runExists' :: forall r f. Exists f -> (forall a. f a -> r) -> r
  runExists' ex f = runExists f ex

  foreign import runExistsEff """
    function runExistsEff(fa) {
      return function(f) {
        return f(fa);
      };
    }
  """ :: forall f eff r. ExistsEff f -> (forall a. f eff a -> r) -> r

  nonPreemptive :: Scheduler
  nonPreemptive ss s@(Get ex)          = runExists' ex \(GetExists (IVar ref) f) -> do
    i <- readRef ref
    case i of
      Blocked fs -> do
        writeRef ref $ Blocked (f:fs)
        reschedule ss
      Empty  -> reschedule (ss ++ [s])
      Full a -> nonPreemptive ss (f a)
  nonPreemptive ss (New ex)            = runExists' ex \(NewExists i f) -> do
    ref <- newRef i
    nonPreemptive ss (f (IVar ref))
  nonPreemptive ss (Put ex)            = runExists' ex \(PutExists (IVar ref) a s) -> do
    fs <- modifyRef' ref \ic -> case ic of
      Blocked fs -> {newState: Full a, retVal: fs}
      Empty      -> {newState: Full a, retVal: []}
      -- TODO: This is all kinds of bad.
      -- Look into changing the type to reflect this possible failure.
      Full    a  -> undefined
    let ss' = ($ a) <$> fs
    nonPreemptive (ss' ++ ss) s
  nonPreemptive ss (SpawnEff ex)       = runExistsEff ex \(SpawnEffExists eff f) -> do
    a <- eff
    nonPreemptive ss (f a)
  nonPreemptive ss (Fork child parent) = nonPreemptive (child:ss) parent
  nonPreemptive ss Stop                = reschedule ss

  reschedule :: forall eff. [Step] -> Eff (ref :: Ref | eff) Unit
  reschedule []     = pure unit
  reschedule (s:ss) = nonPreemptive ss s
