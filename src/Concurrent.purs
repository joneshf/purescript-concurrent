{- |
    Concurrent implementation based heavily on the `monad-par` paper by
    Marlow, Newton and Peyton-Jones
-}

module Concurrent where

  import Control.Monad.Cont.Trans
  import Control.Monad.Eff
  import Control.Monad.Eff.Ref
  import Control.Monad.Eff.Unsafe

  import Data.Exists
  import Data.Identity
  import Data.Maybe

  data IVarContents a = Blocked [a -> Step]
                      | Empty
                      | Full a

  newtype IVar a = IVar (RefVal (IVarContents a))

  data GetExists a = GetExists (IVar a) (a -> Step)
  data NewExists a = NewExists (IVarContents a) (IVar a -> Step)
  data PutExists a = PutExists (IVar a) a Step
  data SpawnEffExists eff a = SpawnEffExists (Eff eff a) (a -> Step)

  data Step = Get (Exists GetExists)
            | New (Exists NewExists)
            | Put (Exists PutExists)
            | SpawnEff (ExistsEff SpawnEffExists)
            | Fork Step Step
            | Stop

  foreign import data ExistsEff :: (# ! -> * -> *) -> *

  foreign import mkExistsEff """
    function mkExistsEff(f) {
      return f;
    }
  """ :: forall f eff a. f eff a -> ExistsEff f

  type Concurrent a = ContT Step Identity a
  type Scheduler = forall eff. [Step] -> Step -> Eff (ref :: Ref | eff) Unit

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

  spawnPure :: forall a. a -> Concurrent (IVar a)
  spawnPure a = do
    i <- new
    put i a
    pure i

  spawnEff :: forall a eff. Eff eff a -> Concurrent (a)
  spawnEff eff = ContT \k ->
    Identity $ SpawnEff $ mkExistsEff $ SpawnEffExists eff (const Stop)

  -- TODO: It'd be nice if this didn't return a `Maybe a`,
  -- Think about other representations.
  runConcurrent :: forall a. Scheduler -> Concurrent a -> Maybe a
  runConcurrent scheduler c = runPure (unsafeInterleaveEff do
    ref <- newRef $ Blocked []
    scheduler [] $ runIdentity $ runContT (c >>= put (IVar ref)) \_ -> pure Stop
    r <- readRef ref
    case r of
      Full a -> pure $ Just a
      _      -> pure $ Nothing)
