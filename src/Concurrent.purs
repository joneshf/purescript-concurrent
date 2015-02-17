module Concurrent where

  import Control.Alt
  import Control.Alternative
  import Control.Apply
  import Control.Monad.Cont.Trans
  import Control.Monad.Eff
  import Control.Monad.Eff.Ref
  import Control.Monad.ST
  import Control.Plus
  import Control.MonadPlus

  import Data.Array
  import Data.Function (on)

  import Debug.Trace

  data Step a = Step a
              | Fork (Step a) (Step a)
              | Stop

  type Concurrent a = forall b. ContT b Step a

  stop :: forall a. Concurrent a
  stop = ContT \_ -> Stop

  fork :: forall a. a -> a -> Concurrent a
  fork x y = ContT \k -> Fork (k x) (k y)

  step :: forall a. a -> Concurrent a
  step = pure

  instance functorStep :: Functor Step where
    (<$>) _ Stop        = Stop
    (<$>) f (Fork c c') = Fork (f <$> c) (f <$> c')
    (<$>) f (Step a)    = Step (f a)

  instance applyStep :: Apply Step where
    (<*>) Stop       _          = Stop
    (<*>) _          Stop       = Stop
    (<*>) (Fork f g) c          = Fork (f <*> c) (g <*> c)
    (<*>) f          (Fork x y) = Fork (f <*> x) (f <*> y)
    (<*>) (Step f)   c          = f <$> c
    (<*>) f          (Step x)   = f <#> ($ x)

  instance bindStep :: Bind Step where
    (>>=) (Step x)    f = f x
    (>>=) (Fork s s') f = Fork (s >>= f) (s' >>= f)
    (>>=) Stop        _ = Stop

  instance applicativeStep :: Applicative Step where
    pure = Step

  instance monadStep :: Monad Step

  roundRobin :: forall m a. (Monad m) => [Step a] -> m Unit
  roundRobin []     = pure unit
  roundRobin (s:ss) = case s of
    Step a      -> pure a *> roundRobin ss
    Fork s' s'' -> roundRobin (ss ++ [s', s''])
    Stop        -> roundRobin ss

  reschedule :: forall b a m. (MonadPlus m) => [Step a] -> m Unit
  reschedule []     = pure unit
  reschedule (s:ss) = schedule ss s

  schedule :: forall b a m. (MonadPlus m) => [Step a] -> Step a -> m Unit
  schedule ss (Step a)            = pure a *> reschedule ss
  schedule ss (Fork child parent) = schedule (child:ss) parent
  schedule ss Stop                = reschedule ss

  data IVarContents a = Blocked [a -> Step a]
                      | Full a

  foreign import undefined :: forall a. a

  foreign import pureRef """
    function pureRef(eff) {
      console.log(eff);
      return eff;
    }
  """ :: forall r s. Eff (ref :: Ref | r) s -> s

  instance altEff :: Alt (Eff e) where
    (<|>) e e' = e *> e'

  instance plusEff :: Plus (Eff e) where
    empty = undefined

  instance alternativeEff :: Alternative (Eff e)

  instance monadPlusEff :: MonadPlus (Eff e)

  runConcurrent :: forall b a m. Concurrent a -> Eff _ a
  runConcurrent c =  (do
    ref <- newSTRef (Blocked [])
    schedule [] (runContT (c >>= put ref) (\_ -> Stop))
    a <- readSTRef ref
    case a of
      Full a' -> pure a'
      Blocked _ -> undefined)
    where
      -- put :: forall a r. RefVal a -> a -> Concurrent Unit
      put ref val = ContT \k ->
        Step ((writeSTRef ref $ Full val))

  pythag :: Number -> Number -> Number
  pythag x y = pureST (runConcurrent do
    xSq <- pure $ x * x
    ySq <- pure $ y * y
    sum <- pure $ xSq + ySq
    pure $ Math.sqrt sum)

  main = do
    print $ pythag 3 4
