module Concurrent where

  import Control.Apply
  import Control.Monad
  import Control.Monad.Cont.Trans
  import Control.Monad.Eff
  import Control.Monad.Trans

  import Data.Array
  import Data.Function

  import Debug.Trace

  forkEff :: Eff _ Unit -> Concurrency Unit (Eff _) Unit
  forkEff = lift <<< lift

  foreign import setTimeout_ """
    function setTimeout_(f, ms) {
      return function() {
        return setTimeout(function() { return f(); }, ms);
      }
    }
  """ :: forall a eff. Fn2 (Eff eff a) Number (Eff eff a)

  -- repA :: Number -> Eff _ Unit
  repA 0 = pure unit
  repA n = do
    forkEff $ print "A"
    repA (n - 1)

  -- repB :: Number -> Eff _ Unit
  repB 0 = pure unit
  repB n = do
    forkEff $ print "B"
    repB (n - 1)

  main = run do
    repA 10
    forkEff $ runFn2 setTimeout_ (print 8) 2
    forkEff $ runFn2 setTimeout_ (print 2) 8
    forkEff $ runFn2 setTimeout_ (print 3) 7
    forkEff $ runFn2 setTimeout_ (print 4) 6
    forkEff $ runFn2 setTimeout_ (print 5) 5
    forkEff $ runFn2 setTimeout_ (print 6) 4
    forkEff $ runFn2 setTimeout_ (print 7) 3
    forkEff $ runFn2 setTimeout_ (print 9) 1
    forkEff $ runFn2 setTimeout_ (print 1) 9
    pure $ print 31
    repB 10

  type Concurrency r m a = ContT r (Step m) a

  roundRobin :: forall m r. (Monad m) => [Step m r] -> m Unit
  roundRobin []     = pure unit
  roundRobin (c:cs) = case c of
    Step m    -> m *> roundRobin cs
    Atom m    -> do
      a <- m
      roundRobin (cs `snoc` a)
    Fork m m' -> roundRobin (cs ++ [m, m'])
    Stop      -> roundRobin cs

  run :: forall r a m. (Monad m) => Concurrency r m a -> m Unit
  run c = roundRobin [step c]

  step :: forall r a m. Concurrency r m a -> Step m r
  step c = runContT c (const Stop)

  atom :: forall r a m. (Functor m) => m a -> Concurrency r m a
  atom m = ContT \k -> Atom $ k <$> m

  fork :: forall r a m. Concurrency r m a -> Concurrency r m Unit
  fork m = ContT \k -> Fork (step m) (k unit)

  stop :: forall r a m. Concurrency r m a
  stop = ContT $ const Stop

  foreign import undefined :: forall a. a

  data Step m a = Step (m a)
                | Atom (m (Step m a))
                | Fork (Step m a) (Step m a)
                | Stop

  instance functorStep :: (Functor m) => Functor (Step m) where
    (<$>) f (Step a)    = Step (f <$> a)
    (<$>) f (Atom m)    = Atom ((f <$>) <$> m)
    (<$>) f (Fork s s') = Fork (f <$> s) (f <$> s')
    (<$>) f Stop        = Stop

  instance applyStep :: (Apply m) => Apply (Step m) where
    (<*>) Stop       _           = Stop
    (<*>) _          Stop        = Stop
    (<*>) (Fork f g) s           = Fork (f <*> s) (g <*> s)
    (<*>) f          (Fork s s') = Fork (f <*> s) (f <*> s')
    (<*>) (Atom f)   s           = Atom ((<*> s) <$> f)
    (<*>) f          (Atom s)    = Atom ((f <*>) <$> s)
    (<*>) (Step f)   (Step m)    = Step (f <*> m)

  instance bindStep :: (Apply m) => Bind (Step m) where
    (>>=) (Step m)    f = Atom (f <$> m)
    (>>=) (Atom m)    f = Atom $ (>>= f) <$> m
    (>>=) (Fork s s') f = Fork (s >>= f) (s' >>= f)
    (>>=) Stop        _ = Stop

  instance applicativeStep :: (Applicative m) => Applicative (Step m) where
    pure x = Step $ pure x

  instance monadStep :: (Monad m) => Monad (Step m)

  instance monadTransStep :: MonadTrans Step where
    lift = Step

-- module Concurrent where

--   import Control.Monad
--   import Control.Monad.Eff
--   import Control.Monad.Trans
--   import Control.Monad.Writer
--   import Control.Monad.Writer.Trans
--   import Control.Monad.Writer.Class

--   import Data.Array
--   import Data.Monoid
--   import Data.Tuple

--   import Debug.Trace

--   -- data Action m r = Atom (m (Action m r))
--   --                 | Fork (Action m r) (Action m r)
--   --                 | Stop

--   -- type C r m a = ContT r (Action m) a

--   -- action :: forall r a m. (Monad m) => C r m a -> Action m r
--   -- action m = runContT m $ const Stop

--   action :: forall r a m. (Monad m) => C m a -> Action m
--   action m = runC m $ const Stop

--   newtype C m a = C ((a -> Action m) -> Action m)

--   data Action m = Atom (m (Action m))
--                 | Fork (Action m) (Action m)
--                 | Stop

--   runC (C m) = m

--   instance functorC :: Functor (C m) where
--     -- (<$>) :: (a -> b) -> C ((a -> Action m) -> Action m) -> C ((b -> Action m) -> Action m)
--     (<$>) f c = C \h -> runC c \a -> h (f a)

--   instance applyC :: Apply (C m) where
--     -- (<*>) :: C (((a -> b) -> Action m) -> Action m) -> C ((a -> Action m) -> Action m) -> C ((b -> Action m) -> Action m)
--     (<*>) f c = C \h -> runC c \a -> runC f \f' -> h (f' a)

--   instance applicativeC :: Applicative (C m) where
--     pure x = C \f -> f x

--   instance bindC :: Bind (C m) where
--     -- (>>=) :: C ((a -> Action m) -> Action m) -> (a -> C ((b -> Action m) -> Action m)) -> C ((b -> Action m) -> Action m)
--     (>>=) c g = C \h -> runC c \a -> runC (g a) h

--   instance monadC :: Monad (C m)

--   instance monadTransC :: MonadTrans C where
--     -- lift :: m a -> C ((a -> Action m) -> Action m)
--     lift = atom

--   atom :: forall a m. (Monad m) => m a -> C m a
--   atom m = C \f -> Atom do
--     a <- m
--     pure $ f a

--   stop :: forall a m. (Monad m) => C m a
--   stop = C $ const Stop

--   par :: forall a m. (Monad m) => C m a -> C m a -> C m a
--   par (C m) (C m') = C \f -> Fork (m f) (m' f)

--   fork :: forall a m. (Monad m) => C m a -> C m Unit
--   fork m = C \f -> Fork (action m) (f unit)

--   roundRobin :: forall m. (Monad m) => [Action m] -> m Unit
--   roundRobin []     = pure unit
--   roundRobin (a:as) = case a of
--     Atom m -> do
--       a' <- m
--       roundRobin $ as ++ [a']
--     Fork a a' -> roundRobin $ as ++ [a, a']
--     Stop -> roundRobin as

--   run :: forall a m. (Monad m) => C m a -> m Unit
--   run m = roundRobin [action m]

--   instance monadWriterC :: (Monad m, MonadWriter w m) => MonadWriter w (C m) where
--     -- writer :: Tuple a w -> C ((a -> Action m) -> Action m)
--     writer = lift <<< writer
--     listen = undefined
--     pass = undefined

--   foreign import undefined :: forall a. a

--   -- loop :: String -> C (Writer String) Unit
--   -- loop s = do
--   --   lift $ tell s
--   --   lift $ tell s
--   --   lift $ tell s
--   --   lift $ tell s

--   -- example :: C (Writer String) Unit
--   -- example = do
--   --   writer $ Tuple unit "start!"
--   --   fork $ loop "fish"
--   --   loop "cat"

--   -- main = print $ execWriter $ run example

--   fourA :: Number -> C (Eff _) Unit
--   fourA 0 = pure unit
--   fourA n = do
--     lift $ print "A"
--     fourA (n - 1)

--   fourB :: Number -> C (Eff _) Unit
--   fourB 0 = pure unit
--   fourB n = do
--     lift $ print "B"
--     fourB (n - 1)

--   main = do
--     run $ par (fourA 4) (fourB 4)
