-- module Concurrent.MVar where

--   import Control.Monad.Eff (Eff())
--   import Control.Monad.Eff.Ref

--   import Data.Function

--   foreign import data MVarEff :: !

--   foreign import data MVar :: * -> *

--   foreign import undefined :: forall a. a

--   -- data MAction m a = Put a m
--   --                  | Take m a

--   foreign import newEmptyMVarImpl """
--     function newEmptyMVarImpl() {
--       return {filled: false, val: null}
--     }
--   """ :: forall a eff. Eff (mvar :: MVarEff | eff) (MVar a)

--   foreign import putMVarImpl """
--     function putMVarImpl(a, m) {
--       return function() {
--         while (m.filled) {}
--         m.filled = true;
--         m.val = a;

--         return {};
--       }
--     }
--   """ :: forall a eff. Fn2 a (MVar a) (Eff (mvar :: MVarEff | eff) Unit)

--   foreign import takeMVarImpl """
--     function takeMVarImpl(m) {
--       return function() {
--         while (!m.filled) {}
--         m.filled = false;

--         return m.val;
--       }
--     }
--   """ :: forall a eff. MVar a -> Eff (mvar :: MVarEff | eff) a

--   modifyMVar :: forall a eff. (a -> a) -> MVar a -> Eff (mvar :: MVarEff | eff) (MVar a)
--   modifyMVar f m = do
--     a <- takeMVar m
--     putMVar (f a) m
--     pure m

--   newEmptyMVar :: forall a eff. Eff (mvar :: MVarEff | eff) (MVar a)
--   newEmptyMVar = newEmptyMVarImpl

--   newMVar :: forall a eff. a -> Eff (mvar :: MVarEff | eff) (MVar a)
--   newMVar a = do
--     m <- newEmptyMVar
--     putMVar a m
--     pure m

--   putMVar :: forall a eff. a -> MVar a -> Eff (mvar :: MVarEff | eff) Unit
--   putMVar a m = runFn2 putMVarImpl a m

--   readMVar :: forall a eff. MVar a -> Eff (mvar :: MVarEff | eff) a
--   readMVar m = do
--     a <- takeMVar m
--     putMVar a m
--     pure a

--   swapMVar :: forall a eff. MVar a -> a -> Eff (mvar :: MVarEff | eff) a
--   swapMVar m a = do
--     a' <- takeMVar m
--     putMVar a m
--     pure a'

--   takeMVar :: forall a eff. MVar a -> Eff (mvar :: MVarEff | eff) a
--   takeMVar = takeMVarImpl
