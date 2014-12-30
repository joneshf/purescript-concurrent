module Concurrent.MVar where

  import Control.Monad.Eff (Eff())

  import Debug.Trace (Trace(), print)

  foreign import data M :: !
  foreign import data MVar :: * -> *

  foreign import newMVar
    """
      function newMVar(x) {
        return function() {
          return new mVarImpl(x, true);
        }
      }
    """ :: forall a eff. a -> Eff (mVar :: M | eff) (MVar a)

  foreign import newEmptyMVar
    """
      function newEmptyMVar() {
        return new mVarImpl(null, false);
      }
    """ :: forall a eff. Eff (mVar :: M | eff) (MVar a)

  foreign import putMVar
    """
      function putMVar(mVar) {
        return function(x) {
          return function() {
            var ready = false;
            mVar.enqueuePut(function() {
              ready = true;
            }, x);
            while (!ready) {}
            return {};
          }
        }
      }
    """ :: forall a eff. MVar a -> a -> Eff (mVar :: M | eff) Unit

  foreign import readMVar
    """
      function readMVar(mVar) {
        return function() {
          var ready = false;
          mVar.enqueueRead(function() {
            ready = true;
          })
          while (!ready) {}
          return mVar.value;
        }
      }
    """ :: forall a eff. MVar a -> Eff (mVar :: M | eff) a

  foreign import takeMVar
    """
      function takeMVar(mVar) {
        return function() {
          var ready = false;
          mVar.enqueueTake(function() {
            ready = true;
          })
          while (!ready) {}
          return mVar.value;
        }
      }
    """ :: forall a eff. MVar a -> Eff (mVar :: M | eff) a

  main :: Eff (mVar :: M, trace :: Trace) Unit
  main = do
    foo <- newEmptyMVar
    putMVar foo 3
    x <- readMVar foo
    print x
    y <- takeMVar foo
    print y
