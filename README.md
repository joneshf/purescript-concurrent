# Module Documentation

## Module Concurrent

## Module Concurrent.MVar

### Types

    data M :: !

    data MVar :: * -> *


### Values

    main :: Eff (trace :: Trace, mVar :: M) Unit

    newEmptyMVar :: forall a eff. Eff (mVar :: M | eff) (MVar a)

    newMVar :: forall a eff. a -> Eff (mVar :: M | eff) (MVar a)

    putMVar :: forall a eff. MVar a -> a -> Eff (mVar :: M | eff) Unit

    readMVar :: forall a eff. MVar a -> Eff (mVar :: M | eff) a

    takeMVar :: forall a eff. MVar a -> Eff (mVar :: M | eff) a



