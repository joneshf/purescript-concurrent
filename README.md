# Module Documentation

## Module Concurrent

### Types

#### `Concurrency`

    type Concurrency r m a = ContT r (Step m) a

#### `Step`

    data Step m a
      = Step (m a)
      | Atom (m (Step m a))
      | Fork (Step m a) (Step m a)
      | Stop 


### Type Class Instances

#### `applicativeStep`

    instance applicativeStep :: (Applicative m) => Applicative (Step m)

#### `applyStep`

    instance applyStep :: (Apply m) => Apply (Step m)

#### `bindStep`

    instance bindStep :: (Apply m) => Bind (Step m)

#### `functorStep`

    instance functorStep :: (Functor m) => Functor (Step m)

#### `monadStep`

    instance monadStep :: (Monad m) => Monad (Step m)

#### `monadTransStep`

    instance monadTransStep :: MonadTrans Step


### Values

#### `atom`

    atom :: forall r a m. (Functor m) => m a -> Concurrency r m a

#### `fork`

    fork :: forall r a m. Concurrency r m a -> Concurrency r m Unit

#### `forkEff`

    forkEff :: Eff _ Unit -> Concurrency Unit (Eff _) Unit

#### `roundRobin`

    roundRobin :: forall m r. (Monad m) => [Step m r] -> m Unit

#### `run`

    run :: forall r a m. (Monad m) => Concurrency r m a -> m Unit

#### `setTimeout_`

    setTimeout_ :: forall a eff. Fn2 (Eff eff a) Number (Eff eff a)

#### `step`

    step :: forall r a m. Concurrency r m a -> Step m r

#### `stop`

    stop :: forall r a m. Concurrency r m a

#### `undefined`

    undefined :: forall a. a


## Module Concurrent.MVar

### Types

#### `MVar`

    data MVar :: * -> *

#### `MVarEff`

    data MVarEff :: !


### Values

#### `modifyMVar`

    modifyMVar :: forall a eff. (a -> a) -> MVar a -> Eff (mvar :: MVarEff | eff) (MVar a)

#### `newEmptyMVar`

    newEmptyMVar :: forall a eff. Eff (mvar :: MVarEff | eff) (MVar a)

#### `newEmptyMVarImpl`

     data MAction m a = Put a m
                      | Take m a

    newEmptyMVarImpl :: forall a eff. Eff (mvar :: MVarEff | eff) (MVar a)

#### `newMVar`

    newMVar :: forall a eff. a -> Eff (mvar :: MVarEff | eff) (MVar a)

#### `putMVar`

    putMVar :: forall a eff. a -> MVar a -> Eff (mvar :: MVarEff | eff) Unit

#### `putMVarImpl`

    putMVarImpl :: forall a eff. Fn2 a (MVar a) (Eff (mvar :: MVarEff | eff) Unit)

#### `readMVar`

    readMVar :: forall a eff. MVar a -> Eff (mvar :: MVarEff | eff) a

#### `swapMVar`

    swapMVar :: forall a eff. MVar a -> a -> Eff (mvar :: MVarEff | eff) a

#### `takeMVar`

    takeMVar :: forall a eff. MVar a -> Eff (mvar :: MVarEff | eff) a

#### `takeMVarImpl`

    takeMVarImpl :: forall a eff. MVar a -> Eff (mvar :: MVarEff | eff) a

#### `undefined`

    undefined :: forall a. a



