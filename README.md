# Module Documentation

## Module Concurrent

### Types

#### `Concurrent`

    type Concurrent a = forall b. ContT b Step a

#### `IVarContents`

    data IVarContents a
      = Blocked [a -> Step a]
      | Full a

#### `Step`

    data Step a
      = Step a
      | Fork (Step a) (Step a)
      | Stop 


### Type Class Instances

#### `altEff`

    instance altEff :: Alt (Eff e)

#### `alternativeEff`

    instance alternativeEff :: Alternative (Eff e)

#### `applicativeStep`

    instance applicativeStep :: Applicative Step

#### `applyStep`

    instance applyStep :: Apply Step

#### `bindStep`

    instance bindStep :: Bind Step

#### `functorStep`

    instance functorStep :: Functor Step

#### `monadPlusEff`

    instance monadPlusEff :: MonadPlus (Eff e)

#### `monadStep`

    instance monadStep :: Monad Step

#### `plusEff`

    instance plusEff :: Plus (Eff e)


### Values

#### `fork`

    fork :: forall a. a -> a -> Concurrent a

#### `pureRef`

    pureRef :: forall r s. Eff (ref :: Ref | r) s -> s

#### `pythag`

    pythag :: Number -> Number -> Number

#### `reschedule`

    reschedule :: forall b a m. (MonadPlus m) => [Step a] -> m Unit

#### `roundRobin`

    roundRobin :: forall m a. (Monad m) => [Step a] -> m Unit

#### `runConcurrent`

    runConcurrent :: forall b a m. Concurrent a -> Eff _ a

#### `schedule`

    schedule :: forall b a m. (MonadPlus m) => [Step a] -> Step a -> m Unit

#### `step`

    step :: forall a. a -> Concurrent a

#### `stop`

    stop :: forall a. Concurrent a

#### `undefined`

    undefined :: forall a. a


## Module Concurrent.Temp

### Types

#### `Concurrent`

    type Concurrent a = ContT Step Identity a

#### `GetExists`

    data GetExists a
      = GetExists (IVar a) (a -> Step)

#### `IVar`

    newtype IVar a
      = IVar (RefVal (IVarContents a))

#### `IVarContents`

    data IVarContents a
      = Blocked [a -> Step]
      | Empty 
      | Full a

#### `NewExists`

    data NewExists a
      = NewExists (IVar a -> Step)

#### `PutExists`

    data PutExists a
      = PutExists (IVar a) a Step

#### `Step`

    data Step
      = Get (Exists GetExists)
      | Put (Exists PutExists)
      | New (Exists NewExists)
      | Fork Step Step
      | Stop 


### Values

#### `fork`

    fork :: Concurrent Unit -> Concurrent Unit

#### `get`

    get :: forall a. IVar a -> Concurrent a

#### `new`

    new :: forall a. Concurrent (IVar a)

#### `put`

    put :: forall a. IVar a -> a -> Concurrent Unit

#### `undefined`

    undefined :: forall a. a


## Module Concurrent.Par

### Type Classes

#### `ParFuture`

    class (Monad m) <= ParFuture future m where
      spawn :: forall a. m a -> m (future a)
      spawnP :: forall a. a -> m (future a)
      get :: forall a. future a -> m a



