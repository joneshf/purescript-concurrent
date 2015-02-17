# Module Documentation

## Module Concurrent

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
      = NewExists (IVarContents a) (IVar a -> Step)

#### `Pair`

    data Pair a b
      = Pair a b

#### `PutExists`

    data PutExists a
      = PutExists (IVar a) a Step

#### `Scheduler`

    type Scheduler = forall eff. [Step] -> Step -> Eff (ref :: Ref | eff) Unit

#### `Step`

    data Step
      = Get (Exists GetExists)
      | Put (Exists PutExists)
      | New (Exists NewExists)
      | Fork Step Step
      | Stop 


### Values

#### `diamond`

     Or just let things resolve themselves.

    diamond :: Number

#### `fork`

    fork :: Concurrent Unit -> Concurrent Unit

#### `get`

    get :: forall a. IVar a -> Concurrent a

#### `new`

    new :: forall a. Concurrent (IVar a)

#### `nonPreemptive`

    nonPreemptive :: Scheduler

#### `put`

    put :: forall a. IVar a -> a -> Concurrent Unit

#### `pythag`

     We can specify explicitly the way things should compute.

    pythag :: Number -> Number -> Number

#### `reschedule`

    reschedule :: forall eff. [Step] -> Eff (ref :: Ref | eff) Unit

#### `runConcurrent`

    runConcurrent :: forall a. Scheduler -> Concurrent a -> a

#### `runExists'`

    runExists' :: forall r f. Exists f -> (forall a. f a -> r) -> r

#### `spawnP`

    spawnP :: forall a. a -> Concurrent (IVar a)

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



