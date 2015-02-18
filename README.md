# Module Documentation

## Module Concurrent

### Types

#### `Concurrent`

    type Concurrent a = ContT Step Identity a

#### `ExistsEff`

    data ExistsEff :: (# ! -> * -> *) -> *

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

#### `PutExists`

    data PutExists a
      = PutExists (IVar a) a Step

#### `Scheduler`

    type Scheduler = forall eff. [Step] -> Step -> Eff (ref :: Ref | eff) Unit

#### `SpawnEffExists`

    data SpawnEffExists eff a
      = SpawnEffExists (Eff eff a) (a -> Step)

#### `Step`

    data Step
      = Get (Exists GetExists)
      | New (Exists NewExists)
      | Put (Exists PutExists)
      | SpawnEff (ExistsEff SpawnEffExists)
      | Fork Step Step
      | Stop 


### Values

#### `fork`

    fork :: Concurrent Unit -> Concurrent Unit

#### `get`

    get :: forall a. IVar a -> Concurrent a

#### `mkExistsEff`

    mkExistsEff :: forall f eff a. f eff a -> ExistsEff f

#### `new`

    new :: forall a. Concurrent (IVar a)

#### `put`

    put :: forall a. IVar a -> a -> Concurrent Unit

#### `runConcurrent`

     TODO: It'd be nice if this didn't return a `Maybe a`,
     Think about other representations.

    runConcurrent :: forall a. Scheduler -> Concurrent a -> Maybe a

#### `spawnEff`

    spawnEff :: forall a eff. Eff eff a -> Concurrent a

#### `spawnPure`

    spawnPure :: forall a. a -> Concurrent (IVar a)

#### `stop`

    stop :: forall a. Concurrent a


## Module Concurrent.Scheduler

### Values

#### `nonPreemptive`

    nonPreemptive :: Scheduler



