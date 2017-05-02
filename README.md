# freer-persistent
freer-effects wrapper for persistent queries

-----

freer-persistent uses free monads and extensible effects to turn persistent queries into easily testible effects.

It defines a new effect called `Db` that corresponds to a subset of the queries performable by persistent.
When you use only this subset, you'll be able to mock the database to write 100% pure tests. 
If you need to use a more advanced query, that's ok too, but that'll require the `SQL` effect.
This effect cannot be mocked out, and that'll be reflected in the type system, so you know which of your functions
are purely testable and which ones need test databases to test. 


Here's an example:

```haskell
import Control.Monad.Freer.Sql

getUsers :: (Member Db r) => Eff r '[Entity User]
getUsers = do
  selectList [] [Desc UserAge]
```

If we want to run this effect against a DB:

```haskell
runM . {otherEffectRunners} . runSql {connectionPool} runDb $ getUsers
```

Running the `SQL` effect interprets to `IO`.

Since `selectList` is one of the simple persistent queries supported, we can also interpret `getUsers` against a mock db:

```haskell
import Control.Monad.Freer.Fresh

emptyDb = makeDb []

dbWithUsers = makeDb [ MkTable [ (1, User "ben" 26)
                               , (2, User "alice" 34)
                               ]
                     ]

emptyDbResults = runFresh' 3 . mockDb emptyDb $ getUsers
-- (resultDb, [])

dbWithUsersResults = runFresh' 3 . mockDb dbWithUsers $ getUsers
-- (resultDb, [Entity 2 (User "alice" 34), Entity 1 (User "ben" 26)])
```

Current supported queries are:

* `insert`
* `insertMany`
* `insertUnique`
* `get`
* `selectFirst`
* `selectList`
* `update`

`Control.Monad.Freer.Sql` exports the other queries from `Database.Persistent` as well
to make it easy to use a combination of simple and more complicated queries:

```haskell
celebrateBirthday :: (Member Db r, Member SQL r) => Eff r () 
celebrateBirthday = do
  benIds <- fmap entityKey <$> selectList [UserName ==. "ben"] []
  sql $ updateWhere [UserId <-. benIds] [UserAge +=. 1]
```

`sql` wraps the query directly from persistent. Notice how we end up with two effects here, one for the mockable db interactions and one for everything else. 

To run:

`runM . {otherEffectRunners} . runSql {connectionPool} . runDb $ celebrateBirthday`

If you wanted to (though I don't have a good reason why you would of the top of my head, hmu if you got something), you could mock the `Db` effect and still run the leftover `SQL` effects.

`runM . {otherEffectRunners} . runSql {connectionPool} . runFresh' 0 . mockDb emptyDb $ celebrateBirthday`
