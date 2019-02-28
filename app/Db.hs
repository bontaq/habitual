module Db where

import Data.Time.Clock.POSIX

type EntityId      = Integer
type Attribute     = String
type Value         = Either String Integer
type TransactionId = Integer
type Operation     = Bool -- indicates if it has been added or removed

data FullRow = EntityId Attribute Value TransactionId Operation
data Row = EntityId Attribute Value

mkTx :: Row -> Operation -> IO ()
mkTx row retract = do
  time <- getPOSIXTime
  pure $ row retract

insert :: Row -> Bool
insert row = mkTx True

retract :: Row -> Bool
retract row = mkTx False

-- what's the schema like?
-- day should be derived by time
-- 1 :food/calories 1234
-- 1 :food/name     "bread"

-- [:db/add entity-id attribute value]
-- db add should assign a db/txInstant

-- [:db/retract entity-id attribute value]

-- '[:find ?txInstant .
--   :where [?a :artist/name "The Rolling Stones" ?tx]
--          [?tx :db/txInstant ?txInstant]]

-- how 2 build rulez engine buhhhhh
