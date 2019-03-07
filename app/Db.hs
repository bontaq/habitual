module Db where

import Data.Time.Clock.POSIX

type EntityId      = Integer
type Attribute     = String
type Value         = Either String Integer
type TransactionId = POSIXTime
type Operation     = Bool -- indicates if it has been added or removed

data Row = Row EntityId Attribute Value TransactionId Operation
           deriving Show
data PartialRow = PartialRow EntityId Attribute Value

mkTx :: PartialRow -> Operation -> IO Row
mkTx (PartialRow e a v) retract = do
  time <- getPOSIXTime
  pure $ Row e a v time retract

insert :: PartialRow -> IO Row
insert row = mkTx row False

retract :: PartialRow -> Bool
retract row = mkTx row False

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
