module Db where

import Data.Time.Clock.POSIX

type EntityId      = Integer
type Attribute     = String
type Value         = Either String Integer
type TransactionId = Integer
type Operation     = Bool -- indicates if it has been added or removed

data Row = EntityId Attribute Value TransactionId Operation

mkTx = undefined

insert :: Row -> Operation -> Bool
insert row retract = undefined

retract :: Row -> Bool
retract row = insert row False

-- what's the schema like?
-- day should be derived by time
-- 1 :food/calories 1234
-- 1 :food/name     "bread"

-- [:db/add entity-id attribute value]
-- db add should assign a db/txInstant

-- [:db/retract entity-id attribute value]
