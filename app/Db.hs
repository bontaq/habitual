module Db where

import Control.Applicative
import Data.Time.Clock.POSIX
import qualified Data.Text as T
import Text.Trifecta

type EntityId      = Integer
type Attribute     = String
type Value         = Either String Integer
type TransactionId = Integer
type Operation     = Bool -- indicates if it has been added or removed

data Row = Row EntityId Attribute Value TransactionId Operation
instance Show Row where
  show (Row e a (Left v) t r) =
    (show e) <> "," <> a <> "," <> v <> "," <> (show t) <> "," <> (show r) <> "\n"
  show (Row e a (Right v) t r) =
    (show e) <> "," <> a <> "," <> (show v) <> "," <> (show t) <> "," <> (show r)

data PartialRow = PartialRow EntityId Attribute Value

writeDb :: String -> IO ()
writeDb = appendFile "./db.txt"

mkTx :: PartialRow -> Operation -> IO Row
mkTx (PartialRow e a v) retract = do
  -- time <- getPOSIXTime
  pure $ Row e a v 1 retract

insert :: PartialRow -> IO ()
insert row = mkTx row False >>= writeDb . show

retract :: PartialRow -> IO ()
retract row = mkTx row True >>= writeDb . show

parseRow :: Parser Row
parseRow = do
  entityId <- integer
  char ','
  attribute <- manyTill anyChar (try (char ','))
  value <- manyTill anyChar (try (char ','))
  txId <- integer
  char ','
  rawOp <- try (string "False") <|> (string "True")
  skipSome (char '\n')
  let op = case rawOp of
        "False" -> False
        "True"  -> True
  pure $ Row entityId attribute (Left value) txId op

readDb :: IO (Maybe [Row])
readDb = do
  -- raw <- readFile "./db.txt"
  rows <- parseFromFile (many parseRow) "./db.txt"
  pure $ rows

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
