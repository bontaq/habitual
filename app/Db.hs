module Db where

import           Control.Applicative
import           Data.Time.Clock.POSIX
import qualified Data.Text             as T
import           Text.Trifecta
import Data.Char
import Data.List

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
readDb = parseFromFile (many parseRow) "./db.txt"

data Find = Find String
            deriving Show
data Where = Where (Either String Integer) String (Either String Integer) -- e a v
             deriving Show
data Query = Query [Find] [Where]
             deriving Show

startsWith :: Eq a => a -> [a] -> Bool
startsWith m (c:cs) = m == c
startsWith _  _     = False

--
-- Entity ID handling
--
entityIdFixed' (Left e) = not $ startsWith '?' e
entityIdFixed' (Right e) = True

entityIdFixed (Where (Left e) _ _) = not $ startsWith '?' e
entityIdFixed (Where (Right _) _ _) = True

compareEntity :: Either String Integer -> Integer -> Bool
compareEntity e' e =
  if entityIdFixed' e'
  then (\(Right id) -> id == e) e'
  else True

--
-- Attribute handling
--
attributeFixed' = not . startsWith '?'

attributeFixed (Where _ a _) = not $ startsWith '?' a

compareAttribute :: String -> String -> Bool
compareAttribute a' a =
  if attributeFixed' a'
  then a' == a
  else True

--
-- Value handling
--
valueFixed' (Left e) = not . startsWith '?' $ e
valueFixed' _        = True

valueFixed (Where _ _ (Left v)) = not $ startsWith '?' v
valueFixed (Where _ _ (Right v)) = True

compareValue :: Either String Integer -> Either String Integer -> Bool
compareValue v'@(Left v'') (Left v) =
  if valueFixed' v'
  then v'' == v
  else True
compareValue (Right v') (Right v) = v' == v

--
-- Running a query
--
checkAgainstFixed :: Row -> Where -> Bool
checkAgainstFixed (Row e a v _ _) (Where e' a' v') =
  let
    matchedEntity = compareEntity e' e
    matchedAttribute = compareAttribute a' a
    matchedValue = compareValue v' v
  in
    all (== True) [matchedEntity, matchedAttribute, matchedValue]

runFilter' :: [Row] -> Where -> [Row]
runFilter' rows w =
  filter (flip checkAgainstFixed $ w) rows

runFilter :: (Maybe [Row]) -> Query -> [Row]
runFilter Nothing     _     = []
runFilter (Just rows) query =
  foldr (flip runFilter') rows wheres
  where wheres = (\(Query _ ws) -> ws) $ query
        finds  = (\(Query qs _) -> qs) $ query

--
-- After basic filtering, starting handling unionizing
--

-- this should return all artist names, I think
testQuery =
  Query
  [Find "?name"]
  [ Where (Left "?id") ":artist/name" (Left "world")
  , Where (Left "?id") ":artist/lastname" (Left "?name")
  ]

-- runQuery' og db -> new db -> where -> db
-- filter by artist name -> new db +

runQuery :: (Maybe [Row]) -> Query -> [Row]
runQuery Nothing _ = []
runQuery _ _ = undefined

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

-- [:find ?name
--  :where [1 :artist/name ?name]]
