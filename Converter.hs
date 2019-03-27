module Converter (convert, normalise, standardise) where

import Declarations
import Ratio ((^%))
import Data.Set (Set, insert, member, empty)
import Data.Maybe (mapMaybe, listToMaybe, catMaybes)
import Data.Either (partitionEithers)

-- Given a list of mixed success results, select a successful result
lookupMaybe :: [Maybe a] -> Maybe a
lookupMaybe = listToMaybe . catMaybes

-- Given a unit conversion graph g, finds a scale from unit u to unit v
unitConvert :: Graph -> Unit -> Unit -> Maybe Rational
unitConvert g u v
  = unitConvert' u empty
  where
    -- Bidirectional unit conversion graph
    g' = g ++ map (\(s, t, w) -> (t, s, 1 / w)) g

    unitConvert' :: Unit -> Set Unit -> Maybe Rational
    unitConvert' c ps
      | c == v    = Just 1
      | otherwise = lookupMaybe (map next as)
      where
        -- Recursively finds a scale from unit x to v
        next :: Edge -> Maybe Rational
        next (_, x, w)
          = fmap (w *) (unitConvert' x (insert c ps))

        -- Finds unseen adjacent nodes with bidirectional edges
        as = filter (\(s, _, _) -> s == c && not (member s ps)) g'

-- Using all unit conversion graphs, apply convert()
tryConvert :: Graphs -> ValueType -> ValueType -> Maybe Rational
tryConvert gs (u, d) (v, e)
  | d == e    = (lookupMaybe [unitConvert g u v | g <- gs]) >>= (^% d)
  | otherwise = Nothing

convert :: Graphs -> ValueTypes -> ValueTypes -> Maybe Rational
convert = multiConvert

multiConvert :: Graphs -> ValueTypes -> ValueTypes -> Maybe Rational
multiConvert gs ss ts
  = foldl ((<*>) . fmap (*)) (Just 1)
    [lookupMaybe [tryConvert gs s t | t <- ts] | s <- ss]

normaliseConvert :: Graphs -> ValueTypes -> (ValueTypes, Rational)
normaliseConvert _ []
  = ([], 1)
normaliseConvert gs ts
  = (t : ts'', w * w')
  where
    (ts'', w') = normaliseConvert gs ts'
    (t : ts', w) = normaliseConvert' gs ts

normaliseConvert' :: Graphs -> ValueTypes -> (ValueTypes, Rational)
normaliseConvert' gs ((u, d) : ts)
  = ((u, d + sum ds) : ts', product ws)
  where
    (ws, ds) = unzip wds
    (ts', wds) = partitionEithers (map tryConvert' ts)
    tryConvert' :: ValueType -> Either ValueType (Rational, Dimension)
    tryConvert' (v, e)
      = case tryConvert gs (v, e) (u, e) of
          Just w -> Right (w, e)
          _      -> Left  (v, e)

-- Normalises a typed unit
normalise :: Graphs -> TypedValue -> TypedValue
normalise gs (v, ts)
  = (v * w, ts')
  where
    (ts', w) = normaliseConvert gs ts

-- Removes aliased types on single dimensional types
standardise :: Aliases -> TypedValue -> TypedValue
standardise as (v, ts)
  = (v * product ws, concat tss)
  where
    (tss, ws) = unzip (map (standardise' . fst) ts)
    standardise' :: Unit -> (ValueTypes, Rational)
    standardise' u
      = case lookup u as of
          Just tsw -> tsw
          _        -> ([(u, 1)], 1)
