module Graphs (environment) where

import Declarations
import Data.Ratio((%))

environment = (graphs, aliases) :: Environment

graphs = [
  distanceGraph,
  massGraph,
  timeGraph 365.25,
  angleGraph,
  dataGraph]

aliases = [("litre", ([("decimeter", 3)], 1))]

generateSISubgraph :: Unit -> Graph
generateSISubgraph u
  = map (\(s, t, w) -> (s ++ u, t ++ u, w)) [
      ("yotta", "zetta", 1000),
      ("zetta", "exa"  , 1000),
      ("exa"  , "peta" , 1000),
      ("peta" , "tera" , 1000),
      ("tera" , "giga" , 1000),
      ("giga" , "mega" , 1000),
      ("mega" , "kilo" , 1000),
      ("kilo" , "hecto", 10),
      ("hecto", "deca" , 10),
      ("deca" , ""     , 10),
      (""     , "deci" , 10),
      ("deci" , "centi", 10),
      ("centi", "milli", 10),
      ("milli", "micro", 1000),
      ("micro", "nano" , 1000),
      ("nano" , "pico" , 1000),
      ("pico" , "femto", 1000),
      ("femto", "atto" , 1000),
      ("atto" , "zepto", 1000),
      ("zepto", "yocto", 1000)]


distanceGraph = generateSISubgraph "meter" ++ [
-- UK imperial units
  ("mile"         , "furlong", 3),
  ("furlong"      , "chain"  , 10),
  ("chain"        , "yard"   , 22),
  ("yard"         , "foot"   , 3),
  ("foot"         , "inch"   , 12),
  ("nautical mile", "cable"  , 10),
  ("cable"        , "fathom" , 100),
  ("fathom"       , "feet"   , 6),

  -- Mixed
  ("inch", "centimeter", 2.54)]

massGraph = generateSISubgraph "gram" ++ [
  -- UK imperial units
  ("ton"  , "stone", 160),
  ("stone", "pound", 14),
  ("pound", "ounce", 16),

  -- Mixed
  ("pound", "kilogram", 0.45359237)]

timeGraph year = generateSISubgraph "second" ++ [
  ("millennium", "century", 10),
  ("century"   , "decade" , 10),
  ("decade"    , "year"   , 10),
  ("year"      , "month"  , 12),
  ("fortnight" , "week"   , 2),
  ("week"      , "day"    , 7),
  ("day"       , "year"   , year),
  ("day"       , "hour"   , 24),
  ("hour"      , "minute" , 60),
  ("minute"    , "second" , 60)]

angleGraph = [
  ("turn", "degree", 360),
  ("degree", "radian", toRational (pi / 180)),
  ("degree", "gon", 10 % 9)]

dataGraph = ("bit", "byte", 8) :
    concat [f u | f <- [(\u -> generateDataSubgraph ('a' : u) 1000),
                        (\u -> generateDataSubgraph ('i' : u) 1024)],
                  u <- ["bit", "byte"]]
  where
    generateDataSubgraph :: String -> Rational -> Graph
    generateDataSubgraph u w
      = map (\(s, t, w) -> (s ++ u, t ++ u, w)) [
          ("yott", "zett", w),
          ("zett", "ex"  , w),
          ("ex"  , "pet" , w),
          ("pet" , "ter" , w),
          ("ter" , "gig" , w),
          ("gig" , "meg" , w),
          ("meg" , "kil" , w)]
