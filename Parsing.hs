module Parsing where

import Data.Maybe(maybeToList)

type RuleID = Int
data Token a = Ep | End | NonTerm RuleID | Term a
data Rule a = RSeq [Token a]
  | RAlt [Token a]
  | ROpt (Token a)
  | RRep (Token a)
type Grammar a = [Rule a]
data ParseTree a = PTError
  | PTToken a
  | PTSeq RuleID [ParseTree a]
  | PTAlt RuleID (ParseTree a)
  | PTOpt RuleID (Maybe (ParseTree a))
  | PTRep RuleID [ParseTree a] deriving (Show, Eq)
type Parser a = [a] -> (ParseTree a, [a])

parseError :: ParseTree a -> Bool
parseError PTError
  = True
parseError (PTToken _)
  = False
parseError (PTSeq _ pts)
  = any parseError pts
parseError (PTAlt _ pt)
  = parseError pt
parseError (PTOpt _ (Just pt))
  = parseError pt
parseError (PTOpt _ Nothing)
  = False
parseError (PTRep _ pts)
  = any parseError pts

parser :: Eq a => Grammar a -> Parser a
parser gram
  = let funcs @ (func : _) = map (\(n, rule) -> parserRule funcs gram n rule)
                                 (zip [0..] gram) in
    func

-- Generate parser function for a rule
parserRule :: Eq a => [Parser a] -> Grammar a -> RuleID -> Rule a -> Parser a
parserRule funcs gram rid (RSeq []) ts
  = (PTSeq rid [], ts)
parserRule funcs gram rid (RSeq (Ep : s)) ts
  = parserRule funcs gram rid (RSeq s) ts
parserRule _ _ rid (RSeq [End]) ts
  = (PTSeq rid [], ts)
parserRule _ _ _ (RSeq (End : s)) ts
  = (PTError, ts)
parserRule funcs gram rid (RSeq (NonTerm nt : s)) ts
  = let (pt         , ts' ) = (funcs !! nt) ts in
    let (PTSeq _ pts, ts'') = parserRule funcs gram rid (RSeq s) ts' in
    (PTSeq rid (pt : pts), ts'')
parserRule funcs gram rid (RSeq (Term tt : s)) (t : ts)
  = let (PTSeq _ pts, ts') = parserRule funcs gram rid (RSeq s) ts in
    if tt == t
    then (PTSeq rid (PTToken t : pts), ts')
    else (PTSeq rid (PTError : pts), ts)
parserRule _ _ rid (RSeq _) []
  = (PTSeq rid [PTError], [])

parserRule _ _ _ (RAlt []) ts
  = (PTError, ts)
parserRule funcs gram rid (RAlt (alt : alts)) ts
  = let (PTSeq _ [pt], ts') = parserRule funcs gram rid (RSeq [alt]) ts in
    if parseError pt
    then parserRule funcs gram rid (RAlt alts) ts
    else (PTAlt rid pt, ts')

parserRule funcs gram rid (ROpt opt) ts
  = let (PTSeq _ [pt], ts') = parserRule funcs gram rid (RSeq [opt]) ts in
    if parseError pt
    then (PTOpt rid Nothing, ts)
    else (PTOpt rid (Just pt), ts')

parserRule funcs gram rid (RRep rep) ts
  = let (PTSeq _ [pt], ts') = parserRule funcs gram rid (RSeq [rep]) ts in
    if parseError pt
    then (PTRep rid [], ts)
    else let (PTRep _ pts, ts'') = parserRule funcs gram rid (RRep rep) ts' in
    (PTRep rid (pt : pts), ts'')

unparse :: ParseTree a -> [a]
unparse (PTToken t)   = [t]
unparse (PTSeq _ pts) = concatMap unparse pts
unparse (PTAlt _ pt)  = unparse pt
unparse (PTOpt _ mpt) = concatMap unparse (maybeToList mpt)
unparse (PTRep _ pts) = concatMap unparse pts
