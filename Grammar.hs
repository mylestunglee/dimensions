module Grammar (parse) where

import Data.Char (digitToInt)
import Parsing
import Declarations

transform :: (Int -> Int) -> Grammar a -> Grammar a
transform f g
  = map transform' g
  where
    transform' :: Rule a -> Rule a
    transform' (RSeq ts) = RSeq (map transform'' ts)
    transform' (RAlt ts) = RAlt (map transform'' ts)
    transform' (ROpt t)  = ROpt (transform'' t)
    transform' (RRep t)  = RRep (transform'' t)

    transform'' :: Token a -> Token a
    transform'' (NonTerm nt) = NonTerm (f nt)
    transform'' t            = t

link :: RuleID -> Grammar a -> Grammar a
link i g
  = transform link' g
  where
    link' :: RuleID -> RuleID
    link' nt
      | nt == -1  = i
      | nt <  -1  = nt + 1
      | otherwise = nt

shift :: Int -> Grammar a -> Grammar a
shift s g
  = transform (s +) g

-- Joins grammar g with grammar h where non-terminals of rule ID of rid refer to
-- the first rule of h
infixl 0 >->
(>->) :: Grammar a -> Grammar a -> Grammar a
(>->) g h
  = link n g ++ transform (n +) h
  where
    n = length g

spaceGrammar = [
  RRep (Term ' ') -- 0: space -> {' '}
  ]

rationalGrammar = [
  RSeq [NonTerm 1, NonTerm 4], -- 0: rational -> number decimal
  RSeq [NonTerm 2, NonTerm 3], -- 1: number -> digit digits]
  RAlt (map Term ['0'..'9']),  -- 2: digit -> '0' | .. | '9'
  RRep (NonTerm 2),            -- 3: digits -> {digit})
  ROpt (NonTerm 5),            -- 4: decimal -> [decimal']
  RSeq [Term '.', NonTerm 1]   -- 5: decimal' -> '.' number
  ]

readRational :: ParseTree Char -> Rational
readRational (PTSeq _ [ptn, ptd])
  = toRational (read (unparse ptn) :: Integer) + readDecimal ptd
  where
    readDecimal :: ParseTree Char -> Rational
    readDecimal (PTOpt _ (Just (PTSeq _ [_, ptn])))
      = (foldr (\a b -> a + b / 10) 0) (0 : map (toRational . digitToInt) (unparse ptn))
    readDecimal _
      = 0

nameGrammar = [
  RSeq [NonTerm 1, NonTerm 2], -- 0: name -> char str
  RAlt (map Term ['a'..'z']),  -- 1: char -> 'a' | .. | 'z'
  RRep (NonTerm 1)             -- 2: str -> {char}
  ]

expressionGrammar = [
  RSeq [NonTerm 4, NonTerm 1],                                      --  0: expr -> term term'
  RRep (NonTerm 2),                                                 --  1: term' -> {term''}
  RSeq [NonTerm (-1), NonTerm 3, NonTerm (-1), NonTerm 4],          --  2: term'' -> space termop space term
  RAlt [Term '+', Term '-'],                                        --  3: termop -> '+' | '-'
  RSeq [NonTerm 5, NonTerm 10, NonTerm 6],                          --  4: term -> negative factor factor'
  RRep (Term '-'),                                                  --  5: negative -> {'-'}
  RRep (NonTerm 7),                                                 --  6: factor' -> {factor''}
  RSeq [NonTerm (-1), NonTerm 8, NonTerm (-1), NonTerm 10],         --  7: factor'' -> space factorop space factor
  ROpt (NonTerm 9),                                                 --  8: factorop -> [factorop']
  RAlt [Term '*', Term '/'],                                        --  9: factorop' -> '*' | '/'
  RSeq [NonTerm 13, NonTerm 11],                                    -- 10: factor -> power power'
  RRep (NonTerm 12),                                                -- 11: power' -> {power''}
  RSeq [NonTerm (-1), Term '^', NonTerm (-1), NonTerm 13],          -- 12: power'' -> space '^' space power
  RAlt [NonTerm (-2), NonTerm (-3), NonTerm 14, NonTerm 15],        -- 13: power -> rational | name | bracket | abs
  RSeq [Term '(', NonTerm (-1), NonTerm 0, NonTerm (-1), Term ')'], -- 14: bracket -> '(' space expr space ')'
  RSeq [Term '|', NonTerm (-1), NonTerm 0, NonTerm (-1), Term '|']  -- 15: abs -> '|' space expr space '|'
  ]

grammar = expressionGrammar >-> spaceGrammar >-> rationalGrammar >-> nameGrammar

readTypedExpression :: ParseTree Char -> TypedExpression
readTypedExpression (PTSeq 0 [pt, PTRep 1 pts])
  = foldl appendTerm (readTypedExpression pt) pts
  where
    appendTerm :: TypedExpression -> ParseTree Char -> TypedExpression
    appendTerm te (PTSeq 2 [_, PTAlt 3 (PTToken '+'), _, pt])
      = Add te (readTypedExpression pt)
    appendTerm te (PTSeq 2 [_, PTAlt 3 (PTToken '-'), _, pt])
      = Sub te (readTypedExpression pt)

readTypedExpression (PTSeq 4 [PTRep 5 pts, pt, PTRep 6 pts'])
  = (uncurry id) (foldl appendFactor (id, term) pts')
  where
    term = foldl (const . Neg) (readTypedExpression pt) pts
    -- Uses functions as a pointer to insert next divide subtree
    appendFactor :: (TypedExpression -> TypedExpression, TypedExpression) ->
      ParseTree Char -> (TypedExpression -> TypedExpression, TypedExpression)
    appendFactor (f, te) (PTSeq 7 [_, PTOpt 8 (Just (PTAlt 9 (PTToken '/'))), _, pt])
      = (\te' -> f (Div te' (readTypedExpression pt)), te)
    appendFactor (f, te) (PTSeq 7 [_, _, _, pt])
      = (\te' -> Mult (f te) te', readTypedExpression pt)

readTypedExpression (PTSeq 10 [pt, PTRep 11 []])
  = readTypedExpression pt
readTypedExpression (PTSeq 10 [pt, PTRep 11 (PTSeq 12 [_, _, _, pt'] : pts)])
  = Pow (readTypedExpression pt) (readTypedExpression (PTSeq 10 [pt', PTRep 11 pts]))

readTypedExpression (PTAlt 13 pt @ (PTSeq 17 _))
  = Val (readRational pt, [])
readTypedExpression (PTAlt 13 pt @ (PTSeq 23 _))
  = Val (1, [(unparse pt, 1)])
readTypedExpression (PTAlt 13 (PTSeq 14 [_, _, pt, _, _]))
  = readTypedExpression pt
readTypedExpression (PTAlt 13 (PTSeq 15 [_, _, pt, _, _]))
  = Abs (readTypedExpression pt)

parse :: String -> (Either TypedExpression String, String)
parse str
  | parseError pt = (Right (unparse pt), ts)
  | otherwise     = (Left (readTypedExpression pt), ts)
  where
    (pt, ts) = (parser grammar) str
