module Calculator (simplify) where

import Declarations
import Converter (convert, normalise, standardise)
import Ratio ((^%))
import Control.Arrow (first, second)

add :: Graphs -> TypedValue -> TypedValue -> Maybe TypedValue
add bg (u, s) (v, t)
  = fmap (\y -> (u * y + v, t)) (convert bg s t)

sub :: Graphs -> TypedValue -> TypedValue -> Maybe TypedValue
sub bg (u, s) (v, t)
  = fmap (\y -> (u * y - v, t)) (convert bg s t)

mult :: TypedValue -> TypedValue -> TypedValue
mult (u, s) (v, t)
  = (u * v, s ++ t)

divide :: TypedValue -> TypedValue -> Maybe TypedValue
divide _ (0, _)
  = Nothing
divide u v
  = fmap (mult u) (pow v (-1, []))

pow :: TypedValue -> TypedValue -> Maybe TypedValue
pow (u, s) (v, [])
  = fmap (\w -> (w, map (second (* v)) s)) (u ^% v)
pow _ _
  = Nothing

simplify :: Environment -> TypedExpression -> TypedExpression
simplify (gs, as) te @ (Add te1 te2)  = simplifyBin (gs, as) (add gs)          Add  te1 te2
simplify (gs, as) te @ (Sub te1 te2)  = simplifyBin (gs, as) (sub gs)          Sub  te1 te2
simplify env      te @ (Mult te1 te2) = simplifyBin env      ((Just .) . mult) Mult te1 te2
simplify env      te @ (Div te1 te2)  = simplifyBin env      divide            Div  te1 te2
simplify env      te @ (Pow te1 te2)  = simplifyBin env      pow               Pow  te1 te2
simplify env      te @ (Neg te1)      = simplifyUni env      (first negate)    Neg  te1
simplify env      te @ (Abs te1)      = simplifyUni env      (first abs)       Abs  te1
simplify (gs, as)      (Val v)        = Val (normalise gs (standardise as v))

simplifyBin :: Environment -> (TypedValue -> TypedValue -> Maybe TypedValue) ->
  (TypedExpression -> TypedExpression -> TypedExpression) -> TypedExpression ->
  TypedExpression -> TypedExpression
simplifyBin (gs, as) f g te1 te2
  = case (te1', te2') of
      (Val u, Val v) -> case (f u v) of
              Just w -> Val (normalise gs w)
              _      -> g te1' te2'
      _              -> g te1' te2'
  where
    te1' = simplify (gs, as) te1
    te2' = simplify (gs, as) te2

simplifyUni :: Environment -> (TypedValue -> TypedValue) -> (TypedExpression ->
  TypedExpression) -> TypedExpression -> TypedExpression
simplifyUni env f g te1
  = case te1' of
      Val v -> Val (f v)
      _     -> g te1'
  where
    te1' = simplify env te1
