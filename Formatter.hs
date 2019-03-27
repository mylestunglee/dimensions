module Formatter (format) where

import Declarations
import Data.Ratio((%), numerator, denominator)

-- Obtains the precedence of the operator embedded in TypedExpression
precedence :: TypedExpression -> Int
precedence (Add _ _)  = 0
precedence (Sub _ _)  = 0
precedence (Mult _ _) = 1
precedence (Div _ _)  = 2
precedence (Val _)    = 2
precedence (Pow _ _)  = 3
precedence (Neg _)    = 4
precedence (Abs _)    = 4

-- Pretty formats a TypedExpression
format :: TypedExpression -> String
format te = formatExp te False

formatExp :: TypedExpression -> Bool -> String
formatExp te @ (Add te1 te2)  b = bbracket (formatBin "+" te te1 te2 (>) (>=)) b
formatExp te @ (Sub te1 te2)  b = bbracket (formatBin "-" te te1 te2 (>) (>=)) b
formatExp te @ (Mult te1 te2) b = bbracket (formatBin "*" te te1 te2 (>) (>=)) b
formatExp te @ (Div te1 te2)  b = bbracket (formatBin "/" te te1 te2 (>) (>=)) b
formatExp te @ (Pow te1 te2)  b = bbracket (formatBin "^" te te1 te2 (>=) (>)) b
formatExp te @ (Neg te')      b = bbracket (formatUni "-" te te'     (>)     ) b
formatExp      (Abs te)       _ = "|" ++ formatExp te False ++ "|"
formatExp (Val (v, ts)) b
  | null ts   = formatRational v b
  | v == 1    = formatTypes ts b
  | otherwise = bbracket (formatRational v False ++ " " ++ formatTypes ts False) b

formatBin :: String -> TypedExpression -> TypedExpression -> TypedExpression ->
  (Int -> Int -> Bool) -> (Int -> Int -> Bool) -> String
formatBin op te te1 te2 f1 f2
  = formatUni "" te te1 f1 ++ " " ++ op ++ " " ++ formatUni "" te te2 f2

formatUni :: String -> TypedExpression -> TypedExpression ->
  (Int -> Int -> Bool) -> String
formatUni op te te' f
  = op ++ formatExp te' (f (precedence te) (precedence te'))

bracket :: String -> String
bracket str
  = "(" ++ str ++ ")"

bbracket :: String -> Bool -> String
bbracket str True = bracket str
bbracket str False = str

formatRational :: Rational -> Bool -> String
formatRational v b
  | denominator v == 1 = show (numerator v)
  | otherwise          = bbracket (show (numerator v) ++ " / " ++ show (denominator v)) b

formatTypes :: ValueTypes -> Bool -> String
formatTypes [t] _
  = formatType t
formatTypes ts b
  = bbracket (unwords (map formatType ts)) b

formatType :: ValueType -> String
formatType (unit, dim)
  = unit ++ if dim /= 1 then " ^ " ++ formatRational dim True else ""
