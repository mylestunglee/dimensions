module Declarations where

type Unit = String
type Edge = (Unit, Unit, Rational)
type Graph = [Edge]
type Graphs = [Graph]
type Dimension = Rational
type ValueType = (Unit, Dimension)
type ValueTypes = [ValueType]
type Value = Rational
type TypedValue = (Value, ValueTypes)
type Aliases = [(Unit, (ValueTypes, Rational))]
type Environment = (Graphs, Aliases)

data TypedExpression =
  Add TypedExpression TypedExpression |
  Sub TypedExpression TypedExpression |
  Mult TypedExpression TypedExpression |
  Div TypedExpression TypedExpression |
  Pow TypedExpression TypedExpression |
  Neg TypedExpression |
  Abs TypedExpression |
  Val TypedValue
