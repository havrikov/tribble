import saarland.cispa.se.tribble.dsl._

// a teeny subset of the js grammar

Grammar(
  'Start := 'AdditiveExpression,

  'AdditiveExpression := 'MultiplicativeExpression
    | 'AdditiveExpression ~ ("+" | "-") ~ 'MultiplicativeExpression,

  'MultiplicativeExpression := 'UnaryExpression | 'MultiplicativeExpression ~ ("*" | "/" | "%") ~ 'UnaryExpression,

  'UnaryExpression := "++" ~ 'UnaryExpression
    | "--" ~ 'UnaryExpression
    | "+" ~ 'UnaryExpression
    | "-" ~ 'UnaryExpression
    | "(" ~ 'AdditiveExpression ~ ")"
    | 'DecimalDigits
    | 'Identifier,

  'DecimalDigits := 'DecimalDigit | 'DecimalDigits ~ 'DecimalDigit,
  'DecimalDigit := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9",
  'Identifier := "x" | "y" | "z"
)
