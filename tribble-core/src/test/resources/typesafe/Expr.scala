import de.cispa.se.tribble.dsl._

Grammar(
  'start := 'expr/*@3*/ ~ "\n".?,
  'expr := 'term/*@2*/ | 'term/*@3*/ ~ "+" ~ 'expr | 'term/*@4*/ ~ "-" ~ 'expr/*@1*/,
  'term := 'factor | 'term ~ "*" ~ 'factor/*@1*/ | 'term/*@1*/ ~ "/" ~ 'factor/*@2*/,
  'factor := 'literal | "(" ~ 'expr/*@2*/ ~ ")" | "+"/*@1*/ ~ 'factor/*@3*/ | "-"/*@1*/ ~ 'factor/*@4*/,
  'literal := 'number | 'variable,
  'variable := "[a-z]".regex,
  'number := "[0-9]+".regex
)
