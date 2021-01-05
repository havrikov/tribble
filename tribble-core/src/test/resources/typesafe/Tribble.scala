import de.cispa.se.tribble.dsl._

// tribble grammar for tribble grammars
// TODO allow annotating max depth constraints, MinLength, MaxLength
// TODO add comments and whitespace

Grammar(
  'Grammar := 'Import.? ~ "Grammar" ~ "(" ~ 'Production ~ ("," ~ 'Production).rep ~ ")",
  'Import := "import de.cispa.se.tribble.dsl._\n",
  'Production := 'Reference ~ ":=" ~ 'Alternation,
  'Alternation := 'Concatenation ~ ("|" ~ 'Concatenation).rep,
  'Concatenation := 'Atom.rep(1) ~ ("@@" ~ 'prob).?,
  'Atom := ("(" ~ 'Alternation ~ ")" | 'Regex | 'Literal | 'Reference) ~ 'Quant.?,
  'Quant := ".?" | ".rep" | ".rep(" ~ 'num ~ ")" | ".rep(" ~ 'num ~ "," ~ 'num ~ ")",
  'num := "0|([1-9][0-9]*)".regex,
  'Reference := "'[A-Za-z][A-Za-z0-9]*".regex,
  'Literal := "\"" ~ ("[^\"\\\\]".regex | "\\" ~ "[nrt\"\\\\]".regex).rep ~ "\"",
  'Regex := "\"" ~ 'regexp ~ "\".regex",

  // TODO import the regex part from its own file
)
