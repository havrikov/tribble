import de.cispa.se.tribble.dsl._

// This variant of the regex grammar is limited to non-repeating quantifiers and complements.

Grammar(
  'regexp := 'unionexp,
  'unionexp := 'interexp ~ ("|" ~ 'unionexp).?,
  'interexp := 'concatexp ~ ("&" ~ 'interexp).?,
  'concatexp := 'repeatexp ~ 'concatexp.?,

  'repeatexp := 'complexp ~ "?"
    | 'complexp ~ "*"
    | 'complexp ~ "+"
    | 'complexp ~ "{" ~ 'num ~ "}"
    | 'complexp ~ "{" ~ 'num ~ ",}"
    | 'complexp ~ "{" ~ 'num ~ "," ~ 'num2 ~ "}"
    | 'complexp,

  'complexp := "~".? ~ 'charclassexp,

  'charclassexp := "[" ~ 'charclasses ~ "]"
    | "[^" ~ 'charclasses ~ "]"
    | 'simpleexp,

  'charclasses := 'charclass ~ 'charclasses.?,
  'charclass := 'charexp ~ ("-" ~ 'charexp).?,

  'simpleexp := 'charexp
    | "" // any single character
    | "\"" ~ ("""[^\"\)\(\\]""".regex | """\\""" | """\\"""").rep(1) ~ "\"" // Unicode string without double-quotes
    | "()" // the empty string
    | "(" ~ 'unionexp ~ ")",

  'charexp :=
    """[^\\"\\/\)\(]""".regex // a single non-reserved character
      | "\\" ~ """[\"\\\/\)\(]""".regex, // a single character

  'num := "[0-9]".regex,
  'num2 := "1[0-9]".regex

)
