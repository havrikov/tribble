import saarland.cispa.se.tribble.dsl._

Grammar(
  'start := 'object | 'array,
  'object := "{" ~ 'members.? ~ "}",
  'members := 'pair | 'pair ~ "," ~ 'members,
  'pair := 'string ~ ":" ~ 'value,
  'array := "[" ~ 'elements.? ~ "]",
  'elements := 'value | 'value ~ "," ~ 'elements,
  'value := 'string | 'number | 'object | 'array | "true" | "false" | "null",
  'string := "\"" ~ 'chars.? ~ "\"",
  'chars := 'char | 'char ~ 'chars,
  'char :=
    """[^\"\\\\]""".regex
      | "\\\""
      | "\\\\"
      | "\\/"
      | "\\b"
      | "\\f"
      | "\\n"
      | "\\r"
      | "\\t"
      | "\\u" ~ "[0-9A-Fa-f]{4}".regex,
  'number := 'int
    | 'int ~ 'frac
    | 'int ~ 'exp
    | 'int ~ 'frac ~ 'exp,
  'int := 'digit | "[1-9]".regex ~ 'digits | "-" ~ 'digit | "-" ~ "[1-9]".regex ~ 'digits,
  'frac := "." ~ 'digits,
  'exp := 'e ~ 'digits,
  'digits := 'digit | 'digit ~ 'digits,
  'digit := "[0-9]".regex,
  'e := "e" | "e+" | "e-" | "E" | "E+" | "E-"
)
