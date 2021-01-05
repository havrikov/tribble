import de.cispa.se.tribble.dsl._

// translated from https://github.com/antlr/grammars-v4/blob/master/json/JSON.g4
// as of commit f8a13794c4159b6022cc655d28345d8fe54aeae9

Grammar(
  'json := 'value,

  'obj := "{" ~ 'OWS ~ 'pair ~ 'OWS ~ ("," ~ 'OWS ~ 'pair).rep ~ 'OWS ~ "}" | "{" ~ 'OWS ~ "}",

  'pair := 'STRING ~ 'OWS ~ ":" ~ 'OWS ~ 'value,

  'arr := "[" ~ 'OWS ~ 'value ~ 'OWS ~ ("," ~ 'OWS ~ 'value).rep ~ 'OWS ~ "]" | "[" ~ 'OWS ~ "]",

  'value := 'STRING
    | 'NUMBER
    | 'obj
    | 'arr
    | "true"
    | "false"
    | "null"
  ,

  'STRING := "\"" ~ ('ESC | 'SAFECODEPOINT).rep ~ "\"",

  'ESC := "\\" ~ ("""[\"\\/bfnrt]""".regex | 'UNICODE),

  'UNICODE := "u" ~ 'HEX ~ 'HEX ~ 'HEX ~ 'HEX,

  'HEX := "[0-9a-fA-F]".regex,

  'SAFECODEPOINT := "[^\u0000-\u001F\\\"\\\\]".regex,

  'NUMBER := "-".? ~ 'INT ~ ("." ~ "[0-9]+".regex).? ~ 'EXP.?,

  'INT := "0" | "[1-9][0-9]*".regex,

  'EXP := ("E" | "e") ~ ("+" | "-").? ~ 'INT,

  'OWS := 'WS.rep,

  'WS := " " | "\t" | "\n" | "\r"
)
