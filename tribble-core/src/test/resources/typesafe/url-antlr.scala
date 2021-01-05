import de.cispa.se.tribble.dsl._

// translated from https://github.com/antlr/grammars-v4/blob/master/url/url.g4
// as of commit 320f7f284210754b37b72b6e2fe90bb999d74847

Grammar(
  'url := 'uri,

  'uri := 'scheme ~ "://" ~ 'login.? ~ 'host ~ (":" ~ 'port).? ~ ("/" ~ 'path).? ~ 'query.? ~ 'frag.? ~ 'WS.?,

  'scheme := 'string,

  'host := "/".? ~ 'hostname,

  'hostname := 'string ~ ("." ~ 'string).rep
              | "[" ~ 'v6host ~ "]",

  'v6host := "::".? ~ ('string | 'DIGITS) ~ ((":" | "::") ~ ('string | 'DIGITS)).rep,

  'port := 'DIGITS,

  'path := 'string ~ ("/" ~ 'string).rep ~ "/".?,

  'user := 'string,

  'login := 'user ~ (":" ~ 'password).? ~ "@",

  'password := 'string,

  'frag := "#" ~ ('string | 'DIGITS),

  'query := "?" ~ 'search,

  'search := 'searchparameter ~ ("&" ~ 'searchparameter).rep,

  'searchparameter := 'string ~ ("=" ~ ('string | 'DIGITS | 'HEX)).?,

  'string := 'STRING,

  'DIGITS := "[0-9]+".regex,

  'HEX := "(%[a-fA-F0-9][a-fA-F0-9])+".regex,

  'STRING := ("[a-zA-Z~0-9]".regex | 'HEX) ~ ("[a-zA-Z0-9.+-]".regex | 'HEX).rep,

  'WS := "[\r\n]+".regex
)
