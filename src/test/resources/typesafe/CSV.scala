import saarland.cispa.se.tribble.dsl._

// CSV grammar as specified by https://tools.ietf.org/html/rfc4180

Grammar(
  'file := ('header ~ 'CRLF).? ~ 'record ~ ('CRLF ~ 'record).rep ~ 'CRLF.?,
  'header := 'name ~ ('COMMA ~ 'name).rep,
  'record := 'field ~ ('COMMA ~ 'field).rep,
  'name := 'field,
  'field := 'escaped | 'non_escaped,
  'escaped := 'DQUOTE ~ ('TEXTDATA | 'COMMA | 'CR | 'LF | 'DQUOTE.rep(2, 2)).rep ~ 'DQUOTE,
  'non_escaped := 'TEXTDATA.rep,

  'COMMA := ",",
  'CR := "\r",
  'DQUOTE := "\"",
  'LF := "\n",
  'CRLF := 'CR ~ 'LF,
  'TEXTDATA := "[\\x20-\\x21]".regex | "[\\x23-\\x2B]".regex | "[\\x2D-\\x7E]".regex
)
