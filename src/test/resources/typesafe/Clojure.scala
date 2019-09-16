import saarland.cispa.se.tribble.dsl._

// Clojure grammar translated from https://github.com/antlr/grammars-v4/blob/master/clojure/Clojure.g4

Grammar(

  'file := ('form ~ 'ws).rep(1),

  'form := 'literal
    | 'list
    | 'vector
    | 'map
    | 'reader_macro
  ,

  'forms := ('form ~ 'ws).rep,

  'list := "(" ~ 'forms ~ ")",

  'vector := "[" ~ 'forms ~ "]",

  'map := "{" ~ ('form ~ 'ws ~ 'form).rep ~ "}",

  'set := "#{" ~ 'forms ~ "}",

  'reader_macro := 'lambda
    | 'meta_data
    | 'regex
    | 'var_quote
    | 'host_expr
    | 'set
    | 'tag
    | 'discard
    | 'dispatch
    | 'deref
    | 'quote
    | 'backtick
    | 'unquote
    | 'unquote_splicing
    | 'gensym
  ,

  // TJP added "&" (gather a variable number of arguments)
  'quote := "'" ~ 'form,

  'backtick := "`" ~ 'form,

  'unquote := "~" ~ 'form,

  'unquote_splicing := "~@" ~ 'form,

  'tag := "^" ~ 'form ~ 'ws ~ 'form,

  'deref := "@" ~ 'form,

  'gensym := 'symbol ~ "#",

  'lambda := "#(" ~ 'form.rep ~ ")",

  'meta_data := "#^" ~ ('map ~ 'ws ~ 'form | 'form),

  'var_quote := "#'" ~ 'symbol,

  'host_expr := "#+" ~ 'form ~ 'ws ~ 'form,

  'discard := "#_" ~ 'form,

  'dispatch := "#" ~ 'symbol ~ 'ws ~ 'form,

  'regex := "#" ~ 'string, // TODO might want to refine this

  'literal := 'string
    | 'number
    | 'character
    | 'nil
    | 'boolean
    | 'keyword
    | 'symbol
    | 'param_name
  ,

  'number := 'float
    | 'hex
    | 'bin
    | 'bign
    | 'long
  ,

  'character := 'char_named
    | 'char_u
    | 'char_any
  ,


  'keyword := 'macro_keyword | 'simple_keyword,
  'simple_keyword := ":" ~ 'symbol,
  'macro_keyword := "::" ~ 'symbol,

  'symbol := 'ns_symbol | 'symbol | "." | "/" | 'name,

  // Lexers
  //--------------------------------------------------------------------

  'string := "\"" ~ ("[^\\\"]".regex | "\\\"").rep ~ "\"",

  // FIXME: Doesn't deal with arbitrary read radixes, BigNums
  'float := "-".? ~ "[0-9]+".regex ~ 'float_tail
    | "-".? ~ "Infinity"
    | "-".? ~ "NaN"
  ,

  'float_tail := 'float_decimal ~ 'float_exp
    | 'float_decimal
    | 'float_exp
  ,

  'float_decimal := "" ~ "[0-9]+".regex,

  'float_exp := ("e" | "E") ~ "-".? ~ "[0-9]+".regex,

  'hexd := "[0-9a-fA-F]".regex,
  'hex := "0" ~ ("x" | "X") ~ 'hexd.rep(1),
  'bin := "0" ~ ("b" | "B") ~ ("1" | "0").rep(1),
  'long := "-".? ~ "[0-9]+".regex ~ ("l" | "L").?,
  'bign := "-".? ~ "[0-9]+".regex ~ ("n" | "N"),

  'char_u := "\\" ~ "u" ~ "[0-9D-Fd-f]".regex ~ 'hexd.rep(3, 3),
  'char_named := "\\" ~ ("newline"
    | "return"
    | "space"
    | "tab"
    | "formfeed"
    | "backspace"),
  'char_any := "\\" ~ "".regex,

  'nil := "nil",

  'boolean := "true" | "false",

  'ns_symbol := 'name ~ "/" ~ 'symbol,

  'param_name := "%" ~ ("[1-9][0-9]*".regex | "&").?,

  // Fragments
  //--------------------------------------------------------------------


  'name := 'symbol_head ~ 'symbol_rest.rep ~ (":" ~ 'symbol_rest.rep(1)).rep,


  'symbol_head := "~(]|[|[0-9]|[`'\\\"\\/:@#%(){} \n\r\t,~])".regex,

  'symbol_rest := 'symbol_head | "[0-9]".regex | "",

  'ws := "[ \\\n\\\r\\\t,]".regex

)
