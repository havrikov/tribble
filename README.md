# tribble 0.1

Fuzzing powered by grammar coverage.

## Building tribble

Building and running tribble requires Java version `1.8` or greater.  
Build tribble by running `./gradlew build` (or `.\gradlew.bat build` on Windows) in the project's root directory.  

## Running tribble

When the build completes, there should be a runnable jar file `tribble-0.1.jar` located in `build/libs`.
Let us move and rename the artifact for convenience:

```bash
mv /build/libs/tribble-0.1.jar tribble.jar
```

Executing `java -jar tribble.jar --help` will print out all available flags and options.  
Let us consider some common use cases for tribble:

### Generating K-Path File Sets
tribble can generate sets of files with full `k-path` coverage. 
For example to generate a set of Markdown files no deeper than 30 derivations with full `2-path` coverage in the directory `out` (the default value) we would execute the following:

```bash
java -jar tribble.jar generate --mode=2-path-30 --suffix=.md --grammar-file=src/test/resources/typesafe/Markdown.scala
```  

It is also possible to leave out the depth restriction, in which case `--mode=2-path-30` becomes just `--mode=2-path` and the generated files are minimal in size.

## Grammars
There are two formats for tribble grammars: a text-based format and a Scala DSL-based one.  

### Scala DSL

The Scala DSL variant profits from type checking and syntax highlighting in IDEs but has a limitation on the size of the grammar.
If a `StackOverflowError` is thrown during compilation, increasing the available stack size usually helps: `-Xss1g`.  
Sometimes, however a `ToolBoxError: reflective compilation has failed` is thrown indicating that the grammar is simply too large and the scala compiler generates a method exceeding the 64kb limit of the JVM.
If this happens, consider switching to the text-based grammar format presented further down.

```scala
// optional import statement which enables syntax highlighting and type checking in IDEs
import saarland.cispa.se.tribble.dsl._

Grammar(
  'Grammar := 'Import.? ~ "Grammar" ~ "(" ~ 'Production ~ ("," ~ 'Production).rep ~ ")",
  'Import saarland.cispa.se.tribblel._\n",
  'Production := 'Reference ~ ":=" ~ 'Alternation,
  'Alternation := 'Concatenation ~ ("|" ~ 'Concatenation).rep,
  'Concatenation := 'Atom.rep(1),
  'Atom := ( "(" ~ 'Alternation ~ ")" | 'Regex | 'Literal | 'Reference ) ~ 'Quant.?,
  'Quant := ".?" | ".rep" | ".rep(" ~ 'num ~ ")" | ".rep(" ~ 'num ~ "," ~ 'num ~ ")",
  'num := "0|([1-9][0-9]*)".regex,
  'Reference := "'[A-Za-z][A-Za-z0-9]*".regex,
  'Literal := "\"" ~ ("[^\"\\\\]".regex | "\\" ~ "[nrt\"\\\\]".regex).rep ~ "\"",
  'Regex := "\"" ~ 'regexp ~ "\".regex"
  // NOTE: 'regexp is defined as below
)
```

Here is an example grammar for JSON written using the Scala DSL:
```scala
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
```


### Text Format

The grammars can also be provided in the format described by the following PEG-like form:

```bnf
Grammar ::= Production+
Production ::= NonTerminal '=' Alternation ';'
Alternation ::= Concatenation ( '|' Concatenation )*
Concatenation ::= Atom+
Atom ::= ( '(' Alternation ')' | Regex | Literal | NonTerminal ) Quant?

Quant ::= [?+*] 
        | '{,' num '}' 
        | '{' num ',}' 
        | '{' num ',' num '}'
        
num ::= [0-9]+
NonTerminal ::= '?[A-Za-z0-9_$]+
Literal ::= '"' ( [^"\] | '\' [nrt"\] )* '"'
Regex ::= '/' regexp '/'
```

Java style comments are also allowed anywhere whitespace is allowed:  
`/* block comment */` and `// line comment <EOL>`

`regexp` is defined as the following subset of the underlying implementing library [dk.brics.automaton](http://www.brics.dk/automaton/doc/index.html?dk/brics/automaton/RegExp.html):
```bnf
regexp  ::= unionexp
unionexp ::= interexp ( '|' unionexp )? 
interexp ::= concatexp ( '&' interexp )?
concatexp ::= repeatexp concatexp?

repeatexp ::= repeatexp '?' 
            | repeatexp '*' 
            | repeatexp '+' 
            | repeatexp '{' num '}' 
            | repeatexp '{' num ',}' 
            | repeatexp '{' num ',' num '}' 
            | complexp  
complexp ::= '~' complexp
           | charclassexp
             
charclassexp ::= '[' charclasses ']' 
               | '[^' charclasses ']' 
               | simpleexp  
               
charclasses ::= charclass charclasses?  
charclass ::= charexp ('-' charexp)? 
simpleexp ::= charexp  
            | '.' (any single character) 
            | " <Unicode string without double-quotes> " 
            | '( )' (the empty string) 
            | '(' unionexp ')' 
charexp ::= <Unicode character> (a single non-reserved character) 
          | '\' <Unicode character>  (a single character)
```

In regexes, the `/` character must be escaped as `\/`.  
Additionally, the characters `&` and `~` must be escaped even in character classes with a backslash or double quotes. 
This is because the underlying library is instantiated with the [flags](http://www.brics.dk/automaton/doc/index.html?dk/brics/automaton/RegExp.html) `COMPLEMENT | INTERSECTION`.

To get more familiar with the text format consider the following grammar for JSON:
```bnf
// JSON grammar from http://json.org/

start = object | array;

object = "{" members? "}";
members = pair | pair "," members;
pair = string ":" value;

array = "[" elements? "]";
elements = value | value "," elements;

value = string
      | number
      | object
      | array
      | "true"
      | "false"
      | "null";

string = "\"" chars? "\"";

chars = char | char chars;

char = /[^\"]/
     | "\\\""
     | "\\\\"
     | "\\/"
     | "\\b"
     | "\\f"
     | "\\n"
     | "\\r"
     | "\\t"
     | "\\u" /[0-9A-Fa-f]{4}/;

number = int
       | int frac
       | int exp
       | int frac exp;

int = digit | /[1-9]/ digits | "-" digit | "-" /[1-9]/ digits;

frac = "." digits;
exp = e digits;

digits = digit | digit digits;

digit = /[0-9]/;

e = "e" | "e+" | "e-" | "E" | "E+" | "E-";
```
