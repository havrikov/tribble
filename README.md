# tribble 1.0.0

Fuzzing powered by grammar coverage.

## Building tribble

Building and running tribble requires Java version `11` or greater.  
Build tribble by running `./gradlew build` (or `.\gradlew.bat build` on Windows) in the project's root directory.  

## Running tribble

When the build completes, there should be a runnable jar file `tribble-1.0.0.jar` located in `tribble-tool/build/libs`.
Let us move and rename the artifact for convenience:

```bash
mv tribble-tool/build/libs/tribble-1.0.0.jar tribble.jar
```

Executing `java -jar tribble.jar --help` will print out all available flags and options.  
Let us consider some common use cases for tribble:

### Generating Uniformly Random File Sets
Let us generate 100 JSON files of approximate size 50 (tree nodes) in the directory `json100`.
  
```bash
java -jar tribble.jar generate --mode=50-random-100 --out-dir=json100 --suffix=.json --grammar-file=tribble-core/src/test/resources/json.tribble
```

### Controlling the Tree Size
For more precise control over the number of nodes in the generated trees the `--mode=min-max-random-n` can be provided
to generate `n` files of sizes between `min` and `max`. E.g. `--mode=22-180-random-100` to generate 100 files between 22 and 180 nodes in size.  
This mode might not be very efficient and so should be used with care.

### Generating K-Path File Sets
tribble can generate sets of files with full `k-path` coverage. 
For example to generate a set of Markdown files no deeper than 30 derivations with full `2-path` coverage in the directory `out` (the default value) we would execute the following:

```bash
java -jar tribble.jar generate --mode=2-path-30 --suffix=.md --grammar-file=tribble-core/src/test/resources/typesafe/Markdown.scala
```  

It is also possible to leave out the depth restriction, in which case `--mode=2-path-30` becomes just `--mode=2-path` and the generated files are minimal in size.

### Reproducing Results
You can add the parameter `--random-seed` to make all runs of tribble reproducible. E.g. `--random-seed=42`.

### Measuring the K-Path Coverage of the Generated Sets
When generating sets of files, tribble can measure the `k-path` coverage achieved.
This is governed by the two parameters `--report-file` and `--report-kcoverage`. For example the configuration
`--report-file=3-path-coverage.csv --report-kcoverage=3` will report the `1-`, `2-`, and `3-path` coverages
achieved by the set generated by this run in the file `3-path-coverage.csv`.  
The default value for `--report-kcoverage` is `4`, while the presence of the `--report-file` parameter determines
whether measurements will be done at all.

### Generating Random Sets According to Annotated Probabilities 
Let us generate 100 JSON files while adhering to probabilities annotated in the grammar, while also never generating optional elements below a tree depth of 10.

```bash
java -jar tribble.jar generate --mode=10-probabilistic-100 --out-dir=json-prob-100 --suffix=.json --grammar-file=tribble-core/src/test/resources/typesafe/JSON.scala
```  

### Generating Random Sets According to Inverted Annotated Probabilities
There are two additional parameters involved in probability-base generation: `--damping` and `--similarity`.  
The actual probabilities used in the generation are calculated from the annotations in several phases:  
1. Missing probability annotations are filled in by uniformly distributing `1 - sum(annotations)` among them
1. The resulting probabilities are scaled up such that their sum is `1.0` if it is not already.
1. All probabilities are recalculated to be `p' = (p + damping) ^ similarity`.

The default values for `--damping` and `--similarity` are `Double.MinPositiveValue` and `1.0`, respectively.  
So if we want to use inverted probabilities for generation we should set `--similarity=-1.0`.

## Grammars
There are two formats for tribble grammars: a text-based format and a Scala DSL-based one.  

### Scala DSL

The preferred way of providing a grammar to tribble is using its Scala DSL variant
because it profits from type checking and syntax highlighting in IDEs.

> Note. If you rely on advanced Scala features to compute (parts of) your grammar programmatically,
> consider looking into the `--loading-strategy=compile` option.
> :warning: This option has a limitation on the size of the grammar.
> If a `StackOverflowError` is thrown during compilation, increasing the available stack size usually helps: `-Xss1g`.  
> Sometimes, however a `ToolBoxError: reflective compilation has failed` is thrown indicating that the grammar is simply too large,
> and the scala compiler generates a method exceeding the 64kb limit of the JVM.
> If this happens, consider switching to the text-based grammar format presented further down.

```scala
// optional import statement which enables syntax highlighting and type checking in IDEs
import de.cispa.se.tribble.dsl._

Grammar(
  'Grammar := 'Import.? ~ "Grammar" ~ "(" ~ 'Production ~ ("," ~ 'Production).rep ~ ")",
  'Import  := "import de.cispa.se.tribble.dsl._\n",
  'Production := 'Reference ~ ":=" ~ 'Alternation,
  'Alternation := 'Concatenation ~ ("|" ~ 'Concatenation).rep,
  'Concatenation := 'Atom.rep(1) ~ ("@@" ~ 'prob).?,
  'Atom := ( "(" ~ 'Alternation ~ ")" | 'Regex | 'Literal | 'Reference ) ~ 'Quant.?,
  'Quant := ".?" | ".rep" | ".rep(" ~ 'num ~ ")" | ".rep(" ~ 'num ~ "," ~ 'num ~ ")",
  'num := "0|([1-9][0-9]*)".regex,
  'prob := "[0-9.xXa-fA-FpP-]+".regex,
  'Reference := "'[A-Za-z][A-Za-z0-9]*".regex,
  'Literal := "\"" ~ ("[^\"\\\\]".regex | "\\" ~ "[nrt\"\\\\]".regex).rep ~ "\"",
  'Regex := "\"" ~ 'regexp ~ "\".regex"
  // NOTE: 'regexp is defined as below
)

```

Here is an example grammar for JSON written using the Scala DSL:
```scala
import de.cispa.se.tribble.dsl._
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
Concatenation ::= Atom+ ('@@' Probability)?
Atom ::= ( '(' Alternation ')' | Regex | Literal | NonTerminal ) Quant?

Quant ::= [?+*] 
        | '{,' num '}' 
        | '{' num ',}' 
        | '{' num ',' num '}'
        
num ::= [0-9]+
Probability ::= [0-9A-Fa-fxXpP.-]+
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

## Technical Notes

### Using the Grammar Cache
Tribble can cache grammars in a binary format such that they can be simply
loaded from disk instead of being parsed and processed every time.
To create a cached version of a grammar use 
```bash
java -jar tribble.jar cache-grammar --grammar-cache-dir=<path-to-cache> --grammar-file=<path-to-grammar>
```
The value of `--grammar-cache-dir` defaults to `./grammar-cache`.

The next time tribble has to parse a grammar file, it will first check
in the `--grammar-cache-dir` if a cache has been created for this particular
grammar file and load it from there. 

To suppress this behavior you can pass the `--ignore-grammar-cache` flag.

### Making the Grammars Generative
To make the grammars produce more meaningful inputs it is worth considering
- explicitly adding whitespace tokens into the productions
- adding a small vocabulary to constrain the number of produced identifiers
