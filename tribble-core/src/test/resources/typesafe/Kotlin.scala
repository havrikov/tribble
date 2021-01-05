import de.cispa.se.tribble.dsl._

// Kotlin grammar from https://kotlinlang.org/docs/reference/grammar.html

Grammar(

  /* Syntax */

  'start := 'kotlinFile | 'script,

  'kotlinFile := 'preamble ~ 'SEMI ~ 'topLevelObject.rep,

  'script := 'preamble ~ 'SEMI ~ ('expression ~ 'SEMI).rep,
  'preamble := 'fileAnnotations.? ~ " " ~ 'packageHeader.? ~ " " ~ 'import.rep,
  'fileAnnotations := 'fileAnnotation.rep,
  'fileAnnotation := "@" ~ "file" ~ ":" ~ ("[" ~ 'unescapedAnnotation.rep(1) ~ "]" | 'unescapedAnnotation),
  'packageHeader := 'modifiers ~ "package " ~ 'SimpleName ~ ("" ~ 'SimpleName).rep ~ 'SEMI,

  'import := "import " ~ 'SimpleName ~ ("" ~ 'SimpleName).rep ~ (".*" | "as" ~ 'SimpleName).? ~ 'SEMI,

  'topLevelObject := 'class | 'object | 'function | 'property | 'typeAlias,
  'typeAlias := 'modifiers ~ "typealias " ~ 'SimpleName ~ 'typeParameters.? ~ "=" ~ 'type,

  /* Classes */

  'class := 'modifiers ~ ("class " | "interface ") ~ 'SimpleName ~
    'typeParameters.? ~
    'primaryConstructor.? ~
    (":" ~ 'annotations ~ 'delegationSpecifier ~ ("," ~ 'delegationSpecifier).rep).? ~
    'typeConstraints ~
    ('classBody.? | 'enumClassBody),

  'primaryConstructor := ('modifiers ~ "constructor").? ~ ("(" ~ 'functionParameter ~ ("," ~ 'functionParameter).rep ~ ")"),
  'classBody := ("{" ~ 'members ~ "}").?,
  'members := 'memberDeclaration.rep,
  'delegationSpecifier := 'constructorInvocation
    | 'userType
    | 'explicitDelegation
  ,
  'explicitDelegation := 'userType ~ "by" ~ 'expression,
  'typeParameters := "<" ~ 'typeParameter ~ ("," ~ 'typeParameter).rep ~ ">",
  'typeParameter := 'modifiers ~ 'SimpleName ~ (":" ~ 'userType).?,

  'typeConstraints := ("where" ~ 'typeConstraint ~ ("," ~ 'typeConstraint).rep).?,
  'typeConstraint := 'annotations ~ 'SimpleName ~ ":" ~ 'type,

  /* Class Members */

  'memberDeclaration := 'companionObject
    | 'object
    | 'function
    | 'property
    | 'class
    | 'typeAlias
    | 'anonymousInitializer
    | 'secondaryConstructor
  ,
  'anonymousInitializer := "init" ~ 'block
  ,
  'companionObject := 'modifiers ~ "companion" ~ "object" ~ 'SimpleName.? ~ (":" ~ 'delegationSpecifier ~ ("," ~ 'delegationSpecifier).rep).? ~ 'classBody.?
  ,
  'valueParameters := "(" ~ ('functionParameter ~ ("," ~ 'functionParameter).rep).? ~ ")"
  ,
  'functionParameter := 'modifiers ~ ("val" | "var").? ~ 'parameter ~ ("=" ~ 'expression).?
  ,
  'block := "{" ~ 'statements ~ "}"
  ,
  'function := 'modifiers ~ "fun" ~
    'typeParameters.? ~
    ('type ~ "").? ~
    'SimpleName ~
    'typeParameters.? ~ 'valueParameters ~ (":" ~ 'type).? ~
    'typeConstraints ~
    'functionBody.?
  ,
  'functionBody := 'block
    | "=" ~ 'expression
  ,
  'variableDeclarationEntry := 'SimpleName ~ (":" ~ 'type).?
  ,
  'multipleVariableDeclarations := "(" ~ 'variableDeclarationEntry ~ ("," ~ 'variableDeclarationEntry).rep ~ ")"
  ,
  'property := 'modifiers ~ ("val" | "var") ~
    'typeParameters.? ~
    ('type ~ "").? ~
    ('multipleVariableDeclarations | 'variableDeclarationEntry) ~
    'typeConstraints ~
    ("by" | "=" ~ 'expression ~ 'SEMI).? ~
    ('getter.? ~ 'setter.? | 'setter.? ~ 'getter.?) ~ 'SEMI
  ,

  'getter := 'modifiers ~ "get"
    | 'modifiers ~ "get()" ~ (":" ~ 'type).? ~ 'functionBody
  ,
  'setter := 'modifiers ~ "set"
    | 'modifiers ~ "set" ~ "(" ~ 'modifiers ~ ('SimpleName | 'parameter) ~ ")" ~ 'functionBody
  ,
  'parameter := 'SimpleName ~ ":" ~ 'type
  ,
  'object := "object" ~ 'SimpleName ~ 'primaryConstructor.? ~ (":" ~ 'delegationSpecifier ~ ("," ~ 'delegationSpecifier).rep).? ~ 'classBody.?,
  'secondaryConstructor := 'modifiers ~ "constructor" ~ 'valueParameters ~ (":" ~ 'constructorDelegationCall).? ~ 'block
  ,
  'constructorDelegationCall := "this" ~ 'valueArguments
    | "super" ~ 'valueArguments
  ,

  /* Enum Classes */

  'enumClassBody := "{" ~ 'enumEntries ~ (";" ~ 'members).? ~ "}",
  'enumEntries := ('enumEntry ~ ("," ~ 'enumEntry).rep ~ ",".? ~ ";".?).?,
  'enumEntry := 'modifiers ~ 'SimpleName ~ 'valueArguments.? ~ 'classBody.?, // fixed error in original grammar which had arguments instead of valueArguments

  /* Types */


  'type := 'typeModifiers ~ 'typeReference
  ,
  'typeReference := "(" ~ 'typeReference ~ ")"
    | 'functionType
    | 'userType
    | 'nullableType
    | "dynamic"
  ,
  'nullableType := 'typeReference ~ "?"
  ,
  'userType := 'simpleUserType ~ ("" ~ 'simpleUserType).rep
  ,
  'simpleUserType := 'SimpleName ~ ("<" ~ ('optionalProjection ~ 'type | "*") ~ ("," ~ ('optionalProjection ~ 'type | "*")).rep ~ ">").?
  ,
  'optionalProjection := 'varianceAnnotation
  ,
  'functionType := ('type ~ "").? ~ "(" ~ ('parameter ~ ("," ~ 'parameter).rep).? ~ ")" ~ "->" ~ 'type.?
  ,

  /* Control Structures */


  'controlStructureBody := 'block
    | 'blockLevelExpression
  ,
  'if := "if (" ~ 'expression ~ ")" ~ 'controlStructureBody ~ 'SEMI ~ ("else" ~ 'controlStructureBody).?
  ,
  'try := "try" ~ 'block ~ 'catchBlock.rep ~ 'finallyBlock.?
  ,
  'catchBlock := "catch" ~ "(" ~ 'annotations ~ 'SimpleName ~ ":" ~ 'userType ~ ")" ~ 'block
  ,
  'finallyBlock := "finally" ~ 'block
  ,
  'loop := 'for
    | 'while
    | 'doWhile
  ,
  'for := "for (" ~ 'annotations ~ ('multipleVariableDeclarations | 'variableDeclarationEntry) ~ "in" ~ 'expression ~ ")" ~ 'controlStructureBody
  ,
  'while := "while (" ~ 'expression ~ ")" ~ 'controlStructureBody
  ,
  'doWhile := "do" ~ 'controlStructureBody ~ "while (" ~ 'expression ~ ")"
  ,

  /* Expressions */

  /* Rules */


  'expression := 'disjunction ~ ('assignmentOperator ~ 'disjunction).rep
  ,
  'disjunction := 'conjunction ~ ("||" ~ 'conjunction).rep
  ,
  'conjunction := 'equalityComparison ~ ("&&" ~ 'equalityComparison).rep
  ,
  'equalityComparison := 'comparison ~ ('equalityOperation ~ 'comparison).rep
  ,
  'comparison := 'namedInfix ~ ('comparisonOperation ~ 'namedInfix).rep
  ,
  'namedInfix := 'elvisExpression ~ ('inOperation ~ 'elvisExpression).rep
    | 'elvisExpression ~ ('isOperation ~ 'type).?
  ,
  'elvisExpression := 'infixFunctionCall ~ ("?:" ~ 'infixFunctionCall).rep
  ,
  'infixFunctionCall := 'rangeExpression ~ ('SimpleName ~ 'rangeExpression).rep
  ,
  'rangeExpression := 'additiveExpression ~ ("src/test" ~ 'additiveExpression).rep
  ,
  'additiveExpression := 'multiplicativeExpression ~ ('additiveOperation ~ 'multiplicativeExpression).rep
  ,
  'multiplicativeExpression := 'typeRHS ~ ('multiplicativeOperation ~ 'typeRHS).rep
  ,
  'typeRHS := 'prefixUnaryExpression ~ ('typeOperation ~ 'prefixUnaryExpression).rep
  ,
  'prefixUnaryExpression := 'prefixUnaryOperation.rep ~ 'postfixUnaryExpression
  ,
  'postfixUnaryExpression := 'atomicExpression ~ 'postfixUnaryOperation.rep
    | 'callableReference ~ 'postfixUnaryOperation.rep
  ,
  'callableReference := ('userType ~ "?".rep).? ~ "::" ~ 'SimpleName ~ 'typeArguments.?
  ,
  'atomicExpression := "(" ~ 'expression ~ ")"
    | 'literalConstant
    | 'functionLiteral
    | "this" ~ 'labelReference.?
    | "super" ~ ("<" ~ 'type ~ ">").? ~ 'labelReference.?
    | 'if
    | 'when
    | 'try
    | 'objectLiteral
    | 'jump
    | 'loop
    | 'SimpleName
  ,
  'labelReference := "@" ~ 'LabelName
  ,
  'labelDefinition := 'LabelName ~ "@"
  ,
  'literalConstant := "true" | "false"
    | 'stringTemplate
    | 'NoEscapeString
    | 'IntegerLiteral
    | 'HexadecimalLiteral
    | 'CharacterLiteral
    | 'FloatLiteral
    | "null"
  ,
  'stringTemplate := "\"" ~ 'stringTemplateElement.rep ~ "\""
  ,
  'stringTemplateElement := 'RegularStringPart
    | 'ShortTemplateEntryStart ~ ('SimpleName | "this")
    | 'EscapeSequence
    | 'longTemplate
  ,
  'longTemplate := "${" ~ 'expression ~ "}"
  ,
  'declaration := 'function
    | 'property
    | 'class
    | 'typeAlias
    | 'object
  ,
  'statement := 'declaration
    | 'blockLevelExpression
  ,
  'blockLevelExpression := 'annotations ~ "\n".rep(1) ~ 'expression
  ,
  'multiplicativeOperation := "*" | "/" | "%"
  ,
  'additiveOperation := "+" | "-"
  ,
  'inOperation := "in" | "!in"
  ,
  'typeOperation := "as" | "as?" | ":"
  ,
  'isOperation := "is" | "!is"
  ,
  'comparisonOperation := "<" | ">" | ">=" | "<="
  ,
  'equalityOperation := "!=" | "=="
  ,
  'assignmentOperator := "="
    | "+=" | "-=" | "*=" | "/=" | "%="
  ,
  'prefixUnaryOperation := "-" | "+"
    | "++" | "--"
    | "!"
    | 'annotations
    | 'labelDefinition
  ,
  'postfixUnaryOperation := "++" | "--" | "!!"
    | 'callSuffix
    | 'arrayAccess
    | 'memberAccessOperation ~ 'postfixUnaryExpression
  ,
  'callSuffix := 'typeArguments.? ~ 'valueArguments ~ 'annotatedLambda
    | 'typeArguments ~ 'annotatedLambda
  ,
  'annotatedLambda := ("@" ~ 'unescapedAnnotation).rep ~ 'labelDefinition.? ~ 'functionLiteral,
  'memberAccessOperation := "" | "?." | "?"
  ,
  'typeArguments := "<" ~ 'type ~ ("," ~ 'type).rep ~ ">"
  ,
  'valueArguments := "(" ~ ('SimpleName ~ "=").? ~ "*".? ~ 'expression ~ ("," ~ 'expression).rep ~ ")"
  ,
  'jump := "throw" ~ 'expression
    | "return" ~ 'labelReference.? ~ 'expression.?
    | "continue" ~ 'labelReference.?
    | "break" ~ 'labelReference.?
  ,
  'functionLiteral := "{" ~ 'statements ~ "}"
    | "{" ~ 'lambdaParameter ~ ("," ~ 'lambdaParameter).rep ~ "->" ~ 'statements ~ "}"
  ,
  'lambdaParameter := 'variableDeclarationEntry
    | 'multipleVariableDeclarations ~ (":" ~ 'type).?
  ,
  'statements := 'SEMI.rep(1) ~ 'statement ~ ('SEMI.rep(1) ~ 'statement).rep ~ 'SEMI.rep(1)
  ,
  'constructorInvocation := 'userType ~ 'callSuffix
  ,
  'arrayAccess := "[" ~ 'expression ~ ("," ~ 'expression).rep ~ "]"
  ,
  'objectLiteral := "object" ~ (":" ~ 'delegationSpecifier ~ ("," ~ 'delegationSpecifier).rep).? ~ 'classBody
  ,

  /* When-Expression */

  'when := "when " ~ ("(" ~ 'expression ~ ") ").? ~ "{\n" ~ 'whenEntry.rep ~ "}"
  ,
  'whenEntry := 'whenCondition ~ ("," ~ 'whenCondition).rep ~ " ->" ~ 'controlStructureBody ~ 'SEMI
    | "else ->" ~ 'controlStructureBody ~ 'SEMI
  ,
  'whenCondition := 'expression
    | ("in" | "!in") ~ 'expression
    | ("is" | "!is") ~ 'type
  ,

  /* Modifiers */

  'modifiers := ('modifier ~ " " | 'annotations).rep
  ,
  'typeModifiers := ('suspendModifier | 'annotations).rep
  ,
  'modifier := 'classModifier
    | 'accessModifier
    | 'varianceAnnotation
    | 'memberModifier
    | 'parameterModifier
    | 'typeParameterModifier
    | 'functionModifier
    | 'propertyModifier
  ,
  'classModifier := "abstract"
    | "final"
    | "enum"
    | "open"
    | "annotation"
    | "sealed"
    | "data"
  ,
  'memberModifier := "override"
    | "open"
    | "final"
    | "abstract"
    | "lateinit"
  ,
  'accessModifier := "private"
    | "protected"
    | "public"
    | "internal"
  ,
  'varianceAnnotation := "in"
    | "out"
  ,
  'parameterModifier := "noinline"
    | "crossinline"
    | "vararg"
  ,
  'typeParameterModifier := "reified"
  ,
  'functionModifier := "tailrec"
    | "operator"
    | "infix"
    | "inline"
    | "external"
    | 'suspendModifier
  ,
  'propertyModifier := "const"
  ,
  'suspendModifier := "suspend"
  ,

  /* Annotations */

  'annotations := (('annotation | 'annotationList) ~ "\n").rep
  ,
  'annotation := "@" ~ ('annotationUseSiteTarget ~ ":").? ~ 'unescapedAnnotation
  ,
  'annotationList := "@" ~ ('annotationUseSiteTarget ~ ":").? ~ "[" ~ 'unescapedAnnotation.rep(1) ~ "]"
  ,
  'annotationUseSiteTarget := "field"
    | "file"
    | "property"
    | "get"
    | "set"
    | "receiver"
    | "param"
    | "setparam"
    | "delegate"
  ,
  'unescapedAnnotation := 'SimpleName ~ ("" ~ 'SimpleName).rep ~ 'typeArguments.? ~ 'valueArguments.?
  ,

  /* Lexical Structure */

  'Digit := "[0-9]".regex,
  'IntegerLiteral := 'Digit | 'Digit ~ ('Digit | "_").rep ~ 'Digit, // fixed the error in the grammar admitting trailing underscores
  'FloatLiteral := "[0-9]*(\\.[0-9]+)?([eE][+-]?[0-9]+)?[dD]?".regex, // partially stolen from json

  'HexDigit := "[0-9a-fA-F]".regex,
  'HexadecimalLiteral := "0x" ~ ('HexDigit | 'HexDigit ~ ('HexDigit | "_").rep ~ 'HexDigit), // fixed the same trailing underscore error
  'CharacterLiteral := "'\\u" ~ 'HexDigit.rep(4, 4) ~ "'", // xxx might want to rethink this

  'NoEscapeString := "\"\"\"" ~ ".+".regex ~ "\"\"\"", // <"""-quoted string> // todo fix this
  'RegularStringPart := "[^\\n\\r\\\\$\\\"]".regex, // <any character other than backslash, quote, $ or newline>
  'ShortTemplateEntryStart := "$",
  'EscapeSequence := 'UnicodeEscapeSequence | 'RegularEscapeSequence,
  'UnicodeEscapeSequence := "\\u" ~ 'HexDigit.rep(4, 4),
  'RegularEscapeSequence := "\\" ~ "[^\\\n\\\r]".regex, // "\" <any character other than newline>

  'SEMI := "\n",
  'SimpleName := 'javaIdentifier | "`" ~ 'javaIdentifier ~ "`",

  'LabelName := "@" ~ 'SimpleName,

  'javaIdentifier := "[a-zA-Z][0-9a-zA-Z]".regex

)
