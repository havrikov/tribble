import saarland.cispa.se.tribble.dsl._

// Translated from https://github.com/antlr/grammars-v4/blob/master/css3/css3.g4

Grammar(
  'stylesheet
    := 'ws ~ ('charset ~ ('Comment | 'Space | 'Cdo | 'Cdc).rep).rep ~ ('imports ~ ('Comment | 'Space | 'Cdo | 'Cdc).rep).rep ~ ('namespace ~ ('Comment | 'Space | 'Cdo | 'Cdc).rep).rep ~ ('nestedStatement ~ ('Comment | 'Space | 'Cdo | 'Cdc).rep).rep
  ,

  'charset
    := 'Charset ~ 'ws ~ 'String ~ 'ws ~ ";" ~ 'ws //# goodCharset
    | 'Charset ~ 'ws ~ 'String ~ 'ws //# badCharset
  ,

  'imports
    := 'Import ~ 'ws ~ ('String | 'Uri) ~ 'ws ~ 'mediaQueryList ~ ";" ~ 'ws //# goodImport
    | 'Import ~ 'ws ~ ('String | 'Uri) ~ 'ws ~ ";" ~ 'ws //# goodImport
    | 'Import ~ 'ws ~ ('String | 'Uri) ~ 'ws ~ 'mediaQueryList //# badImport
    | 'Import ~ 'ws ~ ('String | 'Uri) ~ 'ws //# badImport
  ,

  // Namespaces
  // https://www.w3.org/TR/css-namespaces-3/
  'namespace
    := 'Namespace ~ 'ws ~ ('namespacePrefix ~ 'ws).? ~ ('String | 'Uri) ~ 'ws ~ ";" ~ 'ws //# 'goodNamespace
    | 'Namespace ~ 'ws ~ ('namespacePrefix ~ 'ws).? ~ ('String | 'Uri) ~ 'ws //# 'badNamespace
  ,

  'namespacePrefix
    := 'ident
  ,

  // Media queries
  // https://www.w3.org/TR/css3-mediaqueries/
  'media
    := 'Media ~ 'ws ~ 'mediaQueryList ~ 'groupRuleBody ~ 'ws
  ,

  'mediaQueryList
    := ('mediaQuery ~ ('Comma ~ 'ws ~ 'mediaQuery).rep).? ~ 'ws
  ,

  'mediaQuery
    := ('MediaOnly | 'Not).? ~ 'ws ~ 'mediaType ~ 'ws ~ ('And ~ 'ws ~ 'mediaExpression).rep
    | 'mediaExpression ~ ('And ~ 'ws ~ 'mediaExpression).rep
  ,

  'mediaType
    := 'ident
  ,

  'mediaExpression
    := "(" ~ 'ws ~ 'mediaFeature ~ (":" ~ 'ws ~ 'expr).? ~ ")" ~ 'ws // Grammar allows for "and(", which gets tokenized as Function. In practice, people always insert space before "(" to have it work on Chrome.
  ,

  'mediaFeature
    := 'ident ~ 'ws
  ,

  // Page
  'page
    := 'Page ~ 'ws ~ 'pseudoPage.? ~ "{" ~ 'ws ~ 'declaration.? ~ (";" ~ 'ws ~ 'declaration.?).rep ~ "}" ~ 'ws
  ,

  'pseudoPage
    := ":" ~ 'ident ~ 'ws
  ,

  // Selectors
  // https://www.w3.org/TR/css3-selectors/
  'selectorGroup
    := 'selector ~ ('Comma ~ 'ws ~ 'selector).rep
  ,

  'selector
    := 'simpleSelectorSequence ~ 'ws ~ ('combinator ~ 'simpleSelectorSequence ~ 'ws).rep
  ,

  'combinator
    := 'Plus ~ 'ws
    | 'Greater ~ 'ws
    | 'Tilde ~ 'ws
    | 'Space ~ 'ws
  ,

  'simpleSelectorSequence
    := ('typeSelector | 'universal) ~ ('Hash | 'className | 'attrib | 'pseudo | 'negation).rep
    | ('Hash | 'className | 'attrib | 'pseudo | 'negation).rep(1)
  ,

  'typeSelector
    := 'typeNamespacePrefix.? ~ 'elementName
  ,

  'typeNamespacePrefix
    := ('ident | "*").? ~ "|"
  ,

  'elementName
    := 'ident
  ,

  'universal
    := 'typeNamespacePrefix.? ~ "*"
  ,

  'className
    := "." ~ 'ident
  ,

  'attrib
    := "[" ~ 'ws ~ 'typeNamespacePrefix.? ~ 'ident ~ 'ws ~ (('PrefixMatch | 'SuffixMatch | 'SubstringMatch | "=" | 'Includes | 'DashMatch) ~ 'ws ~ ('ident | 'String) ~ 'ws).? ~ "]"
  ,

  'pseudo
    /* "::" starts a pseudo-element, ":" a pseudo-class */
    /* Exceptions: :first-line, :first-letter, :before And :after. */
    /* Note that pseudo-elements are restricted to one per selector And */
    /* occur MediaOnly in the last simple_selector_sequence. */
    := ":" ~ ":".? ~ ('ident | 'functionalPseudo)
  ,

  'functionalPseudo
    := 'Function ~ 'ws ~ 'expression ~ ")"
  ,

  'expression
    /* In CSS3, the expressions are identifiers, strings, */
    /* or of the form "an+b" */
    := (('Plus | 'Minus | 'Dimension | 'UnknownDimension | 'Number | 'String | 'ident) ~ 'ws).rep(1)
  ,

  'negation
    := 'PseudoNot ~ 'ws ~ 'negationArg ~ 'ws ~ ")"
  ,

  'negationArg
    := 'typeSelector
    | 'universal
    | 'Hash
    | 'className
    | 'attrib
    | 'pseudo
  ,

  // Rules
  'operator
    := "/" ~ 'ws //# goodOperator
    | 'Comma ~ 'ws //# goodOperator
    | 'Space ~ 'ws //# goodOperator
    | "=" ~ 'ws //# badOperator  // IE filter and DXImageTransform function
  ,

  'property
    := 'ident ~ 'ws //# goodProperty
    | 'Variable ~ 'ws //# goodProperty
    | "*" ~ 'ident //# badProperty  // IE hacks
    | "_" ~ 'ident //# badProperty  // IE hacks
  ,

  'ruleset
    := 'selectorGroup ~ "{" ~ 'ws ~ 'declarationList.? ~ "}" ~ 'ws //# knownRuleset
    | 'any.rep ~ "{" ~ 'ws ~ 'declarationList.? ~ "}" ~ 'ws //# unknownRuleset
  ,

  'declarationList
    := (";" ~ 'ws).rep ~ 'declaration ~ 'ws ~ (";" ~ 'ws ~ 'declaration.?).rep
  ,

  'declaration
    := 'property ~ ":" ~ 'ws ~ 'expr ~ 'prio.? //# knownDeclaration
    | 'property ~ ":" ~ 'ws ~ 'value //# unknownDeclaration
  ,

  'prio
    := 'Important ~ 'ws
  ,

  'value
    := ('any | 'block | 'atKeyword ~ 'ws).rep(1)
  ,

  'expr
    := 'term ~ ('operator.? ~ 'term).rep
  ,

  'term
    := 'number ~ 'ws //# knownTerm
    | 'percentage ~ 'ws //# knownTerm
    | 'dimension ~ 'ws //# knownTerm
    | 'String ~ 'ws //# knownTerm
    | 'UnicodeRange ~ 'ws //# knownTerm
    | 'ident ~ 'ws //# knownTerm
    | 'var //# knownTerm
    | 'Uri ~ 'ws //# knownTerm
    | 'hexcolor //# knownTerm
    | 'calc //# knownTerm
    | 'function //# knownTerm
    | 'unknownDimension ~ 'ws //# unknownTerm
    | 'dxImageTransform //# badTerm
  ,

  'function
    := 'Function ~ 'ws ~ 'expr ~ ")" ~ 'ws
  ,

  'dxImageTransform
    := 'DxImageTransform ~ 'ws ~ 'expr ~ ")" ~ 'ws // IE DXImageTransform function
  ,

  'hexcolor
    := 'Hash ~ 'ws
  ,

  'number
    := ('Plus | 'Minus).? ~ 'Number
  ,

  'percentage
    := ('Plus | 'Minus).? ~ 'Percentage
  ,

  'dimension
    := ('Plus | 'Minus).? ~ 'Dimension
  ,

  'unknownDimension
    := ('Plus | 'Minus).? ~ 'UnknownDimension
  ,

  // Error handling
  'any
    := 'ident ~ 'ws
    | 'number ~ 'ws
    | 'percentage ~ 'ws
    | 'dimension ~ 'ws
    | 'unknownDimension ~ 'ws
    | 'String ~ 'ws
    //| Delim ws    // Not implemented yet
    | 'Uri ~ 'ws
    | 'Hash ~ 'ws
    | 'UnicodeRange ~ 'ws
    | 'Includes ~ 'ws
    | 'DashMatch ~ 'ws
    | ":" ~ 'ws
    | 'Function ~ 'ws ~ ('any | 'unused).rep ~ ")" ~ 'ws
    | "(" ~ 'ws ~ ('any | 'unused).rep ~ ")" ~ 'ws
    | "[" ~ 'ws ~ ('any | 'unused).rep ~ "]" ~ 'ws
  ,

  'atRule
    := 'atKeyword ~ 'ws ~ 'any.rep ~ ('block | ";" ~ 'ws) //# unknownAtRule
  ,

  'atKeyword
    := "@" ~ 'ident
  ,

  'unused
    := 'block
    | 'atKeyword ~ 'ws
    | ";" ~ 'ws
    | 'Cdo ~ 'ws
    | 'Cdc ~ 'ws
  ,

  'block
    := "{" ~ 'ws ~ ('declarationList | 'nestedStatement | 'any | 'block | 'atKeyword ~ 'ws | ";" ~ 'ws).rep ~ "}" ~ 'ws
  ,

  // Conditional
  // https://www.w3.org/TR/css3-conditional/
  'nestedStatement
    := 'ruleset
    | 'media
    | 'page
    | 'fontFaceRule
    | 'keyframesRule
    | 'supportsRule
    | 'viewport
    | 'counterStyle
    | 'fontFeatureValuesRule
    | 'atRule
  ,

  'groupRuleBody
    := "{" ~ 'ws ~ 'nestedStatement.rep ~ "}" ~ 'ws
  ,

  'supportsRule
    := 'Supports ~ 'ws ~ 'supportsCondition ~ 'ws ~ 'groupRuleBody
  ,

  'supportsCondition
    := 'supportsNegation
    | 'supportsConjunction
    | 'supportsDisjunction
    | 'supportsConditionInParens
  ,

  'supportsConditionInParens
    := "(" ~ 'ws ~ 'supportsCondition ~ 'ws ~ ")"
    | 'supportsDeclarationCondition
    | 'generalEnclosed
  ,

  'supportsNegation
    := 'Not ~ 'ws ~ 'Space ~ 'ws ~ 'supportsConditionInParens
  ,

  'supportsConjunction
    := 'supportsConditionInParens ~ ('ws ~ 'Space ~ 'ws ~ 'And ~ 'ws ~ 'Space ~ 'ws ~ 'supportsConditionInParens).rep(1)
  ,

  'supportsDisjunction
    := 'supportsConditionInParens ~ ('ws ~ 'Space ~ 'ws ~ 'Or ~ 'ws ~ 'Space ~ 'ws ~ 'supportsConditionInParens).rep(1)
  ,

  'supportsDeclarationCondition
    := "(" ~ 'ws ~ 'declaration ~ ")"
  ,

  'generalEnclosed
    := ('Function | "(") ~ ('any | 'unused).rep ~ ")"
  ,

  // Variable
  // https://www.w3.org/TR/css-variables-1
  'var
    := 'Var ~ 'ws ~ 'Variable ~ 'ws ~ ")" ~ 'ws
  ,

  // Calc
  // https://www.w3.org/TR/css3-values///#calc-syntax
  'calc
    := 'Calc ~ 'ws ~ 'calcSum ~ ")" ~ 'ws
  ,

  'calcSum
    := 'calcProduct ~ ('Space ~ 'ws ~ ('Plus | 'Minus) ~ 'ws ~ 'Space ~ 'ws ~ 'calcProduct).rep
  ,

  'calcProduct
    := 'calcValue ~ ("*" ~ 'ws ~ 'calcValue | "/" ~ 'ws ~ 'number ~ 'ws).rep
  ,

  'calcValue
    := 'number ~ 'ws
    | 'dimension ~ 'ws
    | 'unknownDimension ~ 'ws
    | 'percentage ~ 'ws
    | "(" ~ 'ws ~ 'calcSum ~ ")" ~ 'ws
  ,

  // Font face
  // https://www.w3.org/TR/2013/CR-css-fonts-3-20131003///#font-face-rule
  'fontFaceRule
    := 'FontFace ~ 'ws ~ "{" ~ 'ws ~ 'fontFaceDeclaration.? ~ (";" ~ 'ws ~ 'fontFaceDeclaration.?).rep ~ "}" ~ 'ws
  ,

  'fontFaceDeclaration
    := 'property ~ ":" ~ 'ws ~ 'expr //# knownFontFaceDeclaration
    | 'property ~ ":" ~ 'ws ~ 'value //# unknownFontFaceDeclaration
  ,

  // Animations
  // https://www.w3.org/TR/css3-animations/
  'keyframesRule
    := 'Keyframes ~ 'ws ~ 'Space ~ 'ws ~ 'ident ~ 'ws ~ "{" ~ 'ws ~ 'keyframesBlocks ~ "}" ~ 'ws
  ,

  'keyframesBlocks
    := ('keyframeSelector ~ "{" ~ 'ws ~ 'declarationList.? ~ "}" ~ 'ws).rep
  ,

  'keyframeSelector
    := ('From | 'To | 'Percentage) ~ 'ws ~ ('Comma ~ 'ws ~ ('From | 'To | 'Percentage) ~ 'ws).rep
  ,

  // Viewport
  // https://www.w3.org/TR/css-device-adapt-1/
  'viewport
    := 'Viewport ~ 'ws ~ "{" ~ 'ws ~ 'declarationList.? ~ "}" ~ 'ws
  ,

  // Counter style
  // https://www.w3.org/TR/css-counter-styles-3/
  'counterStyle
    := 'CounterStyle ~ 'ws ~ 'ident ~ 'ws ~ "{" ~ 'ws ~ 'declarationList.? ~ "}" ~ 'ws
  ,

  // Font feature values
  // https://www.w3.org/TR/css-fonts-3/
  'fontFeatureValuesRule
    := 'FontFeatureValues ~ 'ws ~ 'fontFamilyNameList ~ 'ws ~ "{" ~ 'ws ~ 'featureValueBlock.rep ~ "}" ~ 'ws
  ,

  'fontFamilyNameList
    := 'fontFamilyName ~ ('ws ~ 'Comma ~ 'ws ~ 'fontFamilyName).rep
  ,

  'fontFamilyName
    := 'String
    | 'ident ~ ('ws ~ 'ident).rep
  ,

  'featureValueBlock
    := 'featureType ~ 'ws ~ "{" ~ 'ws ~ 'featureValueDefinition.? ~ ('ws ~ ";" ~ 'ws ~ 'featureValueDefinition.?).rep ~ "}" ~ 'ws
  ,

  'featureType
    := 'atKeyword
  ,

  'featureValueDefinition
    := 'ident ~ 'ws ~ ":" ~ 'ws ~ 'number ~ ('ws ~ 'number).rep
  ,

  // The specific words can be identifiers too
  'ident
    := 'Ident
    | 'MediaOnly
    | 'Not
    | 'And
    | 'Or
    | 'From
    | 'To
  ,

  // Comments might be part of CSS hacks, thus pass them to visitor to decide whether to skip
  // Spaces are significant around "+" "-" "(", thus they should not be skipped
  'ws
    := ('Comment | 'Space).rep
  ,

  // Tokens
  'Hex
    := "[0-9a-FA-F]".regex
  ,

  'NewlineOrSpace
    := "\r\n"
    | "\t"
    | "\r"
    | "\n"
    | "\f"
    | " "
  ,

  'Unicode
    := "\\" ~ 'Hex.rep(1,6) ~ 'NewlineOrSpace
  ,

  'Escape
    := 'Unicode
    | "\\" ~ "~[\r\n\f0-9a-fA-F]".regex
  ,

  'Nmstart
    := "[_a-zA-Z]".regex
    | 'Nonascii
    | 'Escape
  ,

  'Nmchar
    := "[_a-zA-Z0-9\\-]".regex
    | 'Nonascii
    | 'Escape
  ,

  // CSS2.2 Grammar defines the following, but I am not sure how to add them to parser for error handling
  // BadString :
  // BadUri :
  // BadComment :
  // BadUri :

  'Comment := "/*" ~ "~\\*".regex.rep ~ "*".rep(1) ~ ("~[\\*\\/]".regex ~ "~\\*".regex.rep ~ "*".rep(1)).rep ~ "/",

  'Name := 'Nmchar.rep(1),

  'Url := ("[!#$%&*-~]".regex | 'Nonascii | 'Escape).rep,

  'Space := (" " | "\t" | "\r" | "\n" | "\f").rep(1),

  'Whitespace := 'Space,

  'Newline
    := "\n"
    | "\r\n"
    | "\r"
    | "\f"
  ,
  'ZeroToFourZeros
    := "0".rep(0, 4)
  ,
  'A
    := "a"
    | "A"
    | "\\" ~ 'ZeroToFourZeros ~ ("41" | "61") ~ 'NewlineOrSpace
  ,
  'B
    := "b"
    | "B"
    | "\\" ~ 'ZeroToFourZeros ~ ("42" | "62") ~ 'NewlineOrSpace
  ,
  'C
    := "c"
    | "C"
    | "\\" ~ 'ZeroToFourZeros ~ ("43" | "63") ~ 'NewlineOrSpace
  ,
  'D
    := "d"
    | "D"
    | "\\" ~ 'ZeroToFourZeros ~ ("44" | "64") ~ 'NewlineOrSpace
  ,
  'E
    := "e"
    | "E"
    | "\\" ~ 'ZeroToFourZeros ~ ("45" | "65") ~ 'NewlineOrSpace
  ,
  'F
    := "f"
    | "F"
    | "\\" ~ 'ZeroToFourZeros ~ ("46" | "66") ~ 'NewlineOrSpace
  ,
  'G
    := "g"
    | "G"
    | "\\" ~ 'ZeroToFourZeros ~ ("47" | "67") ~ 'NewlineOrSpace
    | "\\g"
    | "\\G"
  ,
  'H
    := "h"
    | "H"
    | "\\" ~ 'ZeroToFourZeros ~ ("48" | "68") ~ 'NewlineOrSpace
    | "\\h"
    | "\\H"
  ,
  'I
    := "i"
    | "I"
    | "\\" ~ 'ZeroToFourZeros ~ ("49" | "69") ~ 'NewlineOrSpace
    | "\\i"
    | "\\I"
  ,
  'K
    := "k"
    | "K"
    | "\\" ~ 'ZeroToFourZeros ~ ("4b" | "6b") ~ 'NewlineOrSpace
    | "\\k"
    | "\\K"
  ,
  'L
    := "l"
    | "L"
    | "\\" ~ 'ZeroToFourZeros ~ ("4c" | "6c") ~ 'NewlineOrSpace
    | "\\l"
    | "\\L"
  ,
  'M
    := "m"
    | "M"
    | "\\" ~ 'ZeroToFourZeros ~ ("4d" | "6d") ~ 'NewlineOrSpace
    | "\\m"
    | "\\M"
  ,
  'N
    := "n"
    | "N"
    | "\\" ~ 'ZeroToFourZeros ~ ("4e" | "6e") ~ 'NewlineOrSpace
    | "\\n"
    | "\\N"
  ,
  'O
    := "o"
    | "O"
    | "\\" ~ 'ZeroToFourZeros ~ ("4f" | "6f") ~ 'NewlineOrSpace
    | "\\o"
    | "\\O"
  ,
  'P
    := "p"
    | "P"
    | "\\" ~ 'ZeroToFourZeros ~ ("50" | "70") ~ 'NewlineOrSpace
    | "\\p"
    | "\\P"
  ,
  'Q
    := "q"
    | "Q"
    | "\\" ~ 'ZeroToFourZeros ~ ("51" | "71") ~ 'NewlineOrSpace
    | "\\q"
    | "\\Q"
  ,
  'R
    := "r"
    | "R"
    | "\\" ~ 'ZeroToFourZeros ~ ("52" | "72") ~ 'NewlineOrSpace
    | "\\r"
    | "\\R"
  ,
  'S
    := "s"
    | "S"
    | "\\" ~ 'ZeroToFourZeros ~ ("53" | "73") ~ 'NewlineOrSpace
    | "\\s"
    | "\\S"
  ,
  'T
    := "t"
    | "T"
    | "\\" ~ 'ZeroToFourZeros ~ ("54" | "74") ~ 'NewlineOrSpace
    | "\\t"
    | "\\T"
  ,
  'U
    := "u"
    | "U"
    | "\\" ~ 'ZeroToFourZeros ~ ("55" | "75") ~ 'NewlineOrSpace
    | "\\u"
    | "\\U"
  ,
  'V
    := "v"
    | "V"
    | "\\" ~ 'ZeroToFourZeros ~ ("56" | "76") ~ 'NewlineOrSpace
    | "\\v"
    | "\\V"
  ,
  'W
    := "w"
    | "W"
    | "\\" ~ 'ZeroToFourZeros ~ ("57" | "77") ~ 'NewlineOrSpace
    | "\\w"
    | "\\W"
  ,
  'X
    := "x"
    | "X"
    | "\\" ~ 'ZeroToFourZeros ~ ("58" | "78") ~ 'NewlineOrSpace
    | "\\x"
    | "\\X"
  ,
  'Y
    := "y"
    | "Y"
    | "\\" ~ 'ZeroToFourZeros ~ ("59" | "79") ~ 'NewlineOrSpace
    | "\\y"
    | "\\Y"
  ,
  'Z
    := "z"
    | "Z"
    | "\\" ~ 'ZeroToFourZeros ~ ("5a" | "7a") ~ 'NewlineOrSpace
    | "\\z"
    | "\\Z"
  ,
  'DashChar
    := "-"
    | "\\" ~ 'ZeroToFourZeros ~ "2d" ~ 'NewlineOrSpace
  ,
  'Cdo
    := "<!--"
  ,
  'Cdc
    := "-->"
  ,
  'Includes
    := "~="
  ,
  'DashMatch
    := "|="
  ,
  'Hash
    := "//#" ~ 'Name
  ,
  'Import
    := "@" ~ 'I ~ 'M ~ 'P ~ 'O ~ 'R ~ 'T
  ,
  'Page
    := "@" ~ 'P ~ 'A ~ 'G ~ 'E
  ,
  'Media
    := "@" ~ 'M ~ 'E ~ 'D ~ 'I ~ 'A
  ,
  'Namespace
    := "@" ~ 'N ~ 'A ~ 'M ~ 'E ~ 'S ~ 'P ~ 'A ~ 'C ~ 'E
  ,
  'Charset
    := "@charset "
  ,
  'Important
    := "!" ~ ('Space | 'Comment).rep ~ 'I ~ 'M ~ 'P ~ 'O ~ 'R ~ 'T ~ 'A ~ 'N ~ 'T
  ,
  'FontRelative
    := 'Number ~ 'E ~ 'M
    | 'Number ~ 'E ~ 'X
    | 'Number ~ 'C ~ 'H
    | 'Number ~ 'R ~ 'E ~ 'M
  ,
  // https://www.w3.org/TR/css3-values///#viewport-relative-lengths
  'ViewportRelative
    := 'Number ~ 'V ~ 'W
    | 'Number ~ 'V ~ 'H
    | 'Number ~ 'V ~ 'M ~ 'I ~ 'N
    | 'Number ~ 'V ~ 'M ~ 'A ~ 'X
  ,
  'AbsLength
    := 'Number ~ 'P ~ 'X
    | 'Number ~ 'C ~ 'M
    | 'Number ~ 'M ~ 'M
    | 'Number ~ 'I ~ 'N
    | 'Number ~ 'P ~ 'T
    | 'Number ~ 'P ~ 'C
    | 'Number ~ 'Q
  ,
  'Angle
    := 'Number ~ 'D ~ 'E ~ 'G
    | 'Number ~ 'R ~ 'A ~ 'D
    | 'Number ~ 'G ~ 'R ~ 'A ~ 'D
    | 'Number ~ 'T ~ 'U ~ 'R ~ 'N
  ,
  'Time
    := 'Number ~ 'M ~ 'S
    | 'Number ~ 'S
  ,
  'Freq
    := 'Number ~ 'H ~ 'Z
    | 'Number ~ 'K ~ 'H ~ 'Z
  ,
  'Percentage
    := 'Number ~ "%"
  ,
  'Uri
    := 'U ~ 'R ~ 'L ~ "(" ~ 'Whitespace ~ 'String ~ 'Whitespace ~ ")"
    | 'U ~ 'R ~ 'L ~ "(" ~ 'Whitespace ~ 'Url ~ 'Whitespace ~ ")"
  ,
  'UnicodeRange := ("u" | "U") ~ "+" ~
    ("?".rep(1,6)
    |'Hex ~ "?".rep(0,5)
    |'Hex.rep(2,2) ~ "?".rep(0,4)
    |'Hex.rep(3,3) ~ "?".rep(0,3)
    |'Hex.rep(4,4) ~ "?".rep(0,2)
    |'Hex.rep(5,5) ~ "?".?)
  ,
  // https://www.w3.org/TR/css3-mediaqueries/
  'MediaOnly
    := 'O ~ 'N ~ 'L ~ 'Y
  ,
  'Not
    := 'N ~ 'O ~ 'T
  ,
  'And
    := 'A ~ 'N ~ 'D
  ,
  'Resolution
    := 'Number ~ 'D ~ 'P ~ 'I
    | 'Number ~ 'D ~ 'P ~ 'C ~ 'M
    | 'Number ~ 'D ~ 'P ~ 'P ~ 'X
  ,
  'Length
    := 'AbsLength
    | 'FontRelative
    | 'ViewportRelative
  ,
  'Dimension
    := 'Length
    | 'Time
    | 'Freq
    | 'Resolution
    | 'Angle
  ,
  'UnknownDimension
    := 'Number ~ 'Ident
  ,
  // https://www.w3.org/TR/css3-selectors/
  'Nonascii := "~[\u0000-\u007f]".regex,

  'Plus
    := "+"
  ,
  'Minus
    := "-"
  ,
  'Greater
    := ">"
  ,
  'Comma
    := ","
  ,
  'Tilde
    := "~"
  ,
  'PseudoNot
    := ":" ~ 'N ~ 'O ~ 'T ~ "("
  ,
  'Number
    := "[0-9]+".regex
    | "[0-9]*".regex ~ "." ~ "[0-9]+".regex
  ,
  'String
    := "\"" ~ ("~[\n\r\f\"]".regex | "\\" ~ 'Newline | 'Nonascii | 'Escape).rep ~ "\""
    | "'" ~ ("~[\n\r\f']".regex | "\\" ~ 'Newline | 'Nonascii | 'Escape).rep ~ "'"
  ,
  'PrefixMatch
    := "^="
  ,
  'SuffixMatch
    := "$="
  ,
  'SubstringMatch
    := "*="
  ,
  // https://www.w3.org/TR/css-fonts-3///#font-face-rule
  'FontFace
    := "@" ~ 'F ~ 'O ~ 'N ~ 'T ~ 'DashChar ~ 'F ~ 'A ~ 'C ~ 'E
  ,
  // https://www.w3.org/TR/css3-conditional/
  'Supports
    := "@" ~ 'S ~ 'U ~ 'P ~ 'P ~ 'O ~ 'R ~ 'T ~ 'S
  ,
  'Or
    := 'O ~ 'R
  ,
  // https://www.w3.org/TR/css3-animations/
  'VendorPrefix
    := "-" ~ 'M ~ 'O ~ 'Z ~ "-"
    | "-" ~ 'W ~ 'E ~ 'B ~ 'K ~ 'I ~ 'T ~ "-"
    | "-" ~ 'O ~ "-"
  ,
  'Keyframes
    := "@" ~ 'VendorPrefix.? ~ 'K ~ 'E ~ 'Y ~ 'F ~ 'R ~ 'A ~ 'M ~ 'E ~ 'S
  ,
  'From
    := 'F ~ 'R ~ 'O ~ 'M
  ,
  'To
    := 'T ~ 'O
  ,
  // https://www.w3.org/TR/css3-values///#calc-syntax
  'Calc
    := "calc("
  ,
  // https://www.w3.org/TR/css-device-adapt-1/
  'Viewport
    := "@" ~ 'V ~ 'I ~ 'E ~ 'W ~ 'P ~ 'O ~ 'R ~ 'T
  ,
  // https://www.w3.org/TR/css-counter-styles-3/
  'CounterStyle
    := "@" ~ 'C ~ 'O ~ 'U ~ 'N ~ 'T ~ 'E ~ 'R ~ 'DashChar ~ 'S ~ 'T ~ 'Y ~ 'L ~ 'E
  ,
  // https://www.w3.org/TR/css-fonts-3/
  'FontFeatureValues
    := "@" ~ 'F ~ 'O ~ 'N ~ 'T ~ 'DashChar ~ 'F ~ 'E ~ 'A ~ 'T ~ 'U ~ 'R ~ 'E ~ 'DashChar ~ 'V ~ 'A ~ 'L ~ 'U ~ 'E ~ 'S
  ,
  // https://msdn.microsoft.com/en-us/library/ms532847.aspx
  'DxImageTransform
    := "progid:DXImageTransform.Microsoft." ~ 'Function
  ,
  // Variables
  // https://www.w3.org/TR/css-variables-1
  'Variable
    := "--" ~ 'Nmstart ~ 'Nmchar.rep
  ,
  'Var
    := "var("
  ,
  // Give Ident least priority so that more specific rules matches first
  'Ident
    := "-".? ~ 'Nmstart ~ 'Nmchar.rep
  ,
  'Function
    := 'Ident ~ "("
)
