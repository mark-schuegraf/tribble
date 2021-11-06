import de.cispa.se.tribble.dsl._

/**
 * In this grammar, I follow the convention that, whenever possible, there should not be white space at the end of a production.
 * The reason is that
 * (a) if there are white spaces both at the end of and behind a reference to a production, there is ambiguity
 * (b) there are some cases which require a white space, and I can not do this if I can not have white spaces behind a reference.
 * Therefore, almost everything in the grammar should be followed by 'LTWS.rep (whitespaces including line breaks) or
 * 'WhiteSpace.? (optional white spaces, no line break).
 *
 * If you see things of the form
 *
 * 'LTWS.rep ~ 'X ~ 'LTWS.rep
 *
 * and 'X is nullable (can derive the empty string), there is an ambiguity, because if 'X is empty,
 * any line feed/white space character can belong to either 'LTWS.rep. Try to make sure that 'X cannot derive the empty word,
 * and do
 *
 * ('LTWS.rep ~ 'X).? ~ 'LTWS.rep
 *
 * Derived from https://raw.githubusercontent.com/tc39/ecma262/c75497982a8c07e83a13cc5d7d80cdaaa8a337bd/spec.html
 */

Grammar(
  'START := 'SurroundingWhitespace ~ 'Module ~ 'SurroundingWhitespace,
  'SurroundingWhitespace := "[ \t\r\n]*".regex,
  'ClassElement := (";"  | 'MethodDefinition  | "static" ~ 'WhiteSpace ~ 'MethodDefinition ),
  'ObjectLiteral := ("{" ~ 'LTWS.rep ~ "}"  | "{" ~ 'LTWS.rep ~ 'PropertyDefinitionList ~ 'LTWS.rep ~ "," ~ 'LTWS.rep ~ "}"  | "{" ~ 'LTWS.rep ~ 'PropertyDefinitionList ~ 'LTWS.rep ~ "}" ),
  'UnaryExpression := ('PostfixExpression
    | "!" ~ 'UnaryExpression
    | "+" ~ 'UnaryExpression
    | "++" ~ 'UnaryExpression
    | "-" ~ 'UnaryExpression
    | "--" ~ 'UnaryExpression
    | "delete" ~ 'WhiteSpace ~ 'UnaryExpression
    | "typeof" ~ 'WhiteSpace ~ 'UnaryExpression
    | "void" ~ 'WhiteSpace ~ 'UnaryExpression
    | "~" ~ 'UnaryExpression ),
  'Term := ('Assertion  | 'Atom  | 'Atom ~ 'Quantifier ),
  'ArrowFormalParameters := ("(" ~ ('WhiteSpace.? ~ 'StrictFormalParameters).? ~ 'WhiteSpace.? ~ ")"),
  'SignedInteger := ('DecimalDigits  | "+" ~ 'DecimalDigits  | "-" ~ 'DecimalDigits ),
  'DoubleStringCharacter := ('LineContinuation  | "\\" ~ 'EscapeSequence  | "[^\\\"|\"\\\\\"|\r|\n]".regex ),
  'PatternCharacter := "[^$\\.^*+?)(\\][}{|\\\\]".regex,
  'Atom := ("."  | 'CharacterClass  | 'PatternCharacter  | "(" ~ "?" ~ ":" ~ 'Disjunction ~ ")"  | "(" ~ 'Disjunction ~ ")"  | "\\" ~ 'AtomEscape ),
  'NonemptyClassRangesNoDash := ('ClassAtom  | 'ClassAtomNoDash ~ "-" ~ 'ClassAtom ~ 'ClassRanges  | 'ClassAtomNoDash ~ 'NonemptyClassRangesNoDash ),
  'NonEscapeCharacter := "[^'\"\\\\bfnrtvxu\r\n]".regex,
  'DefaultClause := ("default" ~ 'WhiteSpace.? ~ ":" ~ 'LTWS.rep ~ 'StatementList | "default" ~ 'WhiteSpace.? ~ ":"),
  'DoubleStringCharacters := 'DoubleStringCharacter.rep(1),
  'HexDigits := 'HexDigit.rep(1),
  'PropertyName := ('ComputedPropertyName  | 'LiteralPropertyName ),
  'HT := "\t",
  'EqualityExpression := ('RelationalExpression
    | 'EqualityExpression ~ 'WhiteSpace.? ~ "!="  ~ 'LTWS.rep ~ 'RelationalExpression
    | 'EqualityExpression ~ 'WhiteSpace.? ~ "!==" ~ 'LTWS.rep ~ 'RelationalExpression
    | 'EqualityExpression ~ 'WhiteSpace.? ~ "==" ~ 'LTWS.rep ~ 'RelationalExpression
    | 'EqualityExpression ~ 'WhiteSpace.? ~ "===" ~ 'LTWS.rep ~ 'RelationalExpression ),
  'MultiplicativeExpression := ('UnaryExpression
    | 'MultiplicativeExpression ~ 'WhiteSpace.? ~ 'MultiplicativeOperator ~ 'LTWS.rep ~ 'UnaryExpression ),
  'ShiftExpression := ('AdditiveExpression
    | 'ShiftExpression ~ 'WhiteSpace.? ~ "<<"~ 'LTWS.rep ~ 'AdditiveExpression
    | 'ShiftExpression ~ 'WhiteSpace.? ~ ">>"~ 'LTWS.rep ~ 'AdditiveExpression
    | 'ShiftExpression ~ 'WhiteSpace.? ~ ">>>" ~ 'LTWS.rep ~ 'AdditiveExpression ),
  'LineTerminatorSequence := 'LF,
  'DecimalDigits := ('DecimalDigit  | 'DecimalDigits ~ 'DecimalDigit ),
  'ExportClause := ("{" ~ "}"  | "{" ~ 'ExportsList ~ "," ~ "}"  | "{" ~ 'ExportsList ~ "}" ),
  'AtomEscape := ('CharacterClassEscape  | 'CharacterEscape  | 'DecimalEscape ),
  'FormalParameters := ('FunctionRestParameter | 'FormalParameterList | 'FormalParameterList ~ "," | 'FormalParameterList ~ "," ~ 'FunctionRestParameter ),
  'Pattern := 'Disjunction,
  'CallExpression := ('SuperCall
    | 'CallExpression ~ "." ~ 'IdentifierName
    | 'CallExpression ~ "[" ~ 'WhiteSpace.? ~ 'Expression ~ 'WhiteSpace.? ~ "]"
    | 'CallExpression ~ 'WhiteSpace.? ~ 'Arguments
    | 'CallExpression ~ 'TemplateLiteral
    | 'MemberExpression ~ 'WhiteSpace.? ~ 'Arguments ),
  'IterationStatement := ("do" ~ 'WhiteSpace ~ 'Statement ~ 'LTWS.rep ~ "while" ~ 'WhiteSpace ~ "(" ~ 'Expression ~ ")"
    | "for" ~ 'WhiteSpace.? ~ "(" ~ 'WhiteSpace.? ~ "var" ~ 'WhiteSpace ~ 'ForBinding ~ 'WhiteSpace.? ~ "in" ~ 'WhiteSpace ~ 'Expression ~ ")" ~ 'WhiteSpace.? ~ 'Statement
    | "for" ~ 'WhiteSpace.? ~ "(" ~ 'WhiteSpace.? ~ "var" ~ 'WhiteSpace ~ 'ForBinding ~ 'WhiteSpace.? ~ "of" ~ 'WhiteSpace ~ 'AssignmentExpression ~ ")" ~ 'WhiteSpace.? ~ 'Statement
    | "for" ~ 'WhiteSpace.? ~ "(" ~ 'WhiteSpace.? ~ "var" ~ 'WhiteSpace ~ 'VariableDeclarationList ~ 'WhiteSpace.? ~ ";" ~ 'WhiteSpace.? ~ 'Expression.? ~ ";" ~ 'WhiteSpace.? ~ 'Expression.? ~ ")" ~ 'WhiteSpace.? ~ 'Statement
    | "for" ~ 'WhiteSpace.? ~ "(" ~ 'WhiteSpace.? ~ 'ForDeclaration ~ 'WhiteSpace ~ "in" ~ 'WhiteSpace ~ 'Expression ~ ")" ~ 'WhiteSpace.? ~ 'Statement
    | "for" ~ 'WhiteSpace.? ~ "(" ~ 'WhiteSpace.? ~ 'ForDeclaration ~ 'WhiteSpace ~ "of" ~ 'WhiteSpace ~ 'AssignmentExpression ~ ")" ~ 'WhiteSpace.? ~ 'Statement
    | "for" ~ 'WhiteSpace.? ~ "(" ~ 'WhiteSpace.? ~ 'LeftHandSideExpression ~ 'WhiteSpace ~ "in" ~ 'WhiteSpace ~ 'Expression ~ ")" ~ 'WhiteSpace.? ~ 'Statement
    | "for" ~ 'WhiteSpace.? ~ "(" ~ 'WhiteSpace.? ~ 'LeftHandSideExpression ~ 'WhiteSpace ~ "of" ~ 'WhiteSpace ~ 'AssignmentExpression ~ ")" ~ 'WhiteSpace.? ~ 'Statement
    | "for" ~ 'WhiteSpace.? ~ "(" ~ 'WhiteSpace.? ~ 'LexicalDeclaration ~ 'WhiteSpace.? ~ ";" ~ 'WhiteSpace.? ~ 'Expression.? ~ 'WhiteSpace.? ~ ";" ~ 'WhiteSpace.? ~ 'Expression.? ~ ")" ~ 'WhiteSpace.? ~ 'Statement
    | "for" ~ 'WhiteSpace.? ~ "(" ~ 'WhiteSpace.? ~ 'Expression.? ~ ";" ~ 'WhiteSpace.? ~ 'Expression.? ~ ";" ~ 'WhiteSpace.? ~ 'Expression.? ~ ")" ~ 'WhiteSpace.? ~ 'Statement
    | "while" ~ 'WhiteSpace.? ~ "(" ~ 'WhiteSpace.? ~ 'Expression ~ ")" ~ 'WhiteSpace.? ~ 'Statement ),
  'Element := ('Elision.? ~ 'AssignmentExpression  | 'Elision.? ~ 'SpreadElement ),
  'CaseClause := ("case" ~ 'WhiteSpace ~ 'Expression ~ ":" ~ 'LTWS.rep ~ 'StatementList | "case" ~ 'WhiteSpace ~ 'Expression ~ ":"),
  'StatementList := 'LongStatementList ~ ('WhiteSpace.? ~ ";").?,
  'LongStatementList := ('LongStatementList ~ 'WhiteSpace.? ~ (";" ~ 'LTWS.rep | 'LineTerminator ~ 'LTWS.rep) ~ 'StatementListItem) | 'StatementListItem,
  'ModuleItemList := ('ModuleItem | 'ModuleItemList ~ 'LineTerminator ~ 'LTWS.rep ~ 'ModuleItem ),
  'OctalDigits := ('OctalDigit  | 'OctalDigits ~ 'OctalDigit ),
  'ArrayLiteral := ("[" ~ 'ElementList ~ "," ~ 'Elision.? ~ "]"  | "[" ~ 'ElementList ~ "]"  | "[" ~ 'Elision.? ~ "]" ),
  'RegExpUnicodeEscapeSequence := ("u" ~ 'Hex4Digits  | "u" ~ 'LeadSurrogate ~ "\\u" ~ 'TrailSurrogate  | "u" ~ 'LeadSurrogate  | "u" ~ 'NonSurrogate  | "u" ~ 'TrailSurrogate  | "u{" ~ 'HexDigits ~ "}" ),
  'ImportSpecifier := ('ImportedBinding  | 'IdentifierName ~ 'WhiteSpace ~ "as" ~ 'WhiteSpace ~ 'ImportedBinding ),
  'ImportedDefaultBinding := 'ImportedBinding,
  'BitwiseXORExpression := ('BitwiseANDExpression  | 'BitwiseXORExpression ~ "^" ~ 'BitwiseANDExpression ),
  'ComputedPropertyName := ("[" ~ 'WhiteSpace.? ~ 'AssignmentExpression ~ 'WhiteSpace.? ~ "]"),
  'IfStatement := ("if" ~ 'WhiteSpace ~ "(" ~ 'Expression ~ ")" ~ 'WhiteSpace.? ~ 'Statement ~ 'LTWS.rep ~ "else" ~ 'WhiteSpace ~ 'Statement  | "if" ~ 'WhiteSpace ~ "(" ~ 'Expression ~ ")" ~ 'WhiteSpace.? ~ 'Statement ),
  'BlockStatement := 'Block,
  'DecimalIntegerLiteral := ("0"  | 'NonZeroDigit ~ 'DecimalDigits.? ),
  'BreakableStatement := ('IterationStatement  | 'SwitchStatement ),
  'BinaryIntegerLiteral := ("0B" ~ 'BinaryDigits  | "0b" ~ 'BinaryDigits ),
  'IdentifierReference := 'Identifier,
  'LeadSurrogate := 'Hex4Digits,
  'BindingPattern := ('ArrayBindingPattern  | 'ObjectBindingPattern ),
  'PrimaryExpression := ("this" ~ 'WhiteSpace
    | 'ArrayLiteral
    | 'ClassExpression
    | 'FunctionExpression
    | 'GeneratorExpression
    | 'IdentifierReference
    | 'Literal
    | 'ObjectLiteral
    | 'ParenthesizedExpression
    | 'RegularExpressionLiteral
    | 'TemplateLiteral ),
  'NewExpression := ('MemberExpression  | "new" ~ 'WhiteSpace ~ 'NewExpression ),
  'SubstitutionTemplate := ("`" ~ ('TemplateCharacter | 'TemplateInlay).rep ~ "`"),
  'ExportsList := ('ExportSpecifier  | 'ExportsList ~ "," ~ 'ExportSpecifier ),
  'BindingElementList := ('BindingElisionElement  | 'BindingElementList ~ "," ~ 'BindingElisionElement ),
  'ParenthesizedExpression := ("("  ~ 'WhiteSpace.? ~ 'Expression ~ 'WhiteSpace.? ~ ")"),
  'AwaitExpression := ("await" ~ 'WhiteSpace ~ 'AssignmentExpression),
  'MetaProperty := 'NewTarget,
  'WhiteSpace := ('HT  | 'SP).rep(1),
  'ArrayBindingPattern := ("[" ~ 'LTWS.rep ~ 'BindingElementList ~ 'WhiteSpace.? ~ "," ~ ('LTWS.rep ~ 'Elision).? ~ ('LTWS.rep ~ 'BindingRestElement).? ~ 'LTWS.rep ~ "]"
    | "[" ~ 'LTWS.rep ~ 'BindingElementList ~ 'LTWS.rep ~ "]"
    | "[" ~ ('LTWS.rep ~ 'Elision).? ~ ('LTWS.rep ~ 'BindingRestElement).? ~ 'LTWS.rep ~ "]" ),
  'ClassBody := 'ClassElementList,
  'ContinueStatement := ("continue" | "continue" ~ 'WhiteSpace ~ 'LabelIdentifier),
  'CaseBlock := ("{" ~ ('LTWS.rep ~ 'CaseClauses).? ~ 'LTWS.rep ~ "}"
    | "{" ~ ('LTWS.rep ~ 'CaseClauses).? ~ 'LTWS.rep ~ 'DefaultClause ~ ('LTWS.rep ~ 'CaseClauses).? ~ 'LTWS.rep ~ "}" ),
  'ModuleSpecifier := 'StringLiteral,
  'StringLiteral := ("'" ~ 'SingleStringCharacters.? ~ "'"  | "\"" ~ 'DoubleStringCharacters.? ~ "\"" ),
  'Literal := ('BooleanLiteral  | 'NullLiteral  | 'NumericLiteral  | 'StringLiteral ),
  'NonSurrogate := 'Hex4Digits,
  'NamedImports := ("{" ~ 'LTWS.rep ~ "}"  | "{"~ 'LTWS.rep ~ 'ImportsList ~ ","  ~ 'LTWS.rep ~ "}"  | "{" ~ 'LTWS.rep ~ 'ImportsList ~ 'LTWS.rep ~ "}" ),
  'ClassAtom := ("-"  | 'ClassAtomNoDash ),
  'SP := " ",
  'FunctionRestParameter := 'BindingRestElement,
  'BindingProperty := ('SingleNameBinding  | 'PropertyName ~ 'WhiteSpace.? ~ ":" ~ 'LTWS.rep ~ 'BindingElement ),
  'BitwiseANDExpression := ('EqualityExpression  | 'BitwiseANDExpression ~ 'WhiteSpace.? ~ "&" ~ 'LTWS.rep ~ 'EqualityExpression ),
  'Expression := ('AssignmentExpression  | 'Expression ~ 'WhiteSpace.? ~ "," ~ 'LTWS.rep ~ 'AssignmentExpression ),
  'NonZeroDigit := ("1"  | "2"  | "3"  | "4"  | "5"  | "6"  | "7"  | "8"  | "9" ),
  'LogicalORExpression := ('LogicalANDExpression  | 'LogicalORExpression ~ 'WhiteSpace.? ~ "||" ~ 'WhiteSpace.? ~ 'LogicalANDExpression ),
  'ArrowFunction := (("async" ~ 'WhiteSpace).? ~ 'ArrowParameters ~ 'WhiteSpace ~ "=>" ~ 'WhiteSpace ~ 'ConciseBody),
  'SingleStringCharacter := ('LineContinuation  | "\\" ~ 'EscapeSequence  | "[^'\\\\\r\n]".regex ),
  'CatchParameter := ('BindingIdentifier  | 'BindingPattern ),
  'BindingPropertyList := ('BindingProperty  | 'BindingPropertyList ~ "," ~ 'BindingProperty ),
  'QuantifierPrefix := ("*"  | "+"  | "?"  | "{" ~ 'LTWS.rep ~ 'DecimalDigits ~ 'LTWS.rep ~ "," ~ 'LTWS.rep ~ "}"
    | "{" ~ 'LTWS.rep ~ 'DecimalDigits ~ 'WhiteSpace.? ~ "," ~ 'LTWS.rep ~ 'DecimalDigits ~ 'LTWS.rep ~ "}"
    | "{" ~ 'LTWS.rep ~ 'DecimalDigits ~ 'LTWS.rep ~ "}" ),
  'CoverInitializedName := ('IdentifierReference ~ 'WhiteSpace.? ~ 'Initializer),
  'CaseClauses := ('CaseClause | 'CaseClauses ~ 'CaseClause ),
  'SyntaxCharacter := ("$"  | "("  | ")"  | "*"  | "+"  | "."  | "?"  | "["  | "\\"  | "]"  | "^"  | "{"  | "|"  | "}" ),
  'SingleNameBinding := ('BindingIdentifier ~ ('WhiteSpace ~ 'Initializer).?),
  'BitwiseORExpression := ('BitwiseXORExpression  | 'BitwiseORExpression ~ 'WhiteSpace.? ~ "|" ~ 'LTWS.rep ~ 'BitwiseXORExpression ),
  'ConditionalExpression := ('ShortCircuitExpression  | 'ShortCircuitExpression ~ 'WhiteSpace.? ~ "?" ~ 'LTWS.rep ~ 'AssignmentExpression ~ 'WhiteSpace.? ~ ":" ~ 'LTWS.rep ~ 'AssignmentExpression ),
  'ShortCircuitExpression := ('LogicalORExpression | 'CoalesceExpression),
  'CoalesceExpression := 'CoalesceExpressionHead ~ 'WhiteSpace.? ~ "??" ~ 'LTWS.rep ~ 'BitwiseORExpression,
  'CoalesceExpressionHead := ('CoalesceExpression | 'BitwiseORExpression),
  'YieldExpression := ("yield" ~ 'WhiteSpace ~ "*" ~ 'WhiteSpace ~ 'AssignmentExpression  | "yield" ~ 'WhiteSpace ~ 'AssignmentExpression ),
  'AssignmentExpression := ('ArrowFunction  | 'AwaitExpression  | 'ConditionalExpression  | 'YieldExpression
    | 'LeftHandSideExpression ~ "=" ~ 'AssignmentExpression
    | 'LeftHandSideExpression ~ 'WhiteSpace.? ~ 'AssignmentOperator ~ 'WhiteSpace.? ~ 'AssignmentExpression
    // it is unclear why ||= and &&= are not assignment operators, however, the language spec does it like this
    | 'LeftHandSideExpression ~ 'WhiteSpace.? ~ "&&=" ~ 'WhiteSpace.? ~ 'AssignmentExpression
    | 'LeftHandSideExpression ~ 'WhiteSpace.? ~ "||=" ~ 'WhiteSpace.? ~ 'AssignmentExpression
    | 'LeftHandSideExpression ~ 'WhiteSpace.? ~ "??=" ~ 'WhiteSpace.? ~ 'AssignmentExpression
    ),
  'GeneratorExpression := ("function*" ~ 'WhiteSpace.? ~ 'BindingIdentifier.? ~ "(" ~ ('WhiteSpace.? ~ 'FormalParameters).? ~ 'WhiteSpace.? ~ ")"  ~ 'LTWS.rep ~ "{" ~ 'GeneratorBody ~ 'LTWS.rep ~ "}"),
  'ArrowParameters := ('ArrowFormalParameters  | 'BindingIdentifier ),
  'ImportClause := ('ImportedDefaultBinding  | 'NameSpaceImport  | 'NamedImports  | 'ImportedDefaultBinding ~ "," ~ 'NameSpaceImport  | 'ImportedDefaultBinding ~ "," ~ 'NamedImports ),
  'Hex4Digits := 'HexDigit.rep(4, 4),
  'OctalDigit := ("0"  | "1"  | "2"  | "3"  | "4"  | "5"  | "6"  | "7" ),
  'ClassDeclaration := ("class" ~ 'WhiteSpace ~ 'BindingIdentifier ~ 'ClassTail),
  'NewTarget := ("new" ~ "." ~ "target"),
  'TemplateCharacter := ("$[^{]".regex  | 'LineContinuation  | 'LineTerminatorSequence  | "\\" ~ 'EscapeSequence  | "[^\n\u2028\r\u2029`$\\\\]".regex ),
  'MemberExpression := ('MetaProperty
    | 'PrimaryExpression
    | 'SuperProperty
    | "new" ~ 'WhiteSpace ~ 'MemberExpression ~ 'Arguments
    | 'MemberExpression ~ "." ~ 'IdentifierName
    | 'MemberExpression ~ "[" ~ 'WhiteSpace.? ~ 'Expression ~ 'WhiteSpace.? ~ "]" ~ 'WhiteSpace.?
    | 'MemberExpression ~ 'TemplateLiteral ),
  'BindingElisionElement := ('Elision.? ~ 'BindingElement),
  'EmptyStatement := ";",
  'UnicodeEscapeSequence := ("u" ~ 'Hex4Digits  | "u{" ~ 'HexDigits ~ "}" ),
  'Quantifier := ('QuantifierPrefix  | 'QuantifierPrefix ~ "?" ),
  'GeneratorMethod := ("*" ~ 'PropertyName ~ "(" ~ ('WhiteSpace.? ~ 'StrictFormalParameters).? ~ 'WhiteSpace.? ~ ")"  ~ 'LTWS.rep ~ "{" ~ 'GeneratorBody ~ 'LTWS.rep ~ "}"),
  'GeneratorDeclaration := ("function*" ~ 'WhiteSpace ~ 'BindingIdentifier ~ "(" ~ ('WhiteSpace.? ~ 'FormalParameters).?  ~ 'WhiteSpace.? ~ ")"  ~ 'LTWS.rep ~ "{" ~ 'GeneratorBody ~ 'LTWS.rep ~ "}"
    | "function*(" ~ ('WhiteSpace.? ~ 'FormalParameters).? ~ ")"  ~ 'LTWS.rep ~ "{" ~ 'GeneratorBody ~ 'LTWS.rep ~ "}"),
  'NameSpaceImport := ("* as" ~ 'WhiteSpace ~ 'ImportedBinding),
  'ImportDeclaration := ("import" ~ 'WhiteSpace ~ 'ImportClause ~ 'WhiteSpace ~ 'FromClause ~ ";" ~ 'LineTerminator  | "import" ~ 'WhiteSpace ~ 'ModuleSpecifier ~ ";" ~ 'LineTerminator ),
  'BindingElement := ('SingleNameBinding  | 'BindingPattern ~ ('WhiteSpace ~ 'Initializer).? ),
  'ReturnStatement := ("return" | "return" ~ 'WhiteSpace ~ 'Expression ),
  'Arguments := ("(" ~ 'WhiteSpace.? ~ ")" | "(" ~ 'LTWS.rep ~ 'ArgumentList ~ 'LTWS.rep ~ ")" ),
  'ForBinding := ('BindingIdentifier  | 'BindingPattern ),
  'FromClause := ("from" ~ 'WhiteSpace ~ 'ModuleSpecifier),
  'TemplateInlay := ("${" ~ 'Expression ~ "}"),
  'FunctionStatementList := ('LTWS.rep ~ 'StatementList).?,
  'TemplateLiteral := 'SubstitutionTemplate,
  'RelationalExpression := ('ShiftExpression
    | 'RelationalExpression ~ 'WhiteSpace.? ~ "<" ~ 'LTWS.? ~ 'ShiftExpression
    | 'RelationalExpression ~ 'WhiteSpace.? ~ "<=" ~ 'LTWS.? ~ 'ShiftExpression
    | 'RelationalExpression ~ 'WhiteSpace.? ~ ">" ~ 'LTWS.? ~ 'ShiftExpression
    | 'RelationalExpression ~ 'WhiteSpace.? ~ ">=" ~ 'LTWS.? ~ 'ShiftExpression
    | 'RelationalExpression ~ 'WhiteSpace.? ~ "in" ~ 'LTWS ~ 'ShiftExpression
    | 'RelationalExpression ~ "instanceof" ~ 'WhiteSpace ~ 'ShiftExpression ),
  'NonemptyClassRanges := ('ClassAtomNoDash  | 'ClassAtom ~ "-" ~ 'ClassAtom ~ 'ClassRanges  | 'ClassAtom ~ 'NonemptyClassRangesNoDash ),
  'DecimalLiteral := ("." ~ 'DecimalDigits ~ 'ExponentPart.?  | 'DecimalIntegerLiteral ~ "." ~ 'DecimalDigits.? ~ 'ExponentPart.?  | 'DecimalIntegerLiteral ~ 'ExponentPart.? | 'DecimalIntegerLiteral ~ "n"),
  'VariableStatement := ("var" ~ 'WhiteSpace ~ 'VariableDeclarationList),
  'HexEscapeSequence := ("x" ~ 'HexDigit ~ 'HexDigit),
  'PropertySetParameterList := 'FormalParameter,
  'ObjectBindingPattern := ("{" ~ 'LTWS.rep ~ "}"  | "{" ~ 'LTWS.rep ~ 'BindingPropertyList ~ "," ~ 'LTWS.rep ~ "}"  | "{" ~ 'LTWS.rep ~ 'BindingPropertyList ~ 'LTWS.rep ~ "}" ),
  'StrictFormalParameters := 'FormalParameters,
  'PropertyDefinition := ('CoverInitializedName  | 'IdentifierReference  | 'MethodDefinition  | 'PropertyName ~ 'LTWS.rep ~ ":" ~ 'LTWS.rep ~ 'AssignmentExpression ),
  'DecimalDigit := ("0"  | "1"  | "2"  | "3"  | "4"  | "5"  | "6"  | "7"  | "8"  | "9" ),
  'VariableDeclarationList := ('VariableDeclaration ~ ("," ~ 'WhiteSpace.? ~ 'VariableDeclaration).rep),
  'ArgumentList := ('AssignmentExpression
    | "..." ~ 'LTWS.rep ~ 'AssignmentExpression
    | 'ArgumentList ~ 'LTWS.rep ~ ","  ~ 'LTWS.rep ~ 'AssignmentExpression
    | 'ArgumentList ~ 'LTWS.rep ~ "," ~ 'LTWS.rep ~ "..." ~ 'LTWS.rep ~ 'AssignmentExpression),
  'Statement := ('BlockStatement
    | 'BreakStatement
    | 'BreakableStatement
    | 'ContinueStatement
    | 'DebuggerStatement
    | 'EmptyStatement
    | 'ExpressionStatement
    | 'IfStatement
    | 'LabelledStatement
    | 'ReturnStatement
    | 'ThrowStatement
    | 'TryStatement
    | 'VariableStatement
    | 'WithStatement ),
  'NullLiteral := "null",
  'LabelledItem := ('FunctionDeclaration  | 'Statement ),
  'Declaration := ('ClassDeclaration  | 'HoistableDeclaration  | 'LexicalDeclaration ),
  'SuperCall := ("super" ~ 'Arguments),
  'DebuggerStatement := "debugger",
  'VariableDeclaration := ('BindingIdentifier ~ ('WhiteSpace.? ~ 'Initializer).?  | 'BindingPattern ~ 'WhiteSpace.? ~ 'Initializer ),
  'LexicalDeclaration := ('LetOrConst ~ 'LTWS.rep ~ 'BindingList),
  'LetOrConst := ("const" | "let"),
  'PostfixExpression := ('LeftHandSideExpression
    | 'LeftHandSideExpression ~ "++"
    | 'LeftHandSideExpression ~ "--" ),
  'ClassAtomNoDash := ("\\" ~ 'ClassEscape  | "[^\\\\-\\]-]".regex ),
  'ClassTail := ('ClassHeritage.? ~ "{" ~ ('LTWS.rep ~ 'ClassBody).? ~ 'LTWS.rep ~ "}"),
  'IdentityEscape := ("/"  | 'SyntaxCharacter  | "[a-zA-Z0-9]".regex ),
  'ClassHeritage := ("extends" ~ 'WhiteSpace ~ 'LeftHandSideExpression),
  'ImportsList := ('ImportSpecifier ~ ("," ~ 'ImportSpecifier).rep),
  'TryStatement := ("try" ~ 'WhiteSpace.? ~ 'Block ~ 'LTWS.rep ~ 'Catch ~ 'LTWS.rep ~ 'Finally  | "try" ~ 'WhiteSpace.? ~ 'Block ~ 'LTWS.rep ~ 'Catch  | "try" ~ 'WhiteSpace.? ~ 'Block ~ 'LTWS.rep ~ 'Finally ),
  'BindingRestElement := ("..." ~ 'BindingIdentifier | "..." ~ 'BindingPattern),
  'ForDeclaration := ('LetOrConst ~ 'LTWS.rep ~ 'ForBinding),
  'LabelledStatement := ('LabelIdentifier ~ ":" ~ 'LabelledItem),
  'ClassRanges := (""  | 'NonemptyClassRanges ),
  'MethodDefinition := ('GeneratorMethod
    | "constructor" ~ 'WhiteSpace ~ "(" ~ ('WhiteSpace.? ~ 'StrictFormalParameters).? ~ 'WhiteSpace.? ~ ")"  ~ 'LTWS.rep ~ "{" ~ 'FunctionBody ~ 'LTWS.rep ~ "}"
    | "get" ~ 'WhiteSpace.? ~ 'PropertyName ~ 'WhiteSpace.? ~ "(" ~ 'WhiteSpace.? ~ ")" ~ 'LTWS.? ~ "{" ~ 'FunctionBody ~ 'LTWS.rep ~ "}"
    | "set" ~ 'WhiteSpace.? ~ 'PropertyName ~ 'WhiteSpace.? ~ "(" ~ 'PropertySetParameterList ~ ")"  ~ 'LTWS.rep ~ "{" ~ 'FunctionBody ~ 'LTWS.rep ~ "}"
    | (("async" ~ 'WhiteSpace).? ~ 'PropertyName ~ "(" ~ ('WhiteSpace.? ~ 'StrictFormalParameters).? ~ 'WhiteSpace.? ~ ")" ~ 'WhiteSpace.? ~ "{" ~ 'FunctionBody ~ 'LTWS.rep ~ "}") ),
  'EscapeSequence := ("0"  | 'CharacterEscapeSequence  | 'HexEscapeSequence  | 'UnicodeEscapeSequence ),
  'LogicalANDExpression := ('BitwiseORExpression  | 'LogicalANDExpression ~ 'WhiteSpace.? ~ "&&" ~ 'WhiteSpace.? ~ 'BitwiseORExpression ),
  'HexDigit := "[0-9a-fA-F]".regex,
  'CharacterClass := ("[" ~ "^" ~ 'ClassRanges ~ "]"  | "[" ~ 'ClassRanges ~ "]" ),
  'BinaryDigit := ("0"  | "1" ),
  'MultiplicativeOperator := ("%"  | "*"  | "/" ),
  'OctalIntegerLiteral := ("0O" ~ 'OctalDigits  | "0o" ~ 'OctalDigits ),
  'LineTerminator := 'LF,
  'ModuleBody := 'ModuleItemList,
  'FunctionExpression := ("function" ~ ('WhiteSpace ~ 'BindingIdentifier).? ~ 'WhiteSpace.? ~ "(" ~ ('WhiteSpace.? ~ 'FormalParameters).? ~ 'WhiteSpace.? ~ ")" ~ 'LTWS.rep ~ "{" ~ 'FunctionBody ~ 'LTWS.rep ~ "}"),
  'GeneratorBody := 'FunctionBody,
  'Identifier := 'IdentifierName,
  'ControlEscape := ("f"  | "n"  | "r"  | "t"  | "v" ),
  'BinaryDigits := ('BinaryDigit  | 'BinaryDigits ~ 'BinaryDigit ),
  'Elision := (","  | 'Elision ~ "," ),
  'LexicalBinding := ('BindingIdentifier ~ 'WhiteSpace.? ~ 'Initializer
    | 'BindingIdentifier
    | 'BindingPattern ~ 'WhiteSpace.? ~ 'Initializer ),
  'ExportSpecifier := ('IdentifierName  | 'IdentifierName ~ "as" ~ 'WhiteSpace ~ 'IdentifierName ),
  'LF := "\n",
  'ThrowStatement := ("throw" ~ 'WhiteSpace ~ 'Expression),
  'ClassExpression := ("class" ~ 'WhiteSpace ~ 'BindingIdentifier.? ~ 'ClassTail),
  'HoistableDeclaration := ('FunctionDeclaration  | 'GeneratorDeclaration ),
  'WithStatement := ("with" ~ 'WhiteSpace ~ "(" ~ 'Expression ~ ")" ~ 'WhiteSpace.? ~ 'Statement),
  'CharacterEscapeSequence := ('NonEscapeCharacter  | 'SingleEscapeCharacter ),
  'UnicodeIDContinue := "[a-zA-Z0-9_]".regex,
  'IdentifierName := 'IdentifierStart ~ 'IdentifierPart.rep,
  'IdentifierStart := ('UnicodeLetter  | "\\" ~ 'UnicodeEscapeSequence  | "[$_]".regex ),
  'PropertyDefinitionList := ('PropertyDefinition | 'PropertyDefinitionList ~ 'LTWS.rep ~ "," ~ 'LTWS.rep ~ 'PropertyDefinition ),
  'DecimalEscape := 'NonZeroDigit ~ 'DecimalDigits,
  'StatementListItem := ('Declaration  | 'Statement ),
  'ModuleItem := ('ExportDeclaration
    | 'ImportDeclaration
    | 'StatementListItem ~ ('WhiteSpace.? ~ ";").? ),
  'ClassEscape := ("-"  | "b"  | 'CharacterClassEscape  | 'CharacterEscape  | 'DecimalEscape ),
  'SpreadElement := "..." ~ 'AssignmentExpression,
  'ConciseBody := ('AssignmentExpression  | "{" ~ 'FunctionBody ~ 'LTWS.rep ~ "}" ),
  'LiteralPropertyName := ('IdentifierName  | 'NumericLiteral  | 'StringLiteral ),
  'ExponentPart := ('ExponentIndicator ~ 'SignedInteger ),
  'ExponentIndicator := ("E"  | "e" ),
  'TrailSurrogate := 'Hex4Digits,
  'SuperProperty := ("super" ~ "." ~ 'IdentifierName  | "super" ~ "[" ~ 'Expression ~ "]" ),
  'LeftHandSideExpression := ('CallExpression  | 'NewExpression ),
  'BreakStatement := ("break"  | "break" ~ 'WhiteSpace ~ 'LabelIdentifier),
  'FunctionBody := 'FunctionStatementList,
  'ClassElementList := ('ClassElement ~ 'LineTerminator.?  | 'ClassElementList ~ 'ClassElement ~ 'LineTerminator.? ),
  'Disjunction := ('Alternative  | 'Alternative ~ "|" ~ 'Disjunction ),
  'AssignmentOperator := ("%="  | "*="  | "+="  | "-="  | "/="  | "="  | ">>="  | "^="  | "|=" ),
  'LabelIdentifier := 'Identifier,
  'CharacterEscape := ('ControlEscape  | 'HexEscapeSequence  | 'IdentityEscape  | 'RegExpUnicodeEscapeSequence  | "c" ~ 'ControlLetter ),
  'BindingList := ('LexicalBinding  | 'BindingList ~ 'WhiteSpace.? ~ "," ~ 'LTWS.rep ~ 'LexicalBinding ),
  'Catch := ("catch" ~ 'WhiteSpace.? ~ "(" ~ 'WhiteSpace.? ~ 'CatchParameter ~ 'WhiteSpace.? ~ ")" ~ 'LTWS.rep ~ 'Block),
  'Alternative := (""  | 'Alternative ~ 'Term ),
  'NumericLiteral := ('BinaryIntegerLiteral  | 'DecimalLiteral  | 'HexIntegerLiteral  | 'OctalIntegerLiteral ),
  'Block := ("{" ~ ('LTWS.rep ~ 'StatementList).? ~ 'LTWS.rep ~ "}"),
  'FunctionDeclaration := ("function" ~ 'WhiteSpace.? ~ "(" ~ ('WhiteSpace.? ~ 'FormalParameters).? ~ 'WhiteSpace.? ~ ")" ~ 'LTWS.rep ~ "{" ~ 'FunctionBody ~ 'LTWS.rep ~ "}"
    | "function" ~ 'WhiteSpace ~ 'BindingIdentifier ~ "(" ~ ('WhiteSpace.? ~ 'FormalParameters).? ~ 'WhiteSpace.? ~ ")" ~ 'LTWS.rep ~ "{" ~ 'FunctionBody ~ 'LTWS.rep ~ "}"),
  'Assertion := ("$"  | "\\\\B"  | "\\\\b"  | "^"  | "(" ~ "?" ~ "!" ~ 'Disjunction ~ ")"  | "(" ~ "?" ~ "=" ~ 'Disjunction ~ ")" ),
  'CharacterClassEscape := ("D"  | "S"  | "W"  | "d"  | "s"  | "w" ),
  'IdentifierPart := ("$"  |
    'UnicodeIDContinue  |
    "\\" ~ 'UnicodeEscapeSequence ),
  'HexIntegerLiteral := ("0X" ~ 'HexDigits  | "0x" ~ 'HexDigits ),
  'Initializer := ("=" ~ 'WhiteSpace.? ~ 'AssignmentExpression),
  'SingleStringCharacters := 'SingleStringCharacter.rep(1),
  'ControlLetter := ("A"  | "B"  | "C"  | "D"  | "E"  | "F"  | "G"  | "H"  | "I"  | "J"  | "K"  | "L"  | "M"  | "N"  | "O"  | "P"  | "Q"  | "R"  | "S"  | "T"  | "U"  | "V"  | "W"  | "X"  | "Y"  | "Z"  | "a"  | "b"  | "c"  | "d"  | "e"  | "f"  | "g"  | "h"  | "i"  | "j"  | "k"  | "l"  | "m"  | "n"  | "o"  | "p"  | "q"  | "r"  | "s"  | "t"  | "u"  | "v"  | "w"  | "x"  | "y"  | "z" ),
  'BooleanLiteral := ("false"  | "true" ),
  'SingleEscapeCharacter := ("'"  | "\""  | "\\"  | "b"  | "f"  | "n"  | "r"  | "t"  | "v" ),
  'BindingIdentifier := 'Identifier,
  'FormalParameter := 'BindingElement,
  'RegularExpressionFlags := (""  | 'RegularExpressionFlags ~ 'IdentifierPart ),
  'Finally := ("finally" ~ 'WhiteSpace ~ 'Block),
  'Module := 'ModuleBody,
  'FormalParameterList := ('FormalParameter  | 'FormalParameterList ~ "," ~ 'WhiteSpace.? ~ 'FormalParameter ),
  'RegularExpressionLiteral := ("/" ~ 'Pattern ~ "/" ~ 'RegularExpressionFlags),
  'ElementList := 'Element ~ ("," ~ 'Element).rep,
  'SwitchStatement := ("switch" ~ 'WhiteSpace.? ~ "(" ~ 'Expression ~ ")" ~ 'LTWS.rep ~ 'CaseBlock),
  'ImportedBinding := 'BindingIdentifier,
  'LineContinuation := ("\\" ~ 'LineTerminatorSequence ),
  'ExpressionStatement := 'Expression,
  'ExportDeclaration := ("export" ~ 'WhiteSpace ~ "*" ~ 'WhiteSpace ~ 'FromClause ~ ";"
    | "export" ~ 'WhiteSpace ~ "default" ~ 'WhiteSpace ~ 'AssignmentExpression ~ ";"
    | "export" ~ 'WhiteSpace ~ "default" ~ 'WhiteSpace ~ 'ClassDeclaration
    | "export" ~ 'WhiteSpace ~ "default" ~ 'WhiteSpace ~ 'HoistableDeclaration
    | "export" ~ 'WhiteSpace ~ 'Declaration
    | "export" ~ 'WhiteSpace ~ 'ExportClause ~ ";"
    | "export" ~ 'WhiteSpace ~ 'ExportClause ~ 'FromClause ~ ";"
    | "export" ~ 'WhiteSpace ~ 'VariableStatement ),
  'AdditiveExpression := ('MultiplicativeExpression
    | 'AdditiveExpression ~ 'WhiteSpace.? ~ "+" ~ 'LTWS.rep ~ 'MultiplicativeExpression
    | 'AdditiveExpression ~ 'WhiteSpace.? ~ "-" ~ 'LTWS.rep ~ 'MultiplicativeExpression ),
  'UnicodeLetter
    := "[\u0041-\u005A]".regex
    | "[\u0061-\u007A]".regex
    | "\u00AA"
    | "\u00B5"
    | "\u00BA"
    | "[\u00C0-\u00D6]".regex
    | "[\u00D8-\u00F6]".regex
    | "[\u00F8-\u021F]".regex
    | "[\u0222-\u0233]".regex
    | "[\u0250-\u02AD]".regex
    | "[\u02B0-\u02B8]".regex
    | "[\u02BB-\u02C1]".regex
    | "[\u02D0-\u02D1]".regex
    | "[\u02E0-\u02E4]".regex
    | "\u02EE"
    | "\u037A"
    | "\u0386"
    | "[\u0388-\u038A]".regex
    | "\u038C"
    | "[\u038E-\u03A1]".regex
    | "[\u03A3-\u03CE]".regex
    | "[\u03D0-\u03D7]".regex
    | "[\u03DA-\u03F3]".regex
    | "[\u0400-\u0481]".regex
    | "[\u048C-\u04C4]".regex
    | "[\u04C7-\u04C8]".regex
    | "[\u04CB-\u04CC]".regex
    | "[\u04D0-\u04F5]".regex
    | "[\u04F8-\u04F9]".regex
    | "[\u0531-\u0556]".regex
    | "\u0559"
    | "[\u0561-\u0587]".regex
    | "[\u05D0-\u05EA]".regex
    | "[\u05F0-\u05F2]".regex
    | "[\u0621-\u063A]".regex
    | "[\u0640-\u064A]".regex
    | "[\u0671-\u06D3]".regex
    | "\u06D5"
    | "[\u06E5-\u06E6]".regex
    | "[\u06FA-\u06FC]".regex
    | "\u0710"
    | "[\u0712-\u072C]".regex
    | "[\u0780-\u07A5]".regex
    | "[\u0905-\u0939]".regex
    | "\u093D"
    | "\u0950"
    | "[\u0958-\u0961]".regex
    | "[\u0985-\u098C]".regex
    | "[\u098F-\u0990]".regex
    | "[\u0993-\u09A8]".regex
    | "[\u09AA-\u09B0]".regex
    | "\u09B2"
    | "[\u09B6-\u09B9]".regex
    | "[\u09DC-\u09DD]".regex
    | "[\u09DF-\u09E1]".regex
    | "[\u09F0-\u09F1]".regex
    | "[\u0A05-\u0A0A]".regex
    | "[\u0A0F-\u0A10]".regex
    | "[\u0A13-\u0A28]".regex
    | "[\u0A2A-\u0A30]".regex
    | "[\u0A32-\u0A33]".regex
    | "[\u0A35-\u0A36]".regex
    | "[\u0A38-\u0A39]".regex
    | "[\u0A59-\u0A5C]".regex
    | "\u0A5E"
    | "[\u0A72-\u0A74]".regex
    | "[\u0A85-\u0A8B]".regex
    | "\u0A8D"
    | "[\u0A8F-\u0A91]".regex
    | "[\u0A93-\u0AA8]".regex
    | "[\u0AAA-\u0AB0]".regex
    | "[\u0AB2-\u0AB3]".regex
    | "[\u0AB5-\u0AB9]".regex
    | "\u0ABD"
    | "\u0AD0"
    | "\u0AE0"
    | "[\u0B05-\u0B0C]".regex
    | "[\u0B0F-\u0B10]".regex
    | "[\u0B13-\u0B28]".regex
    | "[\u0B2A-\u0B30]".regex
    | "[\u0B32-\u0B33]".regex
    | "[\u0B36-\u0B39]".regex
    | "\u0B3D"
    | "[\u0B5C-\u0B5D]".regex
    | "[\u0B5F-\u0B61]".regex
    | "[\u0B85-\u0B8A]".regex
    | "[\u0B8E-\u0B90]".regex
    | "[\u0B92-\u0B95]".regex
    | "[\u0B99-\u0B9A]".regex
    | "\u0B9C"
    | "[\u0B9E-\u0B9F]".regex
    | "[\u0BA3-\u0BA4]".regex
    | "[\u0BA8-\u0BAA]".regex
    | "[\u0BAE-\u0BB5]".regex
    | "[\u0BB7-\u0BB9]".regex
    | "[\u0C05-\u0C0C]".regex
    | "[\u0C0E-\u0C10]".regex
    | "[\u0C12-\u0C28]".regex
    | "[\u0C2A-\u0C33]".regex
    | "[\u0C35-\u0C39]".regex
    | "[\u0C60-\u0C61]".regex
    | "[\u0C85-\u0C8C]".regex
    | "[\u0C8E-\u0C90]".regex
    | "[\u0C92-\u0CA8]".regex
    | "[\u0CAA-\u0CB3]".regex
    | "[\u0CB5-\u0CB9]".regex
    | "\u0CDE"
    | "[\u0CE0-\u0CE1]".regex
    | "[\u0D05-\u0D0C]".regex
    | "[\u0D0E-\u0D10]".regex
    | "[\u0D12-\u0D28]".regex
    | "[\u0D2A-\u0D39]".regex
    | "[\u0D60-\u0D61]".regex
    | "[\u0D85-\u0D96]".regex
    | "[\u0D9A-\u0DB1]".regex
    | "[\u0DB3-\u0DBB]".regex
    | "\u0DBD"
    | "[\u0DC0-\u0DC6]".regex
    | "[\u0E01-\u0E30]".regex
    | "[\u0E32-\u0E33]".regex
    | "[\u0E40-\u0E46]".regex
    | "[\u0E81-\u0E82]".regex
    | "\u0E84"
    | "[\u0E87-\u0E88]".regex
    | "\u0E8A"
    | "\u0E8D"
    | "[\u0E94-\u0E97]".regex
    | "[\u0E99-\u0E9F]".regex
    | "[\u0EA1-\u0EA3]".regex
    | "\u0EA5"
    | "\u0EA7"
    | "[\u0EAA-\u0EAB]".regex
    | "[\u0EAD-\u0EB0]".regex
    | "[\u0EB2-\u0EB3]".regex
    | "[\u0EBD-\u0EC4]".regex
    | "\u0EC6"
    | "[\u0EDC-\u0EDD]".regex
    | "\u0F00"
    | "[\u0F40-\u0F6A]".regex
    | "[\u0F88-\u0F8B]".regex
    | "[\u1000-\u1021]".regex
    | "[\u1023-\u1027]".regex
    | "[\u1029-\u102A]".regex
    | "[\u1050-\u1055]".regex
    | "[\u10A0-\u10C5]".regex
    | "[\u10D0-\u10F6]".regex
    | "[\u1100-\u1159]".regex
    | "[\u115F-\u11A2]".regex
    | "[\u11A8-\u11F9]".regex
    | "[\u1200-\u1206]".regex
    | "[\u1208-\u1246]".regex
    | "\u1248"
    | "[\u124A-\u124D]".regex
    | "[\u1250-\u1256]".regex
    | "\u1258"
    | "[\u125A-\u125D]".regex
    | "[\u1260-\u1286]".regex
    | "\u1288"
    | "[\u128A-\u128D]".regex
    | "[\u1290-\u12AE]".regex
    | "\u12B0"
    | "[\u12B2-\u12B5]".regex
    | "[\u12B8-\u12BE]".regex
    | "\u12C0"
    | "[\u12C2-\u12C5]".regex
    | "[\u12C8-\u12CE]".regex
    | "[\u12D0-\u12D6]".regex
    | "[\u12D8-\u12EE]".regex
    | "[\u12F0-\u130E]".regex
    | "\u1310"
    | "[\u1312-\u1315]".regex
    | "[\u1318-\u131E]".regex
    | "[\u1320-\u1346]".regex
    | "[\u1348-\u135A]".regex
    | "[\u13A0-\u13B0]".regex
    | "[\u13B1-\u13F4]".regex
    | "[\u1401-\u1676]".regex
    | "[\u1681-\u169A]".regex
    | "[\u16A0-\u16EA]".regex
    | "[\u1780-\u17B3]".regex
    | "[\u1820-\u1877]".regex
    | "[\u1880-\u18A8]".regex
    | "[\u1E00-\u1E9B]".regex
    | "[\u1EA0-\u1EE0]".regex
    | "[\u1EE1-\u1EF9]".regex
    | "[\u1F00-\u1F15]".regex
    | "[\u1F18-\u1F1D]".regex
    | "[\u1F20-\u1F39]".regex
    | "[\u1F3A-\u1F45]".regex
    | "[\u1F48-\u1F4D]".regex
    | "[\u1F50-\u1F57]".regex
    | "\u1F59"
    | "\u1F5B"
    | "\u1F5D"
    | "[\u1F5F-\u1F7D]".regex
    | "[\u1F80-\u1FB4]".regex
    | "[\u1FB6-\u1FBC]".regex
    | "\u1FBE"
    | "[\u1FC2-\u1FC4]".regex
    | "[\u1FC6-\u1FCC]".regex
    | "[\u1FD0-\u1FD3]".regex
    | "[\u1FD6-\u1FDB]".regex
    | "[\u1FE0-\u1FEC]".regex
    | "[\u1FF2-\u1FF4]".regex
    | "[\u1FF6-\u1FFC]".regex
    | "\u207F"
    | "\u2102"
    | "\u2107"
    | "[\u210A-\u2113]".regex
    | "\u2115"
    | "[\u2119-\u211D]".regex
    | "\u2124"
    | "\u2126"
    | "\u2128"
    | "[\u212A-\u212D]".regex
    | "[\u212F-\u2131]".regex
    | "[\u2133-\u2139]".regex
    | "[\u2160-\u2183]".regex
    | "[\u3005-\u3007]".regex
    | "[\u3021-\u3029]".regex
    | "[\u3031-\u3035]".regex
    | "[\u3038-\u303A]".regex
    | "[\u3041-\u3094]".regex
    | "[\u309D-\u309E]".regex
    | "[\u30A1-\u30FA]".regex
    | "[\u30FC-\u30FE]".regex
    | "[\u3105-\u312C]".regex
    | "[\u3131-\u318E]".regex
    | "[\u31A0-\u31B7]".regex
    | "\u3400"
    | "\u4DB5"
    | "\u4E00"
    | "\u9FA5"
    | "[\uA000-\uA48C]".regex
    | "\uAC00"
    | "\uD7A3"
    | "[\uF900-\uFA2D]".regex
    | "[\uFB00-\uFB06]".regex
    | "[\uFB13-\uFB17]".regex
    | "\uFB1D"
    | "[\uFB1F-\uFB28]".regex
    | "[\uFB2A-\uFB36]".regex
    | "[\uFB38-\uFB3C]".regex
    | "\uFB3E"
    | "[\uFB40-\uFB41]".regex
    | "[\uFB43-\uFB44]".regex
    | "[\uFB46-\uFBB1]".regex
    | "[\uFBD3-\uFD3D]".regex
    | "[\uFD50-\uFD8F]".regex
    | "[\uFD92-\uFDC7]".regex
    | "[\uFDF0-\uFDFB]".regex
    | "[\uFE70-\uFE72]".regex
    | "\uFE74"
    | "[\uFE76-\uFEFC]".regex
    | "[\uFF21-\uFF3A]".regex
    | "[\uFF41-\uFF5A]".regex
    | "[\uFF66-\uFFBE]".regex
    | "[\uFFC2-\uFFC7]".regex
    | "[\uFFCA-\uFFCF]".regex
    | "[\uFFD2-\uFFD7]".regex
    | "[\uFFDA-\uFFDC]".regex,
  'LTWS := ('HT  | 'SP | 'LineTerminator)
)
