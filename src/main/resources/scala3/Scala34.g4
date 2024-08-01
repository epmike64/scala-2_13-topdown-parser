grammar Scala34;

simpleLiteral
    :  IntegerLiteral
    |  FloatingPointLiteral
    |  BooleanLiteral
    |  CharacterLiteral
    |  StringLiteral
    |  SymbolLiteral
    |  'null'
    ;

//// Literals and Paths
id: ID;

qualId
    :   id ('.' id)*
    ;
ids
    : id (',' id)*
    ;

simpleRef
    :   id
    |  (id '.')? 'this'
    |  (id '.')? 'super' classQualifier? '.' id
    ;

classQualifier
    :   '[' id ']'
    ;

/// Types

type_
    :  funType
    |  hkTypeParamClause '=>>' type_
    |  funParamClause '=>>' type_
    |  matchType
    |  infixType
    ;

funType
    :  funTypeArgs ('=>' | '?=>') Type
    |  hkTypeParamClause '=>' Type
    ;

funTypeArgs
    :  infixType
    |  '(' funArgTypes? ')'
    |  funParamClause
    ;

funParamClause
    :  '(' typedFunParam (',' typedFunParam)* ')'
    ;

typedFunParam
    :  id ':' type_
    ;

matchType
    :  infixType 'match'  typeCaseClauses
    ;

infixType
    :  refinedType (id refinedType)*
    ;

refinedType
    :  annotType refinement*
    ;

annotType
    :  simpleType annotation*
    ;

simpleType
    :  simpleLiteral
    |  '?' typeBounds
    |  id
    |  singleton '.' id
    |  singleton '.' 'type'
    |  '(' types ')'
    |  refinement
    |  simpleType1 typeArgs
    |  simpleType1 '#' id
    ;

singleton
    :  simpleRef
    |  simpleLiteral
    |  singleton '.' id
    ;

funArgType
    :  type_
    |  '=>' type_
    ;

funArgTypes
    :  funArgType (',' funArgType )*
    ;

paramType
    :  '=>'? paramValueType
    ;

paramValueType
    :  type_ '*'?
    ;

typeArgs
    :  '[' types ']'
    ;

refinement
    : refineDcl*
    ;

typeBounds
    :  ('>:' type_)? ('<:' type_)?
    ;

typeParamBounds
    :  typeBounds (':' type_)*
    ;

types
    :  type_ (',' type_)*
    ;

////// Expressions

expr
    :  funParams ('=>' | '?=>') expr
    |  hkTypeParamClause '=>' expr
    |  expr1
    ;

blockResult
    :  funParams ('=>' | '?=>') block
    |  hkTypeParamClause '=>' block
    |  expr1
    ;

funParams
    :  bindings
    |  id
    |  '_'
    ;

expr1
    :  'inline'? 'if' '(' expr ')' expr 'else' expr
    |  'inline'? 'if'  expr 'then' expr 'else' expr
    |  'while' '(' expr ')' expr
    |  'while' expr 'do' expr
    |  'try' expr catches ('finally' expr)?
    |  'try' expr ('finally' expr)?
    |  'throw' expr
    |  'return' expr?
    |  forExpr
    |  (simpleExpr '.')? id '=' expr
    |  prefixOperator simpleExpr '=' expr
    |  simpleExpr argumentExprs '=' expr
    |  postfixExpr ascription?
    |  'inline' infixExpr matchClause
    ;

ascription
    :  ':' infixType
    |  ':' annotation+
    ;

catches
    :  'catch' (expr | exprCaseClause)
    ;

postfixExpr
    :  infixExpr id?
    ;

infixExpr
    :  prefixExpr
    |  infixExpr id infixExpr
    |  infixExpr id colonArgument
    |  infixExpr matchClause
    ;

matchClause
    :  'match' caseClauses
    ;

prefixExpr
    :  prefixOperator? simpleExpr
    ;

prefixOperator
    :  '-' | '+' | '~' | '!'
    ;

simpleExpr
    :  simpleRef
    |  Literal
    |  '_'
    |  blockExpr
    |  exprSplice
    |  quoted
    |  QuoteId
    |  'new' constrApp ('with' ConstrApp)* templateBody?
    |  'new' templateBody
    |  '(' exprsInParens ')'
    |  simpleExpr '.' id
    |  simpleExpr '.' matchClause
    |  simpleExpr typeArgs
    |  simpleExpr argumentExprs
    |  simpleExpr colonArgument
    ;

colonArgument
    :  Colon lambdaStart? Indent (caseClauses | block) Outdent
    ;

lambdaStart
    :  funParams ('=>' | '?=>')
    |  hkTypeParamClause '=>'
    ;

quoted
    :  '\'' '{' block '}'
    |  '\'' '[' typeBlock ']'
    ;

exprSplice
    : SpliceId
    |  '$' '{' block '}'
    |  '$' '{'pattern '}'
    ;

exprsInParens
    :  exprInParens (',' exprInParens)*
    ;

exprInParens
    :  postfixExpr ':' type_
    |  expr
    ;

parArgumentExprs
    :  '(' exprsInParens? ')'
    |  '(' 'using' exprsInParens ')'
    |  '(' (exprsInParens ',')? postfixExpr '*' ')'
    ;

argumentExprs
    :  parArgumentExprs
    |  blockExpr
    ;

blockExpr
    :  caseClauses
    | block
    ;

block
    :  blockStat* blockResult?
    ;

blockStat
    :  import_
    |  annotation* localModifier* def
    |  extension
    |  expr1
    |  endMarker
    ;

typeBlock
    :  typeBlockStat* type_
    ;

typeBlockStat
    :  'type' typeDef
    ;

forExpr
    :  'for' '(' Enumerators0 ')'   ('do' | 'yield')? expr
    |  'for' '{' Enumerators0 '}' ('do' | 'yield')? expr
    |  'for'     Enumerators0       ('do' | 'yield') expr
    ;

enumerators0
    :  enumerators
    ;

enumerators
    :  generator (enumerator | guard)*
    ;

enumerator
    :  generator
    |  guard+
    |  pattern1 '=' expr
    ;

generator
    :  'case'? pattern1 '<-' expr
    ;

guard
    :  'if' postfixExpr
    ;

caseClauses
    :  caseClause+
    ;

caseClause
    :  'case' pattern guard? '=>' block
    ;

exprCaseClause
    :  'case' pattern guard? '=>' expr
    ;

typeCaseClauses
    :  typeCaseClause+
;

typeCaseClause
    :  'case' (infixType | '_') '=>' type_
    ;

pattern
    :  pattern1 ('|' pattern1)*
    ;

pattern1
    :  patVar ':' refinedType
    |  '-'? IntegerLiteral ':' refinedType
    |  '-'? FloatingPointLiteral ':' refinedType
    |  pattern2
    ;

pattern2
    :  (id '@')? infixPattern
    ;

infixPattern
    :  simplePattern (id simplePattern)*
    ;

simplePattern
    :  patVar
     |  Literal
     |  '(' patterns? ')'
     |  quoted
     |  simplePattern1 typeArgs? argumentPatterns?
     |  'given' refinedType
     ;

simplePattern1
    :  simpleRef
     |  simplePattern1 '.' id
     ;

patVar
    :  Varid
     |  '_'
     ;

patterns
    :  pattern (',' Pattern)*
    ;

argumentPatterns
    :  '(' patterns? ')'
     |  '(' (patterns ',')? patVar '*' ')'
     ;

////// Type and Value Parameters

clsTypeParamClause
    :  '[' clsTypeParam (',' clsTypeParam)* ']'
    ;

clsTypeParam
    :  annotation* ('+' | '-')? id hkTypeParamClause? typeParamBounds
    ;

typTypeParamClause
    :  '[' typTypeParam (',' typTypeParam)* ']'
    ;

typTypeParam
    :  annotation* id hkTypeParamClause? typeBounds
    ;

hkTypeParamClause :
    '[' hkTypeParam (',' hkTypeParam)* ']'
    ;

hkTypeParam
    :  annotation* ('+' | '-')? (id hkTypeParamClause? | '_') typeBounds
    ;

clsParamClauses   :
    clsParamClause* ( '(' 'implicit'? ClsParams ')')?
    ;

clsParamClause
    :  '(' ClsParams ')'
    |   '(' 'using' (clsParams | funArgTypes) ')'
    ;

clsParams
    :  clsParam (',' clsParam)*
    ;

clsParam
    :  annotation* (modifier* ('val' | 'var'))? param
    ;

defParamClauses
    :  defParamClause+
    ;

defParamClause
    :  defTypeParamClause
    |  defTermParamClause
    |  usingParamClause
    ;

typelessClauses
    :  typelessClause+
    ;

typelessClause
    :  defTermParamClause
    |  usingParamClause
    ;

defTypeParamClause:
    '[' defTypeParam (',' defTypeParam)* ']'
    ;

defTypeParam
    :  annotation* id hkTypeParamClause? typeParamBounds
    ;

defTermParamClause:  '(' defTermParams? ')'
    ;

usingParamClause
    :  '(' 'using' (defTermParams | funArgTypes) ')'
    ;

defImplicitClause
    :  '(' 'implicit' defTermParams ')'
    ;

defTermParams
    : defTermParam (',' defTermParam)*
    ;

defTermParam
    : annotation* 'inline'? param
    ;

param
    :  id ':' paramType ('=' expr)?
    ;

/// Bindings and Imports

bindings
    :  '(' (binding (',' binding)*)? ')'
    ;

binding
    :  (id | '_') (':' type_)?
    ;

modifier
    : localModifier
    |  accessModifier
    |  'override'
    |  'opaque'
    ;

localModifier
    : 'abstract'
    |  'final'
    |  'sealed'
    |  'open'
    |  'implicit'
    |  'lazy'
    |  'inline'
    |  'transparent'
    |  'infix'
    ;

accessModifier
    : ('private' | 'protected') accessQualifier?
    ;

accessQualifier
    : '[' id ']'
    ;

annotation
    : '@' simpleType1 parArgumentExprs*
    ;

import_
    :   'import' importExpr (',' importExpr)*
    ;

export
    : 'export' importExpr (',' importExpr)*
    ;

importExpr
    :  simpleRef (',' simpleRef)* '.' importSpec
    |  simpleRef 'as' id
    ;

importSpec
    : namedSelector
    |  wildCardSelector
    | '{' importSelectors '}'
    ;

namedSelector
    :   id ('as' (id | '_'))?
    ;

wildCardSelector
    : '*'
    | 'given' InfixType?
    ;

importSelectors
    : namedSelector (',' importSelectors)?
    |  wildCardSelector (',' wildCardSelector)*
    ;

endMarker
    :  'end' endMarkerTag    // when followed by EOL
    ;

endMarkerTag
    :  id | 'if' | 'while' | 'for' | 'match' | 'try'
    |  'new' | 'this' | 'given' | 'extension' | 'val'
    ;

//// Declarations and Definitions

refineDcl
    : 'val' valDcl
    |  'def' defDcl
    |  'type' typeDef
    ;

valDcl
    :  ids ':' type_
    ;

defDcl
    : defSig ':' type_
    ;

def
    : 'val' patDef
    |  'var' patDef
    |  'def' defDef
    |  'type'  typeDef
    |  tmplDef
    ;

patDef
    :  ids (':' type_)? ('=' expr)?
    |  pattern2 (':' type_)? ('=' expr)?
    ;

defDef
    :  defSig (':' type_)? ('=' expr)?
    |  'this' typelessClauses defImplicitClause? '=' constrExpr
    ;

defSig
    :  id defParamClauses? defImplicitClause?
    ;

typeDef
    :  id typeParamClause? funParamClause* typeBounds ('=' type_)?
    ;

tmplDef
     :  ('case'? 'class' | 'trait') classDef
    |  'case'? 'object' objectDef
    |  'enum' enumDef
    |  'given' givenDef
    ;

classDef
    :  id classConstr template?
    ;

classConstr
    :  clsTypeParamClause? constrMods? clsParamClauses
    ;

constrMods
    :  annotation* accessModifier?
    ;

objectDef
    :  id template?
    ;

enumDef
    :id classConstr inheritClauses enumBody
    ;

givenDef
    : givenSig? (annotType ('=' expr)? | structuralInstance)
    ;

givenSig
    :  id? defTypeParamClause? usingParamClause* ':'
    ;

structuralInstance
    :  constrApp ('with' constrApp)* ('with' WithTemplateBody)?
    ;
extension
    :  'extension' defTypeParamClause? UsingParamClause*
    '(' defTermParam ')' usingParamClause* extMethods
    ;

extMethods
    : extMethod+
    ;

extMethod
    :  annotation* modifier* 'def' defDef
    |  export
    ;

template
    :  inheritClauses templateBody?
    ;

inheritClauses
    :  ('extends' constrApps)? ('derives' qualId (',' qualId)*)?
    ;

constrApps
    :  constrApp ((',' ConstrApp)* | ('with' ConstrApp)*)
    ;

constrApp
    :  simpleType1 annotation* parArgumentExprs*
    ;

constrExpr
    :  selfInvocation blockStat*
    ;

selfInvocation
    :  'this' argumentExprs+
    ;

withTemplateBody
    : selfType? templateStat+
    ;

templateBody
    : selfType? templateStat+
    ;

templateStat
    : import_
    |  export
    |  annotation* modifier* def
    |  extension
    |  expr1
    |  endMarker
    ;

selfType
    : id (':' infixType)? '=>'
    |  'this' ':' infixType '=>'
    ;

enumBody
    :  selfType? enumStat enumStat*
    ;

enumStat
    : templateStat
    |  annotation* modifier* enumCase
    ;

enumCase
    : 'case' (id classConstr ('extends' ConstrApps)? | ids)
    ;

topStat
    :   import_
    |   export
    |   modifier* def
    |   extension
    ;

topStatSeq
    : topStat*
    ;

compilationUnit
    : ('package' qualId)* topStatSeq
    ;