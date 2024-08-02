
grammar ScalaParser_BISON_yy;

literal
    : qsn_SUB INT_LTR
    | qsn_SUB FLOAT_LTR
    | BOOL_LTR
    | CHR_LTR
    | STR_LTR
    | SYMB_LTR
    | NULL
    ;

qualId
    : ID star_grp20
    ;

ids
    : ID star_grp21
    ;

stableId
    : ID
    | stableId DOT ID
    | qsn_grp22 altgrp1
    ;

classQualifier
    : LSQUARE ID RSQUARE
    ;

type_
    : functionArgTypes FAT_ARROW type_
    | infixType qsn_existentialClause
    ;

functionArgTypes
    : infixType
    | LPAREN qsn_grp24 RPAREN
    ;

existentialClause
    : FORSOME LCURL plus_existentialDcl RCURL
    ;

existentialDcl
    : TYPE typeDcl
    | VAL valDcl
    ;

infixType
    : compoundType star_grp25
    ;

compoundType
    : annotType star_grp26 qsn_refinement
    | refinement
    ;

annotType
    : simpleType star_annotation
    ;

simpleType
    : simpleType typeArgs
    | simpleType POUND ID
    | stableId qsn_grp27
    | LPAREN types RPAREN
    ;

typeArgs
    : LSQUARE types RSQUARE
    ;

types
    : type_ star_grp28
    ;

refinement
    : qsn_NL LCURL plus_refineStat RCURL
    ;

refineStat
    : dcl
    | TYPE typeDef
    ;

typePat
    : type_
    ;

ascription
    : COLON infixType
    | COLON plus_annotation
    | COLON UNDERSCORE MULT
    ;

expr
    : altgrp2 FAT_ARROW expr
    | expr1
    ;

expr1
    : IF LPAREN expr RPAREN star_NL expr qsn_grp29
    | WHILE LPAREN expr RPAREN star_NL expr
    | TRY expr qsn_grp30 qsn_grp31
    | DO expr WHILE LPAREN expr RPAREN
    | FOR altgrp3 qsn_YIELD expr
    | THROW expr
    | RETURN qsn_expr
    | qsn_grp32 ID EQ expr
    | simpleExpr1 argumentExprs EQ expr
    | postfixExpr qsn_ascription
    | postfixExpr MATCH LCURL caseClauses RCURL
    ;

prefixDef
    : SUB
    | ADD
    | TILDE
    | EXCL
    ;

postfixExpr
    : infixExpr qsn_ID star_grp33 qsn_NL
    ;

infixExpr
    : prefixExpr
    | infixExpr ID qsn_NL infixExpr
    ;

prefixExpr
    : qsn_prefixDef altgrp4
    ;

simpleExpr
    : NEW altgrp5
    | blockExpr
    ;

simpleExpr1
    : literal
    | stableId
    | UNDERSCORE
    | LPAREN qsn_exprs RPAREN
    | simpleExpr DOT ID
    | simpleExpr1 qsn_UNDERSCORE DOT ID
    | simpleExpr typeArgs
    | simpleExpr1 qsn_UNDERSCORE typeArgs
    | simpleExpr1 argumentExprs
    ;

exprs
    : expr star_grp34
    ;

argumentExprs
    : LPAREN args RPAREN
    | LCURL args RCURL
    | qsn_NL blockExpr
    ;

args
    : qsn_exprs
    | qsn_grp35 postfixExpr qsn_altgrp6
    ;

blockExpr
    : LCURL caseClauses RCURL
    | LCURL block RCURL
    ;

block
    : plus_blockStat qsn_resultExpr
    ;

blockStat
    : import_
    | star_annotation qsn_altgrp7 def_
    | star_annotation star_localModifier tmplDef
    | expr1
    ;

resultExpr
    : expr1
    | altgrp9 FAT_ARROW block
    ;

enumerators
    : plus_generator
    ;

generator
    : pattern1 LT_DASH expr star_altgrp10
    ;

caseClauses
    : plus_caseClause
    ;

caseClause
    : CASE pattern qsn_guard_ FAT_ARROW block
    ;

guard_
    : IF postfixExpr
    ;

pattern
    : pattern1 star_grp36
    ;

pattern1
    : altgrp11 COLON typePat
    | pattern2
    ;

pattern2
    : ID qsn_grp37
    | pattern3
    ;

pattern3
    : simplePattern
    | simplePattern star_grp38
    ;

simplePattern
    : UNDERSCORE
    | VARID
    | literal
    | stableId qsn_grp39
    | stableId LPAREN qsn_grp40 qsn_grp41 UNDERSCORE MULT RPAREN
    | LPAREN qsn_patterns RPAREN
    ;

patterns
    : pattern qsn_grp42
    | UNDERSCORE MULT
    ;

typeParamClause
    : LSQUARE variantTypeParam star_grp43 RSQUARE
    ;

funTypeParamClause
    : LSQUARE typeParam star_grp44 RSQUARE
    ;

variantTypeParam
    : star_annotation qsn_altgrp12 typeParam
    ;

typeParam
    : altgrp13 qsn_typeParamClause qsn_grp45 qsn_grp46 star_grp47 star_grp48
    ;

paramClauses
    : star_paramClause qsn_grp49
    ;

paramClause
    : qsn_NL LPAREN qsn_params RPAREN
    ;

params
    : param star_grp50
    ;

param
    : star_annotation ID qsn_grp51 qsn_grp52
    ;

paramType
    : type_
    | FAT_ARROW type_
    | type_ MULT
    ;

classParamClauses
    : star_classParamClause qsn_grp53
    ;

classParamClause
    : qsn_NL LPAREN qsn_classParams RPAREN
    ;

classParams
    : classParam star_grp54
    ;

classParam
    : star_annotation star_modifier qsn_altgrp14 ID COLON paramType qsn_grp52
    ;

bindings
    : LPAREN binding star_grp55 RPAREN
    ;

binding
    : altgrp13 qsn_grp56
    ;

modifier
    : localModifier
    | accessModifier
    | OVERRIDE
    ;

localModifier
    : ABSTRACT
    | FINAL
    | SEALED
    | IMPLICIT
    | LAZY
    ;

accessModifier
    : altgrp15 qsn_accessQualifier
    ;

accessQualifier
    : LSQUARE altgrp16 RSQUARE
    ;

annotation
    : AT simpleType star_argumentExprs
    ;

constrAnnotation
    : AT simpleType argumentExprs
    ;

templateBody
    : qsn_NL LCURL qsn_selfType plus_templateStat RCURL
    ;

templateStat
    : import_
    | star_grp57 star_modifier def_
    | star_grp57 star_modifier dcl
    | expr
    ;

selfType
    : ID qsn_grp56 FAT_ARROW
    | THIS COLON type_ FAT_ARROW
    ;

import_
    : IMPORT importExpr star_grp58
    ;

importExpr
    : stableId qsn_grp59
    ;

importSelectors
    : LCURL star_grp60 altgrp18 RCURL
    ;

importSelector
    : ID qsn_grp61
    ;

dcl
    : VAL valDcl
    | VAR varDcl
    | DEF funDcl
    | TYPE star_NL typeDcl
    ;

valDcl
    : ids COLON type_
    ;

varDcl
    : ids COLON type_
    ;

funDcl
    : funSig qsn_grp56
    ;

funSig
    : ID qsn_funTypeParamClause paramClauses
    ;

typeDcl
    : ID qsn_typeParamClause qsn_grp45 qsn_grp46
    ;

patVarDef
    : VAL patDef
    | VAR varDef
    ;

def_
    : patVarDef
    | DEF funDef
    | TYPE star_NL typeDef
    | tmplDef
    ;

patDef
    : pattern2 star_grp62 qsn_grp56 EQ expr
    ;

varDef
    : patDef
    | ids COLON type_ EQ UNDERSCORE
    ;

funDef
    : funSig qsn_grp56 EQ expr
    | funSig qsn_NL LCURL block RCURL
    | THIS paramClause paramClauses altgrp19
    ;

typeDef
    : ID qsn_typeParamClause EQ type_
    ;

tmplDef
    : qsn_CASE CLASS classDef
    | qsn_CASE OBJECT objectDef
    | TRAIT traitDef
    ;

classDef
    : ID qsn_typeParamClause star_constrAnnotation qsn_accessModifier classParamClauses classTemplateOpt
    ;

traitDef
    : ID qsn_typeParamClause traitTemplateOpt
    ;

objectDef
    : ID classTemplateOpt
    ;

classTemplateOpt
    : EXTENDS classTemplate
    | qsn_grp63
    ;

traitTemplateOpt
    : EXTENDS traitTemplate
    | qsn_grp63
    ;

classTemplate
    : qsn_earlyDefs classParents qsn_templateBody
    ;

traitTemplate
    : qsn_earlyDefs traitParents qsn_templateBody
    ;

classParents
    : constr star_grp26
    ;

traitParents
    : annotType star_grp26
    ;

constr
    : annotType star_argumentExprs
    ;

earlyDefs
    : LCURL plus_earlyDef RCURL WITH
    ;

earlyDef
    : star_grp57 star_modifier patVarDef
    ;

constrExpr
    : selfInvocation
    | constrBlock
    ;

constrBlock
    : LCURL selfInvocation star_grp64 RCURL
    ;

selfInvocation
    : THIS plus_argumentExprs
    ;

topStatSeq
    : plus_topStat
    ;

topStat
    : star_grp57 star_modifier tmplDef
    | import_
    | packaging
    | packageObject
    ;

packaging
    : PACKAGE qualId qsn_NL LCURL topStatSeq RCURL
    ;

packageObject
    : PACKAGE OBJECT objectDef
    ;

compilationUnit
    : star_grp65 topStatSeq
    ;


qsn_SUB:
	SUB
	| EMPTY
	;

qsn_classQualifier:
	classQualifier
	| EMPTY
	;

qsn_existentialClause:
	existentialClause
	| EMPTY
	;

star_existentialDcl:
	existentialDcl star_existentialDcl
	| EMPTY
	;

plus_existentialDcl:
	existentialDcl star_existentialDcl
	;

qsn_refinement:
	refinement
	| EMPTY
	;

star_annotation:
	annotation star_annotation
	| EMPTY
	;

qsn_NL:
	NL
	| EMPTY
	;

star_refineStat:
	refineStat star_refineStat
	| EMPTY
	;

plus_refineStat:
	refineStat star_refineStat
	;

plus_annotation:
	annotation star_annotation
	;

qsn_IMPLICIT:
	IMPLICIT
	| EMPTY
	;

star_NL:
	NL star_NL
	| EMPTY
	;

qsn_YIELD:
	YIELD
	| EMPTY
	;

qsn_expr:
	expr
	| EMPTY
	;

qsn_UNDERSCORE:
	UNDERSCORE
	| EMPTY
	;

qsn_ascription:
	ascription
	| EMPTY
	;

qsn_ID:
	ID
	| EMPTY
	;

qsn_prefixDef:
	prefixDef
	| EMPTY
	;

qsn_exprs:
	exprs
	| EMPTY
	;

star_blockStat:
	blockStat star_blockStat
	| EMPTY
	;

plus_blockStat:
	blockStat star_blockStat
	;

qsn_resultExpr:
	resultExpr
	| EMPTY
	;

star_localModifier:
	localModifier star_localModifier
	| EMPTY
	;

star_generator:
	generator star_generator
	| EMPTY
	;

plus_generator:
	generator star_generator
	;

star_caseClause:
	caseClause star_caseClause
	| EMPTY
	;

plus_caseClause:
	caseClause star_caseClause
	;

qsn_guard_:
	guard_
	| EMPTY
	;

qsn_patterns:
	patterns
	| EMPTY
	;

qsn_typeParamClause:
	typeParamClause
	| EMPTY
	;

star_paramClause:
	paramClause star_paramClause
	| EMPTY
	;

qsn_params:
	params
	| EMPTY
	;

star_classParamClause:
	classParamClause star_classParamClause
	| EMPTY
	;

qsn_classParams:
	classParams
	| EMPTY
	;

star_modifier:
	modifier star_modifier
	| EMPTY
	;

qsn_accessQualifier:
	accessQualifier
	| EMPTY
	;

star_argumentExprs:
	argumentExprs star_argumentExprs
	| EMPTY
	;

qsn_selfType:
	selfType
	| EMPTY
	;

star_templateStat:
	templateStat star_templateStat
	| EMPTY
	;

plus_templateStat:
	templateStat star_templateStat
	;

qsn_funTypeParamClause:
	funTypeParamClause
	| EMPTY
	;

qsn_CASE:
	CASE
	| EMPTY
	;

star_constrAnnotation:
	constrAnnotation star_constrAnnotation
	| EMPTY
	;

qsn_accessModifier:
	accessModifier
	| EMPTY
	;

qsn_EXTENDS:
	EXTENDS
	| EMPTY
	;

qsn_earlyDefs:
	earlyDefs
	| EMPTY
	;

qsn_templateBody:
	templateBody
	| EMPTY
	;

star_earlyDef:
	earlyDef star_earlyDef
	| EMPTY
	;

plus_earlyDef:
	earlyDef star_earlyDef
	;

plus_argumentExprs:
	argumentExprs star_argumentExprs
	;

star_topStat:
	topStat star_topStat
	| EMPTY
	;

plus_topStat:
	topStat star_topStat
	;

altgrp1:
	THIS | SUPER qsn_classQualifier DOT ID
	;

altgrp2:
	bindings | qsn_IMPLICIT ID | UNDERSCORE
	;

altgrp3:
	LPAREN enumerators RPAREN | LCURL enumerators RCURL
	;

altgrp4:
	simpleExpr | simpleExpr1 qsn_UNDERSCORE
	;

altgrp5:
	classTemplate | templateBody
	;

qsn_altgrp6:
	COLON | UNDERSCORE | MULT
	| EMPTY
	;

qsn_altgrp7:
	IMPLICIT | LAZY
	| EMPTY
	;

altgrp8:
	qsn_IMPLICIT ID | UNDERSCORE
	;

altgrp9:
	bindings | altgrp8 COLON compoundType
	;

star_altgrp10:
	star_altgrp10_ALT star_altgrp10
	| EMPTY
	;

star_altgrp10_ALT:
	guard_ | pattern1 EQ expr
	;

altgrp11:
	BoundVarid | UNDERSCORE | ID
	;

qsn_altgrp12:
	ADD | SUB
	| EMPTY
	;

altgrp13:
	ID | UNDERSCORE
	;

qsn_altgrp14:
	VAL | VAR
	| EMPTY
	;

altgrp15:
	PRIVATE | PROTECTED
	;

altgrp16:
	ID | THIS
	;

altgrp17:
	ID | UNDERSCORE | importSelectors
	;

altgrp18:
	importSelector | UNDERSCORE
	;

altgrp19:
	EQ constrExpr | qsn_NL constrBlock
	;

star_grp20:
	DOT ID star_grp20
	| EMPTY
	;

star_grp21:
	COMMA ID star_grp21
	| EMPTY
	;

qsn_grp22:
	ID DOT
	| EMPTY
	;

star_grp23:
	COMMA paramType star_grp23
	| EMPTY
	;

qsn_grp24:
	paramType star_grp23
	| EMPTY
	;

star_grp25:
	ID compoundType star_grp25
	| EMPTY
	;

star_grp26:
	WITH annotType star_grp26
	| EMPTY
	;

qsn_grp27:
	DOT TYPE
	| EMPTY
	;

star_grp28:
	COMMA type_ star_grp28
	| EMPTY
	;

qsn_grp29:
	ELSE expr
	| EMPTY
	;

qsn_grp30:
	CATCH expr
	| EMPTY
	;

qsn_grp31:
	FINALLY expr
	| EMPTY
	;

qsn_grp32:
	altgrp4 DOT
	| EMPTY
	;

star_grp33:
	prefixDef simpleExpr1 star_grp33
	| EMPTY
	;

star_grp34:
	COMMA expr star_grp34
	| EMPTY
	;

qsn_grp35:
	exprs COMMA
	| EMPTY
	;

star_grp36:
	PIPE pattern1 star_grp36
	| EMPTY
	;

qsn_grp37:
	AT pattern3
	| EMPTY
	;

star_grp38:
	ID qsn_NL simplePattern star_grp38
	| EMPTY
	;

qsn_grp39:
	LPAREN qsn_patterns RPAREN
	| EMPTY
	;

qsn_grp40:
	patterns COMMA
	| EMPTY
	;

qsn_grp41:
	ID AT
	| EMPTY
	;

qsn_grp42:
	COMMA patterns
	| EMPTY
	;

star_grp43:
	COMMA variantTypeParam star_grp43
	| EMPTY
	;

star_grp44:
	COMMA typeParam star_grp44
	| EMPTY
	;

qsn_grp45:
	GH_COLON type_
	| EMPTY
	;

qsn_grp46:
	LT_COLON type_
	| EMPTY
	;

star_grp47:
	LT_PERCENT type_ star_grp47
	| EMPTY
	;

star_grp48:
	COLON type_ star_grp48
	| EMPTY
	;

qsn_grp49:
	qsn_NL LPAREN IMPLICIT params RPAREN
	| EMPTY
	;

star_grp50:
	COMMA param star_grp50
	| EMPTY
	;

qsn_grp51:
	COLON paramType
	| EMPTY
	;

qsn_grp52:
	EQ expr
	| EMPTY
	;

qsn_grp53:
	qsn_NL LPAREN IMPLICIT classParams RPAREN
	| EMPTY
	;

star_grp54:
	COMMA classParam star_grp54
	| EMPTY
	;

star_grp55:
	COMMA binding star_grp55
	| EMPTY
	;

qsn_grp56:
	COLON type_
	| EMPTY
	;

star_grp57:
	annotation qsn_NL star_grp57
	| EMPTY
	;

star_grp58:
	COMMA importExpr star_grp58
	| EMPTY
	;

qsn_grp59:
	DOT altgrp17
	| EMPTY
	;

star_grp60:
	importSelector COMMA star_grp60
	| EMPTY
	;

qsn_grp61:
	FAT_ARROW altgrp13
	| EMPTY
	;

star_grp62:
	COMMA pattern2 star_grp62
	| EMPTY
	;

qsn_grp63:
	qsn_EXTENDS templateBody
	| EMPTY
	;

star_grp64:
	blockStat star_grp64
	| EMPTY
	;

star_grp65:
	PACKAGE qualId star_grp65
	| EMPTY
	;