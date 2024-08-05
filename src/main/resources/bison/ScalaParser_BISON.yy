%token NULL
%token VAR
%token COLON
%token STR_LTR
%token NEW
%token DO
%token ID
%token GH_COLON
%token THROW
%token TYPE
%token VARID
%token DEF
%token MULT
%token PRIVATE
%token TILDE
%token EQ
%token SUPER
%token FLOAT_LTR
%token IF
%token DOT
%token LCURL
%token EXCL
%token TRY
%token FINALLY
%token THIS
%token BOOL_LTR
%token FINAL
%token LT_DASH
%token WITH
%token PIPE
%token FOR
%token LAZY
%token RSQUARE
%token SUB
%token PACKAGE
%token OVERRIDE
%token LT_PERCENT
%token ABSTRACT
%token BoundVarid
%token POUND
%token ELSE
%token CHR_LTR
%token COMMA
%token PROTECTED
%token INT_LTR
%token VAL
%token Scala
%token CLASS
%token CASE
%token SEALED
%token FORSOME
%token UNDERSCORE
%token AT
%token ADD
%token RPAREN
%token LSQUARE
%token CATCH
%token FAT_ARROW
%token EXTENDS
%token RETURN
%token IMPLICIT
%token MATCH
%token SYMB_LTR
%token LT_COLON
%token TRAIT
%token LPAREN
%token YIELD
%token IMPORT
%token RCURL
%token WHILE
%token OBJECT
%token EOF
%token DOT_IMPRT
%token DOT_TYPE
%token DOT_ID
%token DOT_altgrp1
%token EXTENDS_tmpl
%token EXTENDS_body
%token COMMA1
%token M1 M2
%%

compilationUnit
    : topStatSeq
    ;

topStatSeq
    : PACKAGE qualId topStatSeq
	| topStat topStatSeq
	| EOF
    ;

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
    : ID stableIdRest
    | ID DOT altgrp1 stableIdRest
    | altgrp1 stableIdRest
    ;

stableIdRest
	: DOT ID stableIdRest
	| %empty
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
     : stableId DOT_TYPE simpleTypeRest
     | stableId simpleTypeRest
     | LPAREN types RPAREN simpleTypeRest
     ;

simpleTypeRest
    : typeArgs simpleTypeRest
    | POUND ID simpleTypeRest
    | %empty
    ;

typeArgs
    : LSQUARE types RSQUARE
    ;

types
    : type_  typesRest
    ;

typesRest:
	COMMA1 type_ typesRest
	| %empty
	;

refinement
    :  LCURL plus_refineStat RCURL
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
    : IF LPAREN expr RPAREN  expr qsn_grp29
    | WHILE LPAREN expr RPAREN  expr
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
    : infixExpr M1 qsn_ID M2 star_grp33
    ;

infixExpr
    : prefixExpr infixExprRest
    ;

infixExprRest
    : ID infixExpr infixExprRest
    | %empty
    ;

prefixExpr
    : qsn_prefixDef altgrp4
    ;

simpleExpr
    : NEW altgrp5
    | blockExpr
    ;

simpleExpr1
    : literal simpleExpr1Rest
    | stableId simpleExpr1Rest
    | UNDERSCORE simpleExpr1Rest
    | LPAREN qsn_exprs RPAREN simpleExpr1Rest
    | simpleExpr DOT_ID simpleExpr1Rest
    | simpleExpr typeArgs simpleExpr1Rest
    ;

simpleExpr1Rest
    :  qsn_UNDERSCORE DOT ID simpleExpr1Rest
    |  qsn_UNDERSCORE typeArgs simpleExpr1Rest
    |  argumentExprs simpleExpr1Rest
    |  %empty
    ;

exprs
    : expr star_grp34
    ;

argumentExprs
    : LPAREN args RPAREN
    | LCURL args RCURL
    |  blockExpr
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
    : LPAREN qsn_params RPAREN
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
    :  LPAREN qsn_classParams RPAREN
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
    :  LCURL qsn_selfType plus_templateStat RCURL
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
    | TYPE  typeDcl
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
    | TYPE  typeDef
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
    | funSig  LCURL block RCURL
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
    : EXTENDS_tmpl classTemplate
    | EXTENDS_body templateBody
    | templateBody
    | %empty
    ;

traitTemplateOpt
    : EXTENDS_tmpl traitTemplate
    | EXTENDS_body templateBody
    | templateBody
    | %empty
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

packaging
    : PACKAGE qualId  LCURL topStatSeq RCURL
    ;

packageObject
    : PACKAGE OBJECT objectDef
    ;

topStat
    : star_grp57 star_modifier tmplDef
    | import_
    | packaging
    | packageObject
    ;

//////////////////////////////////////////
////////////////////////////////////////////

qsn_SUB:
	SUB
	| %empty
	;

qsn_classQualifier:
	classQualifier
	| %empty
	;

qsn_existentialClause:
	existentialClause
	| %empty
	;

star_existentialDcl:
	existentialDcl star_existentialDcl
	| %empty
	;

plus_existentialDcl:
	existentialDcl star_existentialDcl
	;

qsn_refinement:
	refinement
	| %empty
	;

star_annotation:
	annotation star_annotation
	| %empty
	;

star_refineStat:
	refineStat star_refineStat
	| %empty
	;

plus_refineStat:
	refineStat star_refineStat
	;

plus_annotation:
	annotation star_annotation
	;

qsn_IMPLICIT:
	IMPLICIT
	| %empty
	;

qsn_YIELD:
	YIELD
	| %empty
	;

qsn_expr:
	expr
	| %empty
	;

qsn_UNDERSCORE:
	UNDERSCORE
	| %empty
	;

qsn_ascription:
	ascription
	| %empty
	;

qsn_ID:
	ID
	| %empty
	;

qsn_prefixDef:
	prefixDef
	| %empty
	;

qsn_exprs:
	exprs
	| %empty
	;

star_blockStat:
	blockStat star_blockStat
	| %empty
	;

plus_blockStat:
	blockStat star_blockStat
	;

qsn_resultExpr:
	resultExpr
	| %empty
	;

star_localModifier:
	localModifier star_localModifier
	| %empty
	;

star_generator:
	generator star_generator
	| %empty
	;

plus_generator:
	generator star_generator
	;

star_caseClause:
	caseClause star_caseClause
	| %empty
	;

plus_caseClause:
	caseClause star_caseClause
	;

qsn_guard_:
	guard_
	| %empty
	;

qsn_patterns:
	patterns
	| %empty
	;

qsn_typeParamClause:
	typeParamClause
	| %empty
	;

star_paramClause:
	paramClause star_paramClause
	| %empty
	;

qsn_params:
	params
	| %empty
	;

star_classParamClause:
	classParamClause star_classParamClause
	| %empty
	;

qsn_classParams:
	classParams
	| %empty
	;

star_modifier:
	modifier star_modifier
	| %empty
	;

qsn_accessQualifier:
	accessQualifier
	| %empty
	;

star_argumentExprs:
	argumentExprs star_argumentExprs
	| %empty
	;

qsn_selfType:
	selfType
	| %empty
	;

star_templateStat:
	templateStat star_templateStat
	| %empty
	;

plus_templateStat:
	templateStat star_templateStat
	;

qsn_funTypeParamClause:
	funTypeParamClause
	| %empty
	;

qsn_CASE:
	CASE
	| %empty
	;

star_constrAnnotation:
	constrAnnotation star_constrAnnotation
	| %empty
	;

qsn_accessModifier:
	accessModifier
	| %empty
	;

qsn_earlyDefs:
	earlyDefs
	| %empty
	;

qsn_templateBody:
	templateBody
	| %empty
	;

star_earlyDef:
	earlyDef star_earlyDef
	| %empty
	;

plus_earlyDef:
	earlyDef star_earlyDef
	;

plus_argumentExprs:
	argumentExprs star_argumentExprs
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
	| %empty
	;

qsn_altgrp7:
	IMPLICIT | LAZY
	| %empty
	;

altgrp8:
	qsn_IMPLICIT ID | UNDERSCORE
	;

altgrp9:
	bindings | altgrp8 COLON compoundType
	;

star_altgrp10:
	star_altgrp10_ALT star_altgrp10
	| %empty
	;

star_altgrp10_ALT:
	guard_ | pattern1 EQ expr
	;

altgrp11:
	BoundVarid | UNDERSCORE | ID
	;

qsn_altgrp12:
	ADD | SUB
	| %empty
	;

altgrp13:
	ID | UNDERSCORE
	;

qsn_altgrp14:
	VAL | VAR
	| %empty
	;

altgrp15:
	PRIVATE | PROTECTED
	;

altgrp16:
	ID | THIS
	;

altgrp17:
	UNDERSCORE | importSelectors
	;

altgrp18:
	importSelector | UNDERSCORE
	;

altgrp19:
	EQ constrExpr |  constrBlock
	;

star_grp20:
	DOT ID star_grp20
	| %empty
	;

star_grp21:
	COMMA ID star_grp21
	| %empty
	;

star_grp23:
	COMMA paramType star_grp23
	| %empty
	;

qsn_grp24:
	paramType star_grp23
	| %empty
	;

star_grp25:
	ID compoundType star_grp25
	| %empty
	;

star_grp26:
	WITH annotType star_grp26
	| %empty
	;





qsn_grp29:
	ELSE expr
	| %empty
	;

qsn_grp30:
	CATCH expr
	| %empty
	;

qsn_grp31:
	FINALLY expr
	| %empty
	;

qsn_grp32:
	altgrp4 DOT
	| %empty
	;

star_grp33:
	prefixDef simpleExpr1 star_grp33
	| %empty
	;

star_grp34:
	COMMA expr star_grp34
	| %empty
	;

qsn_grp35:
	exprs COMMA
	| %empty
	;

star_grp36:
	PIPE pattern1 star_grp36
	| %empty
	;

qsn_grp37:
	AT pattern3
	| %empty
	;

star_grp38:
	ID  simplePattern star_grp38
	| %empty
	;

qsn_grp39:
	LPAREN qsn_patterns RPAREN
	| %empty
	;

qsn_grp40:
	patterns COMMA
	| %empty
	;

qsn_grp41:
	ID AT
	| %empty
	;

qsn_grp42:
	COMMA patterns
	| %empty
	;

star_grp43:
	COMMA variantTypeParam star_grp43
	| %empty
	;

star_grp44:
	COMMA typeParam star_grp44
	| %empty
	;

qsn_grp45:
	GH_COLON type_
	| %empty
	;

qsn_grp46:
	LT_COLON type_
	| %empty
	;

star_grp47:
	LT_PERCENT type_ star_grp47
	| %empty
	;

star_grp48:
	COLON type_ star_grp48
	| %empty
	;

qsn_grp49:
	 LPAREN IMPLICIT params RPAREN
	| %empty
	;

star_grp50:
	COMMA param star_grp50
	| %empty
	;

qsn_grp51:
	COLON paramType
	| %empty
	;

qsn_grp52:
	EQ expr
	| %empty
	;

qsn_grp53:
	 LPAREN IMPLICIT classParams RPAREN
	| %empty
	;

star_grp54:
	COMMA classParam star_grp54
	| %empty
	;

star_grp55:
	COMMA binding star_grp55
	| %empty
	;

qsn_grp56:
	COLON type_
	| %empty
	;

star_grp57:
	annotation  star_grp57
	| %empty
	;

star_grp58:
	COMMA importExpr star_grp58
	| %empty
	;

qsn_grp59:
	DOT_IMPRT altgrp17
	| %empty
	;

star_grp60:
	importSelector COMMA star_grp60
	| %empty
	;

qsn_grp61:
	FAT_ARROW altgrp13
	| %empty
	;

star_grp62:
	COMMA pattern2 star_grp62
	| %empty
	;



star_grp64:
	blockStat star_grp64
	| %empty
	;

%%

int main() {
	return 0;
}