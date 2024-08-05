package com.fdk.compiler.parser

enum FTokenKind(val name: String, val tag: FTag):
	case EOF extends FTokenKind(null, FTag.DEFAULT)
	case ERROR extends FTokenKind(null, FTag.DEFAULT)
	case ID extends FTokenKind(null, FTag.NAMED)
	case ABSTRACT extends FTokenKind("abstract", FTag.DEFAULT)
	case ASSERT extends FTokenKind("assert", FTag.DEFAULT)
	case BOOLEAN extends FTokenKind("boolean", FTag.DEFAULT)
	case BREAK extends FTokenKind("break", FTag.DEFAULT)
	case BYTE extends FTokenKind("byte", FTag.NAMED)
	case CASE extends FTokenKind("case", FTag.DEFAULT)
	case CATCH extends FTokenKind("catch", FTag.DEFAULT)
	case CHAR extends FTokenKind("char", FTag.NAMED)
	case CLASS extends FTokenKind("class", FTag.DEFAULT)
	case CONST extends FTokenKind("const", FTag.DEFAULT)
	case CONTINUE extends FTokenKind("continue", FTag.DEFAULT)
	case DEFAULT extends FTokenKind("default", FTag.DEFAULT)
	case DO extends FTokenKind("do", FTag.DEFAULT)
	case DOUBLE extends FTokenKind("double", FTag.NAMED)
	case ELSE extends FTokenKind("else", FTag.DEFAULT)
	case ENUM extends FTokenKind("enum", FTag.DEFAULT)
	case EXTENDS extends FTokenKind("extends", FTag.DEFAULT)
	case FINAL extends FTokenKind("final", FTag.DEFAULT)
	case FINALLY extends FTokenKind("finally", FTag.DEFAULT)
	case FLOAT extends FTokenKind("float", FTag.NAMED)
	case FOR extends FTokenKind("for", FTag.DEFAULT)
	case GOTO extends FTokenKind("goto", FTag.DEFAULT)
	case IF extends FTokenKind("if", FTag.DEFAULT)
	case IMPLEMENTS extends FTokenKind("implements", FTag.DEFAULT)
	case IMPORT extends FTokenKind("import", FTag.DEFAULT)
	case INSTANCEOF extends FTokenKind("instanceof", FTag.DEFAULT)
	case INT extends FTokenKind("int", FTag.NAMED)

	case LONG extends FTokenKind("long", FTag.DEFAULT)
	case NEW extends FTokenKind("new", FTag.DEFAULT)
	case PACKAGE extends FTokenKind("package", FTag.DEFAULT)
	case PRIVATE extends FTokenKind("private", FTag.DEFAULT)
	case PROTECTED extends FTokenKind("protected", FTag.DEFAULT)
	case PUBLIC extends FTokenKind("public", FTag.DEFAULT)
	case RETURN extends FTokenKind("return", FTag.DEFAULT)
	case SHORT extends FTokenKind("short", FTag.DEFAULT)
	case STATIC extends FTokenKind("static", FTag.DEFAULT)
	case STRICTFP extends FTokenKind("strictfp", FTag.DEFAULT)
	case SUPER extends FTokenKind("super", FTag.DEFAULT)
	case SWITCH extends FTokenKind("switch", FTag.DEFAULT)
	case SYNCHRONIZED extends FTokenKind("synchronized", FTag.DEFAULT)
	case THIS extends FTokenKind("this", FTag.DEFAULT)
	case THROW extends FTokenKind("throw", FTag.DEFAULT)
	case THROWS extends FTokenKind("throws", FTag.DEFAULT)
	case TRANSIENT extends FTokenKind("transient", FTag.DEFAULT)
	case TRY extends FTokenKind("try", FTag.DEFAULT)
	case VOID extends FTokenKind("void", FTag.DEFAULT)
	case VOLATILE extends FTokenKind("volatile", FTag.DEFAULT)
	case WHILE extends FTokenKind("while", FTag.DEFAULT)
	case LPAREN extends FTokenKind("(", FTag.DEFAULT)
	case RPAREN extends FTokenKind(")", FTag.DEFAULT)
	case LBRACE extends FTokenKind("{", FTag.DEFAULT)
	case RBRACE extends FTokenKind("}", FTag.DEFAULT)
	case LBRACKET extends FTokenKind("[", FTag.DEFAULT)
	case RBRACKET extends FTokenKind("]", FTag.DEFAULT)
	case SEMI extends FTokenKind(";", FTag.DEFAULT)
	case COMMA extends FTokenKind(",", FTag.DEFAULT)
	case DOT extends FTokenKind(".", FTag.DEFAULT)
	case ELLIPSIS extends FTokenKind("...", FTag.DEFAULT)
	case EQ extends FTokenKind("=", FTag.DEFAULT)
	case GT extends FTokenKind(">", FTag.DEFAULT)
	case LT extends FTokenKind("<", FTag.DEFAULT)
	case BANG extends FTokenKind("!", FTag.DEFAULT)
	case TILDE extends FTokenKind("~", FTag.DEFAULT)
	case QUES extends FTokenKind("?", FTag.DEFAULT)

	case EQEQ extends FTokenKind("==", FTag.DEFAULT)
	case LTEQ extends FTokenKind("<=", FTag.DEFAULT)
	case GTEQ extends FTokenKind(">=", FTag.DEFAULT)
	case BANGEQ extends FTokenKind("!=", FTag.DEFAULT)
	case AMPAMP extends FTokenKind("&&", FTag.DEFAULT)
	case BARBAR extends FTokenKind("||", FTag.DEFAULT)
	case PLUSPLUS extends FTokenKind("++", FTag.DEFAULT)
	case SUBSUB extends FTokenKind("--", FTag.DEFAULT)
	case PLUS extends FTokenKind("+", FTag.DEFAULT)
	case SUB extends FTokenKind("-", FTag.DEFAULT)
	case STAR extends FTokenKind("*", FTag.DEFAULT)
	case SLASH extends FTokenKind("/", FTag.DEFAULT)
	case AMP extends FTokenKind("&", FTag.DEFAULT)
	case BAR extends FTokenKind("|", FTag.DEFAULT)
	case CARET extends FTokenKind("^", FTag.DEFAULT)
	case PERCENT extends FTokenKind("%", FTag.DEFAULT)
	case LTLT extends FTokenKind("<<", FTag.DEFAULT)
	case GTGT extends FTokenKind(">>", FTag.DEFAULT)
	case GTGTGT extends FTokenKind(">>>", FTag.DEFAULT)
	case PLUSEQ extends FTokenKind("+=", FTag.DEFAULT)
	case SUBEQ extends FTokenKind("-=", FTag.DEFAULT)
	case STAREQ extends FTokenKind("*=", FTag.DEFAULT)
	case SLASHEQ extends FTokenKind("/=", FTag.DEFAULT)
	case AMPEQ extends FTokenKind("&=", FTag.DEFAULT)
	case BAREQ extends FTokenKind("|=", FTag.DEFAULT)
	case CARETEQ extends FTokenKind("^=", FTag.DEFAULT)
	case PERCENTEQ extends FTokenKind("%=", FTag.DEFAULT)
	case UPPER_BOUND extends FTokenKind("<:", FTag.DEFAULT)
	case LOWER_BOUND extends FTokenKind(">:", FTag.DEFAULT)
	case GTGTEQ extends FTokenKind(">>=", FTag.DEFAULT)
	case GTGTGTEQ extends FTokenKind(">>>=", FTag.DEFAULT)
	case USCORE extends FTokenKind("_", FTag.DEFAULT)
	case COLONCOLON extends FTokenKind("::", FTag.DEFAULT)
	case VARID extends FTokenKind("VARID", FTag.NAMED)
	case COLON extends FTokenKind("COLON", FTag.DEFAULT)
	case INT_LTR extends FTokenKind("INT_LTR", FTag.NUMERIC)
	case ADD extends FTokenKind("ADD", FTag.DEFAULT)

	case BOOL_LTR extends FTokenKind("BOOL_LTR", FTag.DEFAULT)

	case VAR extends FTokenKind("VAR", FTag.DEFAULT)

	case POUND extends FTokenKind("POUND", FTag.DEFAULT)

	case RSQUARE extends FTokenKind("RSQUARE", FTag.DEFAULT)
	case BoundVarid extends FTokenKind("BoundVarid", FTag.NAMED)
	case RCURL extends FTokenKind("RCURL", FTag.DEFAULT)

	case EXCL extends FTokenKind("EXCL", FTag.DEFAULT)
	case OBJECT extends FTokenKind("OBJECT", FTag.DEFAULT)

	case LCURL extends FTokenKind("LCURL", FTag.DEFAULT)
	case GH_COLON extends FTokenKind("GH_COLON", FTag.DEFAULT)
	case NULL extends FTokenKind("NULL", FTag.DEFAULT)
	case IMPLICIT extends FTokenKind("IMPLICIT", FTag.DEFAULT)
	case FORSOME extends FTokenKind("FORSOME", FTag.DEFAULT)

	case LT_DASH extends FTokenKind("LT_DASH", FTag.DEFAULT)

	case NL extends FTokenKind("NL", FTag.DEFAULT)

	case LSQUARE extends FTokenKind("LSQUARE", FTag.DEFAULT)

	case SEALED extends FTokenKind("SEALED", FTag.DEFAULT)

	case YIELD extends FTokenKind("YIELD", FTag.DEFAULT)
	case SYMB_LTR extends FTokenKind("SYMB_LTR", FTag.DEFAULT)
	case TRAIT extends FTokenKind("TRAIT", FTag.DEFAULT)
	case LT_COLON extends FTokenKind("LT_COLON", FTag.DEFAULT)
	case FLOAT_LTR extends FTokenKind("FLOAT_LTR", FTag.NUMERIC)
	case MATCH extends FTokenKind("MATCH", FTag.DEFAULT)
	case LT_PERCENT extends FTokenKind("LT_PERCENT", FTag.DEFAULT)
	case LAZY extends FTokenKind("LAZY", FTag.DEFAULT)

	case TYPE extends FTokenKind("TYPE", FTag.DEFAULT)

	case STR_LTR extends FTokenKind("STR_LTR", FTag.STRING)

	case AT extends FTokenKind("AT", FTag.DEFAULT)

	case WITH extends FTokenKind("WITH", FTag.DEFAULT)
	case VAL extends FTokenKind("VAL", FTag.DEFAULT)
	case OVERRIDE extends FTokenKind("OVERRIDE", FTag.DEFAULT)
	case FAT_ARROW extends FTokenKind("FAT_ARROW", FTag.DEFAULT)
	case MULT extends FTokenKind("MULT", FTag.DEFAULT)

	case PIPE extends FTokenKind("PIPE", FTag.DEFAULT)
	case DEF extends FTokenKind("DEF", FTag.DEFAULT)
	case UNDERSCORE extends FTokenKind("UNDERSCORE", FTag.DEFAULT)
	case LITERAL extends FTokenKind("LITERAL", FTag.DEFAULT)
	case CHR_LTR extends FTokenKind("CHR_LTR", FTag.STRING)
