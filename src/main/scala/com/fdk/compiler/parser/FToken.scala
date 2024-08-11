package com.fdk.compiler.parser

import com.fdk.compiler.parser.FToken.FTokenKind

object FToken {

	enum FTokenTag {
		case DEFAULT
		case NAMED
		case STRING
		case NUMERIC
	}

	enum FTokenKind(val name: String, val tag: FTokenTag) {
		case LONGLITERAL extends FTokenKind(null, FTokenTag.NUMERIC)
		case FLOATLITERAL extends FTokenKind(null, FTokenTag.NUMERIC)
		case INTLITERAL extends FTokenKind(null, FTokenTag.NUMERIC)
		case DOUBLELITERAL extends FTokenKind(null, FTokenTag.NUMERIC)
		case CHARLITERAL extends FTokenKind(null, FTokenTag.NUMERIC)
		case STRINGLITERAL extends FTokenKind(null, FTokenTag.STRING)
		case TRUE extends FTokenKind("true", FTokenTag.NAMED)
		case FALSE extends FTokenKind("false", FTokenTag.NAMED)
		case NULL extends FTokenKind("null", FTokenTag.NAMED)
		case UNDERSCORE extends FTokenKind("_", FTokenTag.NAMED)
		case ID extends FTokenKind(null, FTokenTag.NAMED)
		case BOOLEAN extends FTokenKind("boolean", FTokenTag.NAMED)
		case ASSERT extends FTokenKind("assert", FTokenTag.NAMED)
		case BYTE extends FTokenKind("byte", FTokenTag.NAMED)
		case VOID extends FTokenKind("void", FTokenTag.NAMED)

		case EOF extends FTokenKind(null, FTokenTag.DEFAULT)
		case ERROR extends FTokenKind(null, FTokenTag.DEFAULT)
		case ABSTRACT extends FTokenKind("abstract", FTokenTag.DEFAULT)
		case BREAK extends FTokenKind("break", FTokenTag.DEFAULT)
		case CASE extends FTokenKind("case", FTokenTag.DEFAULT)
		case CATCH extends FTokenKind("catch", FTokenTag.DEFAULT)
		case CHAR extends FTokenKind("char", FTokenTag.NAMED)
		case CLASS extends FTokenKind("class", FTokenTag.DEFAULT)
		case CONST extends FTokenKind("const", FTokenTag.DEFAULT)
		case CONTINUE extends FTokenKind("continue", FTokenTag.DEFAULT)
		case DEFAULT extends FTokenKind("default", FTokenTag.DEFAULT)
		case DO extends FTokenKind("do", FTokenTag.DEFAULT)
		case DOUBLE extends FTokenKind("double", FTokenTag.NAMED)
		case ELSE extends FTokenKind("else", FTokenTag.DEFAULT)
		case ENUM extends FTokenKind("enum", FTokenTag.DEFAULT)
		case EXTENDS extends FTokenKind("extends", FTokenTag.DEFAULT)
		case FINAL extends FTokenKind("final", FTokenTag.DEFAULT)
		case FINALLY extends FTokenKind("finally", FTokenTag.DEFAULT)
		case FLOAT extends FTokenKind("float", FTokenTag.NAMED)
		case FOR extends FTokenKind("for", FTokenTag.DEFAULT)
		case GOTO extends FTokenKind("goto", FTokenTag.DEFAULT)
		case IF extends FTokenKind("if", FTokenTag.DEFAULT)
		case IMPLEMENTS extends FTokenKind("implements", FTokenTag.DEFAULT)
		case IMPORT extends FTokenKind("import", FTokenTag.DEFAULT)
		case INSTANCEOF extends FTokenKind("instanceof", FTokenTag.DEFAULT)
		case INT extends FTokenKind("int", FTokenTag.NAMED)
		case THIS extends FTokenKind("this", FTokenTag.NAMED)

		case LONG extends FTokenKind("long", FTokenTag.DEFAULT)
		case NEW extends FTokenKind("new", FTokenTag.DEFAULT)
		case PACKAGE extends FTokenKind("package", FTokenTag.DEFAULT)
		case PRIVATE extends FTokenKind("private", FTokenTag.DEFAULT)
		case PROTECTED extends FTokenKind("protected", FTokenTag.DEFAULT)
		case PUBLIC extends FTokenKind("public", FTokenTag.DEFAULT)
		case RETURN extends FTokenKind("return", FTokenTag.DEFAULT)
		case SHORT extends FTokenKind("short", FTokenTag.DEFAULT)
		case STATIC extends FTokenKind("static", FTokenTag.DEFAULT)
		case STRICTFP extends FTokenKind("strictfp", FTokenTag.DEFAULT)
		case SUPER extends FTokenKind("super", FTokenTag.DEFAULT)
		case SWITCH extends FTokenKind("switch", FTokenTag.DEFAULT)
		case SYNCHRONIZED extends FTokenKind("synchronized", FTokenTag.DEFAULT)
		case THROW extends FTokenKind("throw", FTokenTag.DEFAULT)
		case THROWS extends FTokenKind("throws", FTokenTag.DEFAULT)
		case TRANSIENT extends FTokenKind("transient", FTokenTag.DEFAULT)
		case TRY extends FTokenKind("try", FTokenTag.DEFAULT)
		case VOLATILE extends FTokenKind("volatile", FTokenTag.DEFAULT)
		case WHILE extends FTokenKind("while", FTokenTag.DEFAULT)
		case LPAREN extends FTokenKind("(", FTokenTag.DEFAULT)
		case RPAREN extends FTokenKind(")", FTokenTag.DEFAULT)
		case LCURL extends FTokenKind("{", FTokenTag.DEFAULT)
		case RCURL extends FTokenKind("}", FTokenTag.DEFAULT)
		case LBRACKET extends FTokenKind("[", FTokenTag.DEFAULT)
		case RBRACKET extends FTokenKind("]", FTokenTag.DEFAULT)
		case SEMI extends FTokenKind(";", FTokenTag.DEFAULT)
		case COMMA extends FTokenKind(",", FTokenTag.DEFAULT)
		case DOT extends FTokenKind(".", FTokenTag.DEFAULT)
		case ELLIPSIS extends FTokenKind("...", FTokenTag.DEFAULT)
		case EQ extends FTokenKind("=", FTokenTag.DEFAULT)
		case GT extends FTokenKind(">", FTokenTag.DEFAULT)
		case LT extends FTokenKind("<", FTokenTag.DEFAULT)
		case BANG extends FTokenKind("!", FTokenTag.DEFAULT)
		case TILDE extends FTokenKind("~", FTokenTag.DEFAULT)
		case QUES extends FTokenKind("?", FTokenTag.DEFAULT)

		case EQEQ extends FTokenKind("==", FTokenTag.DEFAULT)
		case LTEQ extends FTokenKind("<=", FTokenTag.DEFAULT)
		case GTEQ extends FTokenKind(">=", FTokenTag.DEFAULT)
		case BANGEQ extends FTokenKind("!=", FTokenTag.DEFAULT)
		case AMPAMP extends FTokenKind("&&", FTokenTag.DEFAULT)
		case BARBAR extends FTokenKind("||", FTokenTag.DEFAULT)
		case PLUSPLUS extends FTokenKind("++", FTokenTag.DEFAULT)
		case SUBSUB extends FTokenKind("--", FTokenTag.DEFAULT)
		case PLUS extends FTokenKind("+", FTokenTag.DEFAULT)
		case SUB extends FTokenKind("-", FTokenTag.DEFAULT)
		case STAR extends FTokenKind("*", FTokenTag.DEFAULT)
		case SLASH extends FTokenKind("/", FTokenTag.DEFAULT)
		case AMP extends FTokenKind("&", FTokenTag.DEFAULT)
		case BAR extends FTokenKind("|", FTokenTag.DEFAULT)
		case CARET extends FTokenKind("^", FTokenTag.DEFAULT)
		case PERCENT extends FTokenKind("%", FTokenTag.DEFAULT)
		case LTLT extends FTokenKind("<<", FTokenTag.DEFAULT)
		case GTGT extends FTokenKind(">>", FTokenTag.DEFAULT)
		case GTGTGT extends FTokenKind(">>>", FTokenTag.DEFAULT)
		case PLUSEQ extends FTokenKind("+=", FTokenTag.DEFAULT)
		case SUBEQ extends FTokenKind("-=", FTokenTag.DEFAULT)
		case STAREQ extends FTokenKind("*=", FTokenTag.DEFAULT)
		case SLASHEQ extends FTokenKind("/=", FTokenTag.DEFAULT)
		case AMPEQ extends FTokenKind("&=", FTokenTag.DEFAULT)
		case BAREQ extends FTokenKind("|=", FTokenTag.DEFAULT)
		case CARETEQ extends FTokenKind("^=", FTokenTag.DEFAULT)
		case PERCENTEQ extends FTokenKind("%=", FTokenTag.DEFAULT)
		case UPPER_BOUND extends FTokenKind("<:", FTokenTag.DEFAULT)
		case LOWER_BOUND extends FTokenKind(">:", FTokenTag.DEFAULT)
		case GTGTEQ extends FTokenKind(">>=", FTokenTag.DEFAULT)
		case GTGTGTEQ extends FTokenKind(">>>=", FTokenTag.DEFAULT)
		case USCORE extends FTokenKind("_", FTokenTag.DEFAULT)
		case COLONCOLON extends FTokenKind("::", FTokenTag.DEFAULT)
		case VARID extends FTokenKind("VARID", FTokenTag.NAMED)
		case COLON extends FTokenKind("COLON", FTokenTag.DEFAULT)
		case INT_LTR extends FTokenKind("INT_LTR", FTokenTag.NUMERIC)
		case ADD extends FTokenKind("ADD", FTokenTag.DEFAULT)

		case BOOL_LTR extends FTokenKind("BOOL_LTR", FTokenTag.DEFAULT)

		case VAR extends FTokenKind("VAR", FTokenTag.DEFAULT)

		case POUND extends FTokenKind("POUND", FTokenTag.DEFAULT)

		case RSQUARE extends FTokenKind("RSQUARE", FTokenTag.DEFAULT)
		case BoundVarid extends FTokenKind("BoundVarid", FTokenTag.NAMED)

		case EXCL extends FTokenKind("EXCL", FTokenTag.DEFAULT)
		case OBJECT extends FTokenKind("OBJECT", FTokenTag.DEFAULT)

		case GH_COLON extends FTokenKind("GH_COLON", FTokenTag.DEFAULT)

		case IMPLICIT extends FTokenKind("IMPLICIT", FTokenTag.DEFAULT)
		case FORSOME extends FTokenKind("FORSOME", FTokenTag.DEFAULT)

		case LT_DASH extends FTokenKind("LT_DASH", FTokenTag.DEFAULT)

		case NL extends FTokenKind("NL", FTokenTag.DEFAULT)

		case LSQUARE extends FTokenKind("LSQUARE", FTokenTag.DEFAULT)

		case SEALED extends FTokenKind("SEALED", FTokenTag.DEFAULT)

		case YIELD extends FTokenKind("YIELD", FTokenTag.DEFAULT)
		case SYMB_LTR extends FTokenKind("SYMB_LTR", FTokenTag.DEFAULT)
		case TRAIT extends FTokenKind("TRAIT", FTokenTag.DEFAULT)
		case LT_COLON extends FTokenKind("LT_COLON", FTokenTag.DEFAULT)
		case FLOAT_LTR extends FTokenKind("FLOAT_LTR", FTokenTag.NUMERIC)
		case MATCH extends FTokenKind("MATCH", FTokenTag.DEFAULT)
		case LT_PERCENT extends FTokenKind("LT_PERCENT", FTokenTag.DEFAULT)
		case LAZY extends FTokenKind("LAZY", FTokenTag.DEFAULT)

		case TYPE extends FTokenKind("TYPE", FTokenTag.DEFAULT)

		case STR_LTR extends FTokenKind("STR_LTR", FTokenTag.STRING)

		case AT extends FTokenKind("AT", FTokenTag.DEFAULT)

		case WITH extends FTokenKind("WITH", FTokenTag.DEFAULT)
		case VAL extends FTokenKind("VAL", FTokenTag.DEFAULT)
		case OVERRIDE extends FTokenKind("OVERRIDE", FTokenTag.DEFAULT)
		case FAT_ARROW extends FTokenKind("FAT_ARROW", FTokenTag.DEFAULT)
		case LEFT_ARROW extends FTokenKind("LEFT_ARROW", FTokenTag.DEFAULT)
		case RIGHT_ARROW extends FTokenKind("RIGHT_ARROW", FTokenTag.DEFAULT)
		case MULT extends FTokenKind("MULT", FTokenTag.DEFAULT)

		case PIPE extends FTokenKind("PIPE", FTokenTag.DEFAULT)
		case DEF extends FTokenKind("DEF", FTokenTag.DEFAULT)
		case CHR_LTR extends FTokenKind("CHR_LTR", FTokenTag.STRING)
	}

	private lazy val tokens = {
		var map = Map[String, FTokenKind]()
		FTokenKind.values.foreach { kind =>
			if(kind.name != null) {
				map += (kind.name -> kind)
			}
		}
		map
	}

	def lookupKind(name: String): FTokenKind = {
		tokens.getOrElse(name, FTokenKind.ID)
	}
}

class FToken(val kind: FTokenKind, val pos: Int, val endPos: Int) {

	def name: String = throw new UnsupportedOperationException

	def stringVal: String = throw new UnsupportedOperationException

	def radix: Int = throw new UnsupportedOperationException

	override def toString: String = s"FToken ${kind.toString}($pos, $endPos)"
}

class NamedToken(kind: FTokenKind, pos: Int, endPos: Int, override val name: String) extends FToken(kind, pos, endPos){

	override def toString: String = s"NamedToken ${kind.toString}($pos, $endPos, $name)"
}

class StringToken(kind: FTokenKind, pos: Int, endPos: Int, override val stringVal: String) extends FToken(kind, pos, endPos) {

	override def toString: String = s"StringToken ${kind.toString}($pos, $endPos, $stringVal)"
}

class NumericToken(kind: FTokenKind, pos: Int, endPos: Int, override val stringVal: String, override val radix: Int) extends StringToken(kind, pos, endPos, stringVal){

	override def toString: String = s"NumericToken ${kind.name}($pos, $endPos, $stringVal, $radix)"
}

