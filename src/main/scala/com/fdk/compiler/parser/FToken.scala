package com.fdk.compiler.parser

object FToken {

	enum FTokenTag {
		case DEFAULT
		case NAMED
		case STRING
		case NUMERIC
	}

	enum FTokenKind(val name: String, val tag: FTokenTag) {
		case ABSTRACT extends FTokenKind("abstract", FTokenTag.DEFAULT)
		case CASE extends FTokenKind("case", FTokenTag.DEFAULT)
		case CATCH extends FTokenKind("catch", FTokenTag.DEFAULT)
		case CLASS extends FTokenKind("class", FTokenTag.DEFAULT)
		case DEF extends FTokenKind("def", FTokenTag.DEFAULT)
		case DO extends FTokenKind("do", FTokenTag.DEFAULT)
		case ELSE extends FTokenKind("else", FTokenTag.DEFAULT)
		case EXTENDS extends FTokenKind("extends", FTokenTag.DEFAULT)
		case FALSE extends FTokenKind("false", FTokenTag.NAMED)
		case FINAL extends FTokenKind("final", FTokenTag.DEFAULT)
		case FINALLY extends FTokenKind("finally", FTokenTag.DEFAULT)
		case FOR extends FTokenKind("for", FTokenTag.DEFAULT)
		case FORSOME extends FTokenKind("forSome", FTokenTag.DEFAULT)
		case IF extends FTokenKind("if", FTokenTag.DEFAULT)
		case IMPLICIT extends FTokenKind("implicit", FTokenTag.DEFAULT)
		case IMPORT extends FTokenKind("import", FTokenTag.DEFAULT)
		case LAZY extends FTokenKind("lazy", FTokenTag.DEFAULT)
		case MACRO extends FTokenKind("macro", FTokenTag.DEFAULT)
		case MATCH extends FTokenKind("match", FTokenTag.DEFAULT)
		case NEW extends FTokenKind("new", FTokenTag.DEFAULT)
		case NULL extends FTokenKind("null", FTokenTag.NAMED)
		case OBJECT extends FTokenKind("object", FTokenTag.DEFAULT)
		case OVERRIDE extends FTokenKind("override", FTokenTag.DEFAULT)
		case PACKAGE extends FTokenKind("package", FTokenTag.DEFAULT)
		case PRIVATE extends FTokenKind("private", FTokenTag.DEFAULT)
		case PROTECTED extends FTokenKind("protected", FTokenTag.DEFAULT)
		case RETURN extends FTokenKind("return", FTokenTag.DEFAULT)
		case SEALED extends FTokenKind("sealed", FTokenTag.DEFAULT)
		case SUPER extends FTokenKind("super", FTokenTag.DEFAULT)
		case THIS extends FTokenKind("this", FTokenTag.DEFAULT)
		case THROW extends FTokenKind("throw", FTokenTag.DEFAULT)
		case TRAIT extends FTokenKind("trait", FTokenTag.DEFAULT)
		case TRY extends FTokenKind("try", FTokenTag.DEFAULT)
		case TRUE extends FTokenKind("true", FTokenTag.NAMED)
		case TYPE extends FTokenKind("type", FTokenTag.DEFAULT)
		case VAL extends FTokenKind("val", FTokenTag.DEFAULT)
		case VAR extends FTokenKind("var", FTokenTag.DEFAULT)
		case WHILE extends FTokenKind("while", FTokenTag.DEFAULT)
		case WITH extends FTokenKind("with", FTokenTag.DEFAULT)
		case YIELD extends FTokenKind("yield", FTokenTag.DEFAULT)

		case LONGLITERAL extends FTokenKind(null, FTokenTag.NUMERIC)
		case FLOATLITERAL extends FTokenKind(null, FTokenTag.NUMERIC)
		case INTLITERAL extends FTokenKind(null, FTokenTag.NUMERIC)
		case DOUBLELITERAL extends FTokenKind(null, FTokenTag.NUMERIC)
		case CHARLITERAL extends FTokenKind(null, FTokenTag.NUMERIC)
		case STRINGLITERAL extends FTokenKind(null, FTokenTag.STRING)
		case BOOLEANLITERAL extends FTokenKind(null, FTokenTag.NAMED)

		case ID extends FTokenKind(null, FTokenTag.NAMED)

		case EOF extends FTokenKind(null, FTokenTag.DEFAULT)
		case ERROR extends FTokenKind(null, FTokenTag.DEFAULT)

		case PUBLIC extends FTokenKind("public", FTokenTag.DEFAULT)
		case SYNCHRONIZED extends FTokenKind("synchronized", FTokenTag.DEFAULT)

		case THROWS extends FTokenKind("throws", FTokenTag.DEFAULT)
		case TRANSIENT extends FTokenKind("transient", FTokenTag.DEFAULT)
		case VOLATILE extends FTokenKind("volatile", FTokenTag.DEFAULT)

		case AT extends FTokenKind("@", FTokenTag.DEFAULT)
		case FAT_ARROW extends FTokenKind("=>", FTokenTag.DEFAULT)
		case LEFT_ARROW extends FTokenKind("<-", FTokenTag.DEFAULT)
		case RIGHT_ARROW extends FTokenKind("->", FTokenTag.DEFAULT)
		case UNDERSCORE extends FTokenKind("_", FTokenTag.NAMED)
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
		case PIPE extends FTokenKind("|", FTokenTag.DEFAULT)
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
		case COLONCOLON extends FTokenKind("::", FTokenTag.DEFAULT)
		case COLON extends FTokenKind(":", FTokenTag.DEFAULT)
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

import com.fdk.compiler.parser.FToken.FTokenKind

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

