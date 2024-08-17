package com.fdk.compiler.parser

import com.fdk.compiler.parser.FParser.assrt
import com.fdk.compiler.parser.FToken.FTokenKind
import com.fdk.compiler.parser.FToken.FTokenKind.*

object FParser {
	def assrt(cond: Boolean, msg: String = ""): Unit = if (!cond) throw new AssertionError(msg)

	def apply(lexer: IFLexer): FParser = new FParser(lexer)
}


class FParser(lexer: IFLexer) {
	private[this] var token: FToken = lexer.nextToken()

	def next(): Unit = {
		val prev = token
		token = lexer.nextToken()
		println(s"Next=[${token}]")
		if(prev.kind == token.kind) {
			println(s"Next & prev tokens are same")
		}
	}

	def pushState(): Unit = {
		lexer.pushState()
	}

	def popState(stateId: Int, discard: Boolean): Unit = {
		lexer.popState(stateId, discard)
	}

	def skip(n: Int): Unit = {
		if (n == 1) next()
		else if (n > 1) token = lexer.skip(n)
		else throw new IllegalArgumentException("n must be positive")
	}

	def slide(kind: FTokenKind): Int = {
		var n = 0
		while (token.kind == kind) {
			n += 1
			next()
		}
		n
	}

	def acceptCount(kind: FTokenKind, count: Int): Unit = {
		for (i <- 0 until count) {
			if (token.kind == kind) next()
			else {
				setErrorEndPos(token.pos)
				reportSyntaxError(token.pos, "expected", kind)
			}
		}
	}

	def lookAhead(n: Int): FToken = {
		if (n == 0) token
		else if (n > 0) lexer.lookAhead(n)
		else throw new IllegalArgumentException("n must be positive")
	}

	/** If next input token matches given token, skip it, otherwise report
	 * an error.
	 */
	def accept(kind: FTokenKind): Unit = {
		if (token.kind == kind) next()
		else {
			throw new IllegalArgumentException(s"Expected token $kind, but got ${token.kind}")
		}
	}

	def acceptOneOf(kinds: FTokenKind*): Unit = {
		if (kinds.contains(token.kind)) next()
		else {
			setErrorEndPos(token.pos)
			reportSyntaxError(token.pos, "expected", kinds: _*)
		}
	}


	def isTokenLaOneOf(n: Int, kinds: FTokenKind*): Boolean = {
		kinds.contains(lookAhead(n).kind)
	}

	def isTokenPrefix(prefix: FTokenKind*): Boolean = {
		for (i <- 0 until prefix.length) {
			val t = lookAhead(i)
			if (t.kind != prefix(i)) {
				return false
			}
		}
		true
	}

	def isToken(kind: FTokenKind): Boolean = {
		token.kind == kind
	}


	def isTokenOneOf(kinds: FTokenKind*): Boolean = {
		kinds.contains(token.kind)
	}

	def setErrorEndPos(errPos: Int): Unit = {
		//endPosTable.setErrorEndPos(errPos)
	}

	def reportSyntaxError(pos: Int, key: String, kinds: FTokenKind*): Unit = {
		//reporter.syntaxError(token.offset, msg)
	}

	def ident(): Unit = {
		accept(ID)
	}

	def qualId(): Unit = {
		ident()
		while (token.kind == DOT) {
			next()
			ident()
		}
	}

	def _package(): Unit = {
		accept(PACKAGE)
		qualId()
	}

	def _import(): Boolean = {
		if (isToken(IMPORT)) {
			next()
			stableId()
			if (isToken(DOT)) {
				token.kind match
					case ID | UNDERSCORE => {
						next()
					}
					case LCURL => {
						next()
						if (isToken(ID)) {
							ident()
							if (isToken(FAT_ARROW)) {
								next()
								acceptOneOf(ID, UNDERSCORE)
							}
							while (isToken(COMMA)) {
								next()
								ident()
								if (isToken(FAT_ARROW)) {
									next()
									acceptOneOf(ID, UNDERSCORE)
								}
							}
						} else if (isToken(UNDERSCORE)) {
							next()
						} else {
							reportSyntaxError(token.pos, "expected", ID, UNDERSCORE)
						}

						accept(RCURL)
					}
			}
			return true
		}
		false
	}


	def annotType(): Unit = {
		simpleType()
		while (token.kind == AT) {
			next()
			simpleType()
		}
	}

	def refineStat(): Boolean = {
		if (dcl()) {

		} else {
			return false
		}
		true
	}

	def refinement(): Boolean = {
		if (isToken(LCURL)) {
			refineStat()
			accept(RCURL)
			return true
		}
		false
	}

	def compoundType(): Unit = {
		annotType()
		while (token.kind == WITH) {
			next()
			annotType()
		}
	}

	def infixType(): Boolean = {
		compoundType()
		while (token.kind == ID) {
			next()
			compoundType()
		}
		true
	}

	def simpleType(): Boolean = {
		if (isToken(LPAREN)) {
			next()
			types()
			accept(RPAREN)
			simpleTypeRest()
			return true
		} else if (stableId()) {
			if (isTokenPrefix(DOT, TYPE)) {
				skip(2)
			}
			simpleTypeRest()
			return true
		}
		false
	}

	def simpleTypeRest(): Boolean = {
		if (typeArgs()) {
			simpleTypeRest()
		} else if (isTokenPrefix(POUND, ID)) {
			skip(2)
			simpleTypeRest()
		}
		true
	}

	def _type(): Boolean = {
		if (functionArgTypes()) {
			if (isToken(FAT_ARROW)) {
				next()
				assrt(_type())
			}
			return true
		} //else if (infixType()) { //	existentialClause? //}
		false
	}

	def typeArgs(): Boolean = {
		if (isToken(LBRACKET)) {
			next()
			types()
			accept(RBRACKET)
			return true
		}
		false
	}

	def functionArgTypes(): Boolean = {
		if (isToken(LPAREN)) {
			next()
			if (!isToken(RPAREN)) {
				paramType()
				while (isToken(COMMA)) {
					next()
					paramType()
				}
			}
			accept(RPAREN)
			return true
		} else if (simpleType()) {
			return true
		}
		false
	}

	def paramType(): Boolean = {
		if (isToken(FAT_ARROW)) {
			next()
			assrt(_type())
		} else {
			assrt(_type())
			if (isToken(STAR)) next()
		}
		true
	}

	def classParam(): Boolean = {
		modifiers()
		if (isTokenOneOf(VAR, VAL) && isTokenLaOneOf(1, ID)) {
			next()
			ident()
			accept(COLON)
			paramType()
			if (token.kind == EQ) {
				next()
				expr()
			}
			return true
		}
		false
	}

	def typeParamClause(): Boolean = {
		if (isToken(LBRACKET)) {
			variantTypeParam()
			while (token.kind == COMMA) {
				next()
				variantTypeParam()
			}
			accept(RBRACKET)
			return true
		}
		false
	}

	def typeParam(): Unit = {
		if (isTokenOneOf(ID, UNDERSCORE)) {
			next()
		}
		if (isToken(LBRACKET)) {
			typeParamClause()
		}

		if (isToken(LOWER_BOUND)) {
			next()
			assrt(_type())
		}
		if (isToken(UPPER_BOUND)) {
			next()
			assrt(_type())
		}
		if (isToken(COLON)) {
			next() //Context bound
			assrt(_type())
		}
	}

	def types(): Unit = {
		assrt(_type())
		while (token.kind == COMMA) {
			next()
			assrt(_type())
		}
	}

	def classQualifier(): Unit = {
		accept(LBRACKET)
		ident()
		accept(RBRACKET)
	}

	def stableId2(): Unit = {
		if (token.kind == LBRACKET) {
			classQualifier()
		}
		accept(DOT)
		accept(ID)
	}

	def stableIdRest(): Unit = {
		while (isTokenPrefix(DOT, ID)) {
			next()
			accept(ID)
		}
	}

	def stableId(): Boolean = {
		if (isToken(ID)) {
			ident()
			if (token.kind == DOT) {
				if (isTokenLaOneOf(1, THIS, SUPER)) {
					skip(2)
					stableId2()
				}
			}
			stableIdRest()
		} else if (isTokenOneOf(THIS, SUPER)) {
			next()
			stableId2()
			stableIdRest()
		} else {
			return false
		}
		true
	}

	def variantTypeParam(): Unit = {
		if (isTokenOneOf(PLUS, SUB)) {
			next()
		}
		typeParam()
	}

	def pattern2(): Boolean = {
		if (isToken(ID) && isTokenLaOneOf(1, AT)) {
			ident()
			next()
			pattern3()
			return true
		}
		pattern3()
	}

	def pattern1(): Boolean = {
		if (isTokenOneOf(UNDERSCORE, ID) && isTokenLaOneOf(1, COLON)) {
			skip(2)
			assrt(_type())
		} else if (pattern2()) {
		} else {
			return false
		}
		true
	}

	def pattern(): Boolean = {
		if (pattern1()) {
			while (isToken(PIPE)) {
				next()
				pattern1()
			}
		} else {
			return false
		}
		true
	}

	def patterns(): Unit = {
		if (token.kind == UNDERSCORE) {
			next()
			accept(STAR)
		}
		else {
			pattern()
			while (token.kind == COMMA) {
				next()
				patterns()
			}
		}
	}

	def simplePatternRest(): Boolean = {
		if (isToken(LPAREN)) {
			next()
			if (token.kind != RPAREN) {
				patterns()
			}
			accept(RPAREN)
			return true
		}
		false
	}

	def simplePattern(): Boolean = {
		if (isToken(UNDERSCORE) || literal()) {
			next()
			return true
		}
		if (stableId()) {
			simplePatternRest()
			return true
		}
		simplePatternRest()
	}

	def guard(): Boolean = {
		if (isToken(IF)) {
			next()
			postfixExpr()
			return true
		}
		false
	}

	def generatorRest(): Boolean = {
		if (guard()) {
		} else if (pattern1()) {
			accept(EQ)
			expr()
		} else {
			return false
		}
		true
	}

	def generator(): Boolean = {
		if (pattern1()) {
			accept(LEFT_ARROW)
			expr()
			while (generatorRest()) {}
		} else {
			return false
		}
		true
	}

	def enumerators(): Boolean = {
		if (generator()) {
			while (generator()) {}
		} else {
			return false
		}
		true
	}


	def simpleExpr(): Boolean = {
		if (isToken(NEW)) {
			next()
			if (templateBody() || classTemplate()) return true
		} else if (blockExpr()) {
			return true
		}
		false
	}

	def caseClauses(): Boolean = {
		if (caseClause()) {
			while (caseClause()) {}
			return true
		}
		false
	}

	def blockExpr(): Boolean = {
		if (isToken(LCURL)) {
			next()
			if (caseClauses()) {
			} else {
				assrt(block())
			}
			accept(RCURL)
			return true
		}
		false
	}

	def simpleExpr1(): Boolean = {
		var loop = true
		var rv = false
		while(loop) {
			if (literal() || stableId()) {
				simpleExpr1Rest()
			} else if(isToken(UNDERSCORE)){
				next()
			} else if(isToken(LPAREN)){
				next()
				if(!isToken(RPAREN)){
					exprs()
				}
				accept(RPAREN)
			} else if(simpleExpr()){
				if(isToken(DOT)){
					next()
					ident()
				} else {
					assrt(typeArgs())
				}
			} else {
				loop = false
			}
			if(loop){
				rv = true
			}
		}
		rv
	}

	def simpleExpr1Rest(): Boolean = {
		if (isToken(UNDERSCORE)) {
			next()
			simpleExpr1Rest2()
			return true
		} 
		if(simpleExpr1Rest2()){
			return true
		}
		argumentExprs()
	}

	def simpleExpr1Rest2(): Boolean = {
		if (isToken(DOT)) {
			next()
			ident()
			return true
		}
		if(typeArgs()){
			return true
		}
		false
	}

	def prefixExpr(): Boolean = {
		if (isTokenOneOf(SUB, PLUS, BANG, TILDE)) {
			next()
		}
		if (simpleExpr()) {
			return true
		} else if (simpleExpr1()) {
			if (isToken(UNDERSCORE)) {
				next()
			}
			return true
		}
		false
	}

	def infixExpr(): Boolean = {
		if (prefixExpr()) {
			if (isToken(ID)) {
				while ( {
					next()
					prefixExpr()
					isToken(ID)
				}) {}
			}
			return true
		}
		false
	}

	def prefixDef(): Boolean = {
		if (isTokenOneOf(SUB, PLUS, TILDE, BANG)) {
			next()
			return true
		}
		false
	}

	def postfixExpr(): Boolean = {
		if (infixExpr()) {
			if (isToken(ID)) {
				ident()
			}
			while (prefixDef()) {
				simpleExpr1()
			}
			return true
		}
		false
	}


	def caseClause(): Boolean = {
		if (isToken(CASE)) {
			next()
			pattern()
			guard()
			accept(FAT_ARROW)
			assrt(block())
			return true
		}
		false
	}

	def expr1(): Boolean = {
		if (isToken(IF)) {
			accept(LPAREN)
			expr()
			accept(RCURL)
			expr()
			if (token.kind == ELSE) {
				next()
				expr()
			}
			return true
		}

		if (isToken(WHILE)) {
			accept(LPAREN)
			expr()
			accept(RCURL)
			expr()
			return true
		}

		if (isToken(TRY)) {
			expr()
			if (token.kind == CATCH) {
				next()
				expr()
			}
			if (token.kind == FINALLY) {
				next()
				expr()
			}
			return true
		}

		if (isToken(DO)) {
			expr()
			accept(WHILE)
			accept(LPAREN)
			expr()
			accept(RPAREN)
			return true
		}

		if (isToken(FOR)) {
			if (token.kind == LPAREN) {
				next()
				enumerators()
				accept(RPAREN)
			}
			else {
				accept(LCURL)
				enumerators()
				accept(RCURL)
			}
			if (token.kind == YIELD) {
				next()
			}
			expr()
			return true
		}

		if (isToken(THROW)) {
			assrt(expr())
			return true
		}

		if (isToken(RETURN)) {
			expr()
			return true
		}

		if (simpleExpr()) {
			ident()
			accept(EQ)
			expr()
			return true
		}

		if (simpleExpr1()) {
			if (isTokenPrefix(UNDERSCORE, ID)) {
				next()
				ident()
				accept(EQ)
				expr()
				return true
			}

			if (isToken(ID)) {
				ident()
				accept(EQ)
				expr()
				return true
			}

			if (argumentExprs()) {
				accept(EQ)
				expr()
				return true
			}
			return true
		}

		if (postfixExpr()) {
			if (isToken(MATCH)) {
				next()
				accept(LCURL)
				caseClauses()
				accept(RCURL)
				return true
			}

			if (ascription()) {
				return true
			}
		}
		false
	}

	//	/*
	//		SimpleExpr
	//	 */
	//		if (isTokenOneOf(NEW, LCURL)) {
	//			simpleExpr()
	//		}
	//
	//		if(isTokenPrefix(ID, EQ)){
	//			ident()
	//			accept(EQ)
	//			expr()
	//			return true
	//		}
	//
	//	/*
	//			SimpleExpr1
	//	 */
	//		if (isTokenOneOf(UNDERSCORE, LPAREN)) {
	//			simpleExpr1()
	//		}
	//
	//		/*
	//
	//		 */
	//		if(isTokenOneOf(SUB, PLUS, BANG, TILDE)){
	//			postfixExpr()
	//		}
	//
	//		throw new IllegalArgumentException(s"Unexpected Token ${token.kind}")


	//		if(postfixExpr()){
	//
	//			if (isToken(MATCH)) {
	//				next()
	//				accept(LCURL)
	//				while (token.kind != RCURL) {
	//					next()
	//					caseClause()
	//				}
	//				next()
	//				return true
	//			}
	//
	//			if(ascription()){
	//				return true
	//			}
	//
	//			if (argumentExprs()) {
	//				accept(EQ)
	//				expr()
	//				return true
	//			}
	//
	//			if(isToken(ID)){
	//				ident()
	//				accept(EQ)
	//				expr()
	//				return true
	//			}
	//
	//			if(isToken(EQ)){
	//				next()
	//				expr()
	//				return true
	//			}
	//
	//			return true
	//		}

	def annotations(): Boolean = {
		if (isToken(AT)) {
			while ( {
				next()
				simpleType()
				argumentExprs()
				isToken(AT)
			}) {}
			return true
		}
		false
	}

	def ascription(): Boolean = {
		if (isToken(COLON)) {
			if (isTokenPrefix(UNDERSCORE, STAR)) {
				skip(2)
				return true
			} else if (annotations() || infixType()) {
				return true
			}
		}
		false
	}

	def args(): Boolean = {
		var isExpr = false
		if (exprs()) {
			isExpr = true
			while (isToken(COMMA)) {
				next()
				exprs()
			}
		} 
			
		if (postfixExpr()) {
			if (isTokenOneOf(COLON, UNDERSCORE, STAR)) {
				next()
			}
			return true
		}
		isExpr
	}

	def bindings(): Boolean = {
		if (isToken(LPAREN)) {
			while ( {
				next()
				acceptOneOf(ID, UNDERSCORE)
				if (isToken(COLON)) {
					next()
					assrt(_type())
				}
				isToken(COMMA)
			}) {}
			return true
		}
		false
	}

	def exprs(): Boolean = {
		assrt(expr())
		while (isToken(COMMA)) {
			next()
			assrt(expr())
		}
		true
	}

	def expr(): Boolean = {
		if (bindings()) {
			accept(FAT_ARROW)
			return expr()
		}

		if (isToken(IMPLICIT)) {
			acceptOneOf(ID, UNDERSCORE)
			accept(FAT_ARROW)
			return expr()
		}

		if (isTokenOneOf(ID, UNDERSCORE) && isTokenLaOneOf(1, FAT_ARROW)) {
			next()
			accept(FAT_ARROW)
			return expr()
		}

		expr1()
	}

	def resultExprRest(): Boolean = {
		accept(FAT_ARROW)
		block()
		true
	}

	def resultExpr(): Boolean = {
		if (bindings()) {
			resultExprRest()
		} else if (isTokenPrefix(IMPLICIT, ID) || isToken(ID) || isToken(UNDERSCORE)) {
			if (isToken(IMPLICIT)) next()
			next()
			accept(COLON)
			compoundType()
			resultExprRest()
		} else if (expr1()) {
		} else {
			return false
		}
		true
	}

	def argumentExprs(): Boolean = {
		if (blockExpr()) {
			return true
		}
		if (isTokenOneOf(LPAREN, LCURL)) {
			val t = if (token.kind == LPAREN) RPAREN else RCURL
			next()
			args()
			accept(t)
			return true
		}

		false
	}

	def constr(): Unit = {
		simpleType()
		while (argumentExprs()) {}
	}

	def classParents(): Unit = {
		constr()
		while (isToken(WITH)) {
			next()
			simpleType()
		}
	}

	def pattern3(): Boolean = {
		simplePattern()
		while (isToken(ID)) {
			ident()
			assert(simplePattern())
		}
		true
	}


	def patDef(): Boolean = {
		if (pattern2()) {
			while (isToken(COMMA)) {
				next()
				pattern2()
			}
			if (isToken(COLON)) {
				next()
				assrt(_type())
			}
			accept(EQ)
			expr()
		} else {
			return false
		}
		true
	}

	def ids(): Boolean = {
		if (isToken(ID)) {
			ident()
			while (isToken(COMMA)) {
				next()
				ident()
			}
		} else {
			return false
		}
		true
	}

	def varDef(): Boolean = {
		if (patDef()) {
		} else if (ids()) {
			accept(COLON)
			assrt(_type())
			accept(EQ)
			accept(UNDERSCORE)
		} else {
			return false
		}
		true
	}

	def patVarDef(): Boolean = {
		if (isToken(VAL)) {
			next()
			return patDef()
		} else if (isToken(VAR)) {
			next()
			return varDef()
		}
		false
	}

	def earlyDefs(): Unit = {
		accept(LCURL)
		modifiers()
		patVarDef()
		accept(RCURL)
		accept(WITH)
	}

	def selfInvocation(): Boolean = {
		if (isToken(THIS)) {
			next()
			argumentExprs()
			return true
		}
		false
	}

	def constrBlock(): Boolean = {
		if (isToken(LCURL)) {
			selfInvocation()
			while (blockStat()) {}
			accept(RCURL)
			return true
		}
		false
	}

	def constrExpr(): Boolean = {
		if (selfInvocation()) {
		} else if (constrBlock()) {
		} else {
			return false
		}
		true
	}

	def defDcl(): Boolean = {
		if (isToken(VAL)) {
			if (valDcl() || patVarDef()) {
				return true
			}
		} else if (isToken(VAR)) {
			if (varDcl() || patVarDef()) {
				return true
			}
		} else if (isToken(DEF)) {
			if (funDclDef()) {
				return true
			}
		} else if (isToken(TYPE)) {
			if (typeDclDef()) {
				return true
			}
		}
		false
	}

	def typeDclDef(): Boolean = {
		if (isToken(TYPE)) {
			next()
			ident()
			typeParamClause()
			if (isToken(EQ)) {
				next()
				assrt(_type())
				return true
			}
			if (isToken(LOWER_BOUND)) {
				next()
				assrt(_type())
			}
			if (isToken(UPPER_BOUND)) {
				next()
				assrt(_type())
			}
			return true
		}
		false
	}

	def varDcl(): Boolean = {
		if (isToken(VAR) && isTokenLaOneOf(1, ID)) {
			ids()
			accept(COLON)
			assrt(_type())
			return true
		}
		false
	}

	def valDcl(): Boolean = {
		if (isToken(VAL) && isTokenLaOneOf(1, ID)) {
			ids()
			accept(COLON)
			assrt(_type())
			return true
		}
		false
	}

	def funTypeParamClause(): Boolean = {
		if (isToken(LBRACKET)) {
			next()
			typeParam()
			while (isToken(COMMA)) {
				next()
				typeParam()
			}
			accept(RBRACKET)
			return true
		}
		false
	}

	def param(): Boolean = {
		if (isTokenOneOf(ID)) {
			ident()
			if (isToken(COLON)) {
				next()
				paramType()
			} else if (isToken(EQ)) {
				next()
				expr()
			}
			return true
		}
		false
	}

	def params(): Boolean = {
		if (param()) {
			while (isToken(COMMA)) {
				next()
				param()
			}
			return true
		}
		false
	}

	def paramClausesRest(): Boolean = {
		if (isToken(LPAREN) && isTokenLaOneOf(1, IMPLICIT)) {
			skip(2)
			params()
			accept(RPAREN)
			return true
		}
		false
	}

	def paramClause(): Boolean = {
		if (isToken(LPAREN)) {
			next()
			params()
			accept(RPAREN)
			return true
		}
		false
	}

	def paramClauses(): Boolean = {
		if (paramClausesRest()) {
			return true
		}
		else if (paramClause()) {
			paramClausesRest()
			return true
		}
		false
	}

	def funSig(): Boolean = {
		if (isToken(ID)) {
			ident()
			funTypeParamClause()
			paramClauses()
			return true
		}
		false
	}

	def funDclDef(): Boolean = {
		if (isToken(DEF)) {
			next()
			if (funSig()) {
				if (isToken(COLON)) {
					next()
					assrt(_type())
					if (isToken(EQ)) {
						next()
						expr()
					}
				} else if (isToken(LCURL)) {
					block()
					accept(RCURL)
				} else if (isToken(EQ)) {
					next()
					expr()
				}
				return true
			} else if (isToken(THIS)) {
				next()
				paramClause()
				paramClauses()
				if (isToken(EQ)) {
					next()
					constrExpr()
				} else if (constrBlock()) {
				}
				return true
			}
		}
		false
	}

	def dcl(): Boolean = {
		if (isToken(VAL)) {
			return valDcl()
		} else if (isToken(VAR)) {
			return varDcl()
		} else if (isToken(DEF)) {
			return funDclDef()
		} else if (isToken(TYPE)) {
			return typeDclDef()
		}
		false
	}

	def _def(): Boolean = {
		if (patVarDef()) {
			return true
		}

		if (funDclDef()) {
			return true
		}

		if (typeDclDef()) {
			return true
		}

		if (tmplDef()) {
			return true
		}
		false
	}

	def block(): Boolean = {
		assrt(blockStat())
		while (blockStat()) {}
		resultExpr()
		true
	}

	def modifiers(): Boolean = {
		var isModifier: Boolean = false
		while (modifier()) {
			isModifier = true
		}
		isModifier
	}

	def modifier(): Boolean = {
		if (localModifier() || accessModifier()) {
			return true
		}
		if (isToken(OVERRIDE)) {
			next()
			return true
		}
		false
	}

	def localModifier(): Boolean = {
		if (isTokenOneOf(ABSTRACT, FINAL, SEALED, IMPLICIT, LAZY)) {
			next()
			return true
		}
		false
	}

	def accessModifier(): Boolean = {
		if (isTokenOneOf(PRIVATE, PROTECTED)) {
			next()
			accessQualifier()
			return true
		}
		false
	}

	def blockStat(): Boolean = {
		if (_import()) {
			return true
		}

		if (localModifier()) {
			tmplDef()
			return true
		}

		if (isTokenOneOf(IMPLICIT, LAZY)) {
			next()
			_def()
			return true
		}

		if (_def()) {
			return true
		}

		if (expr1()) {
			return true
		}
		false
	}

	def templateStat(): Boolean = {
		if (_import()) {
		} else if (modifiers()) {
			return defDcl()
		}
		if (defDcl()) {
		} else if (expr()) {
		} else {
			return false
		}
		true
	}

	def literal(): Boolean = {
		if (isToken(SUB) && isTokenLaOneOf(1, INTLITERAL, FLOATLITERAL)) {
			skip(2)
			return true
		} else if (isTokenOneOf(INTLITERAL, FLOATLITERAL, STRINGLITERAL, CHARLITERAL, BOOLEANLITERAL, NULL)) {
			next()
			return true
		}
		false
	}

	def selfType(): Boolean = {
		if (isToken(ID)) {
			if (isToken(COLON)) {
				next()
				assrt(_type())
			}
		} else if (isToken(THIS)) {
			accept(COLON)
			assrt(_type())
		} else {
			return false
		}
		accept(FAT_ARROW)
		true
	}

	def classParamClause(): Boolean = {
		if (isToken(LPAREN)) {
			next()
			classParams()
			accept(RPAREN)
			return true
		}
		false
	}

	def classParams(): Boolean = {
		if (classParam()) {
			while (isToken(COMMA)) {
				next()
				classParam()
			}
			return true
		}
		false
	}

	def classParamClausesRest(): Boolean = {
		if (isTokenPrefix(LPAREN, IMPLICIT)) {
			skip(2)
			classParams()
			return true
		}
		false
	}

	def classParamClauses(): Boolean = {
		if (classParamClausesRest()) {
			return true
		}
		if (classParamClause()) {
			while (classParamClause()) {}
			classParamClausesRest()
			return true
		}
		false
	}

	def classTemplate(): Boolean = {
		//earlyDefs?
		classParents()
		templateBody()
		true
	}

	def templateBody(): Boolean = {
		if (isToken(LCURL)) {
			next()
			selfType()
			while (!isToken(RCURL)) {
				templateStat() //+
			}
			accept(RCURL)
			return true
		}
		false
	}

	def classTemplateOpt(): Boolean = {
		//Not completely correct according to the grammar
		if (isToken(EXTENDS)) {
			next()
			//earlyDefs?
			classParents()
			templateBody()
			return true
		} else if (templateBody()) {
			return true
		}
		false
	}

	def traitTemplateOpt(): Boolean = {
		//Not completely correct according to the grammar
		if (isToken(EXTENDS)) {
			next()
			classParents()
			templateBody()
			return true
		} else if (templateBody()) {
			return true
		}
		false
	}

	def classDef(isCase: Boolean): Boolean = {
		if (isToken(CLASS)) {
			next()
			ident()
			typeParamClause()
			accessModifier()
			classParamClauses()
			classTemplateOpt()
			return true
		}
		false
	}

	def objectDef(isCase: Boolean): Boolean = {
		if (isToken(OBJECT)) {
			next()
			ident()
			classTemplateOpt()
			return true
		}
		false
	}

	def traitDef(): Boolean = {
		if (isToken(TRAIT)) {
			next()
			ident()
			typeParamClause()
			traitTemplateOpt()
			return true
		}
		false
	}

	def accessQualifier(): Boolean = {
		if (isToken(LBRACKET)) {
			next()
			acceptOneOf(ID, THIS)
			accept(RBRACKET)
			return true
		}
		false
	}

	def tmplDef(): Boolean = {
		var isCase = false
		if (isToken(CASE)) {
			if (!isTokenLaOneOf(1, CLASS, OBJECT)) {
				return false
			}
			isCase = true
			next()
		}

		if (traitDef()) {
			return true
		}

		if (objectDef(isCase)) {
			return true
		}

		if (classDef(isCase)) {
			return true
		}

		false
	}

	def topStatement(): Unit = {
		if (isToken(IMPORT)) _import()
		else {
			modifiers()
			tmplDef()
		}
	}

	def topStatements(): Unit = {
		while (token.kind != EOF) {
			topStatement()
		}
	}

	def compilationUnit(): Unit = {
		while (token.kind == PACKAGE) {
			_package()
		}
		topStatements()
	}
}

