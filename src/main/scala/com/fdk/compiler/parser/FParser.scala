package com.fdk.compiler.parser

import com.fdk.compiler.parser.FParser.assrt
import com.fdk.compiler.parser.FToken.FTokenKind
import com.fdk.compiler.parser.FToken.FTokenKind.*
import com.fdk.compiler.tree.{FAccessQualifier, FClassDef, FNon, FObjectDef, FStableId, FTraitDef, FTree}

object FParser {
	def assrt(cond: Boolean, msg: String = ""): Unit = if (!cond) throw new AssertionError(msg)

	def apply(lexer: IFLexer): FParser = new FParser(lexer)
}


class FParser(lexer: IFLexer) {
	private[this] var token: FToken = lexer.nextToken()

	def next(): Unit = {
		token = lexer.nextToken()
		println(s"Next=[${token}]")
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

	def _package(): FTree = {
		if(isToken(PACKAGE)){
			next()
			qualId()
			return FTree()
		}
		FNon
	}

	def _import(): FTree = {
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
			return FTree()
		}
		FNon
	}


	def annotType(): Unit = {
		simpleType()
		while (token.kind == AT) {
			next()
			simpleType()
		}
	}

	def refineStat(): FTree = {
		if (dcl()) {

		} else {
			return FNon
		}
		FTree()
	}

	def refinement(): FTree = {
		if (isToken(LCURL)) {
			refineStat()
			accept(RCURL)
			return FTree()
		}
		FNon
	}

	def compoundType(): Unit = {
		annotType()
		while (token.kind == WITH) {
			next()
			annotType()
		}
	}

	def infixType(): FTree = {
		compoundType()
		while (token.kind == ID) {
			next()
			compoundType()
		}
		FTree()
	}

	def simpleType(): FTree = {
		var t = FNon
		if (isToken(LPAREN)) {
			next()
			types()
			accept(RPAREN)
			simpleTypeRest()
			return FSimpleType()
			
		} else if (stableId()) {
			if (isTokenPrefix(DOT, TYPE)) {
				skip(2)
			}
			simpleTypeRest()
			return FTree()
		}
		t
	}

	def simpleTypeRest(): FTree = {
		if (typeArgs()) {
			simpleTypeRest()
		} else if (isTokenPrefix(POUND, ID)) {
			skip(2)
			simpleTypeRest()
		}
		FTree()
	}

	def _type(): FTree = {
		if (functionArgTypes()) {
			if (isToken(FAT_ARROW)) {
				next()
				assrt(_type())
			}
			return FTree()
		} //else if (infixType()) { //	existentialClause? //}
		FNon
	}

	def typeArgs(): FTree = {
		if (isToken(LBRACKET)) {
			next()
			types()
			accept(RBRACKET)
			return FTree()
		}
		FNon
	}

	def functionArgTypes(): FTree = {
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
			return FTree()
		} else if (simpleType()) {
			return FTree()
		}
		FNon
	}

	def paramType(): FTree = {
		if (isToken(FAT_ARROW)) {
			next()
			assrt(_type())
		} else {
			assrt(_type())
			if (isToken(ID)) next()//STAR
		}
		FTree()
	}

	def classParam(): FTree = {
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
			return FTree()
		}
		FNon
	}

	def typeParamClause(): FTree = {
		if (isToken(LBRACKET)) {
			variantTypeParam()
			while (token.kind == COMMA) {
				next()
				variantTypeParam()
			}
			accept(RBRACKET)
			return FTree()
		}
		FNon
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

	def stableId(): FTree = {
		var t = FNon
		if (isToken(ID)) {
			t = FStableId()
			ident()
			if (token.kind == DOT) {
				if (isTokenLaOneOf(1, THIS, SUPER)) {
					skip(2)
					stableId2()
				}
			}
			stableIdRest()
		} else if (isTokenOneOf(THIS, SUPER)) {
			t = FStableId()
			next()
			stableId2()
			stableIdRest()
		} 
		t
	}

	def variantTypeParam(): Unit = {
		if (isToken(ID)) { //OneOf(PLUS, SUB))
			next()
		}
		typeParam()
	}

	def pattern2(): FTree = {
		if (isToken(ID) && isTokenLaOneOf(1, AT)) {
			ident()
			next()
			pattern3()
			return FTree()
		}
		pattern3()
	}

	def pattern1(): FTree = {
		if (isTokenOneOf(UNDERSCORE, ID) && isTokenLaOneOf(1, COLON)) {
			skip(2)
			assrt(_type())
		} else if (pattern2()) {
		} else {
			return FNon
		}
		FTree()
	}

	def pattern(): FTree = {
		if (pattern1()) {
			while (isToken(ID)) {//PIPE
				next()
				pattern1()
			}
		} else {
			return FNon
		}
		FTree()
	}

	def patterns(): Unit = {
		if (token.kind == UNDERSCORE) {
			next()
			accept(ID) //STAR
		}
		else {
			pattern()
			while (token.kind == COMMA) {
				next()
				patterns()
			}
		}
	}

	def simplePatternRest(): FTree = {
		if (isToken(LPAREN)) {
			next()
			if (token.kind != RPAREN) {
				patterns()
			}
			accept(RPAREN)
			return FTree()
		}
		FNon
	}

	def simplePattern(): FTree = {
		if (isToken(UNDERSCORE) || literal()) {
			next()
			return FTree()
		}
		if (stableId()) {
			simplePatternRest()
			return FTree()
		}
		simplePatternRest()
	}

	def guard(): FTree = {
		if (isToken(IF)) {
			next()
			postfixExpr()
			return FTree()
		}
		FNon
	}

	def generatorRest(): FTree = {
		if (guard()) {
		} else if (pattern1()) {
			accept(EQ)
			expr()
		} else {
			return FNon
		}
		FTree()
	}

	def generator(): FTree = {
		if (pattern1()) {
			accept(LEFT_ARROW)
			expr()
			while (generatorRest()) {}
		} else {
			return FNon
		}
		FTree()
	}

	def enumerators(): FTree = {
		if (generator()) {
			while (generator()) {}
		} else {
			return FNon
		}
		FTree()
	}


	def simpleExpr(): FTree = {
		if (isToken(NEW)) {
			next()
			if (templateBody() || classTemplate()) return FTree()
		} else if (blockExpr()) {
			return FTree()
		}
		FNon
	}

	def caseClauses(): FTree = {
		if (caseClause()) {
			while (caseClause()) {}
			return FTree()
		}
		FNon
	}

	def blockExpr(): FTree = {
		if (isToken(LCURL)) {
			next()
			if (caseClauses()) {
			} else {
				assrt(block())
			}
			accept(RCURL)
			return FTree()
		}
		FNon
	}

	def simpleExpr1(): FTree = {
		if (literal() || stableId()){
			simpleExpr1Rest()
			return FTree()
		}
		if(isToken(UNDERSCORE)){
			next()
			simpleExpr1Rest()
			return FTree()
		}
		if(isToken(LPAREN)){
			next()
			if(!isToken(RPAREN)){
				exprs()
			}
			accept(RPAREN)
			simpleExpr1Rest()
			return FTree()
		}
		if(simpleExpr()){
			if(isToken(DOT)){
				next()
				ident()
				simpleExpr1Rest()
				return FTree()
			}
			assrt(typeArgs())
			simpleExpr1Rest()
			return FTree()
		}
		FNon
	}
	
	def simpleExpr1Rest(): FTree = {
		if(isTokenPrefix(UNDERSCORE, DOT)){
			skip(2)
			ident()
			simpleExpr1Rest()
			return FTree()
		}
		if(isTokenPrefix(UNDERSCORE, LBRACKET)){
			next()
			typeArgs()
			simpleExpr1Rest()
			return FTree()
		}
		if(isToken(DOT)) {
			next()
			ident()
			simpleExpr1Rest()
			return FTree()
		}
		if(isToken(LBRACKET)){
			typeArgs()
			simpleExpr1Rest()
			return FTree()
		} 
		if(argumentExprs()){
			simpleExpr1Rest()
			return FTree()
		} 
		FNon
	}

	def simpleExpr1Rest_OLD(): FTree = {
		if (isToken(UNDERSCORE)) {
			next()
			simpleExpr1Rest2()
			return FTree()
		} 
		if(simpleExpr1Rest2()){
			return FTree()
		}
		argumentExprs()
	}

	def simpleExpr1Rest2(): FTree = {
		if (isToken(DOT)) {
			next()
			ident()
			return FTree()
		}
		if(typeArgs()){
			return FTree()
		}
		FNon
	}

	def prefixExpr(): FTree = {
		if (isToken(ID)) {//SUB, PLUS, BANG, TILDE
			next()
		}
		if (simpleExpr()) {
			return FTree()
		} else if (simpleExpr1()) {
			if (isToken(UNDERSCORE)) {
				next()
			}
			return FTree()
		}
		FNon
	}

	def infixExpr(): FTree = {
		if (prefixExpr()) {
			if (isToken(ID)) {
				while ( {
					next()
					prefixExpr()
					isToken(ID)
				}) {}
			}
			return FTree()
		}
		FNon
	}

	def prefixDef(): FTree = {
		if (isToken(ID)) {//SUB, PLUS, TILDE, BANG
			next()
			return FTree()
		}
		FNon
	}

	def postfixExpr(): FTree = {
		if (infixExpr()) {
			if (isToken(ID)) {
				ident()
			}
			while (prefixDef()) {
				simpleExpr1()
			}
			return FTree()
		}
		FNon
	}


	def caseClause(): FTree = {
		if (isToken(CASE)) {
			next()
			pattern()
			guard()
			accept(FAT_ARROW)
			assrt(block())
			return FTree()
		}
		FNon
	}

	def expr1(): FTree = {
		if (isToken(IF)) {
			accept(LPAREN)
			expr()
			accept(RCURL)
			expr()
			if (token.kind == ELSE) {
				next()
				expr()
			}
			return FTree()
		}

		if (isToken(WHILE)) {
			accept(LPAREN)
			expr()
			accept(RCURL)
			expr()
			return FTree()
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
			return FTree()
		}

		if (isToken(DO)) {
			expr()
			accept(WHILE)
			accept(LPAREN)
			expr()
			accept(RPAREN)
			return FTree()
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
			return FTree()
		}

		if (isToken(THROW)) {
			assrt(expr())
			return FTree()
		}

		if (isToken(RETURN)) {
			expr()
			return FTree()
		}

		if (simpleExpr()) {
			ident()
			accept(EQ)
			expr()
			return FTree()
		}

		if (simpleExpr1()) {
			var suffix = false
			if(isToken(UNDERSCORE)){
				next()
				suffix = true
			}
			if (isToken(DOT)) {
				next()
				suffix = true
			}
			if (isToken(ID)) {
				ident()
				accept(EQ)
				expr()
				return FTree()
			}

			if (!suffix && argumentExprs()) {
				accept(EQ)
				expr()
				return FTree()
			}
			return FTree()
		}

		if (postfixExpr()) {
			if (isToken(MATCH)) {
				next()
				accept(LCURL)
				caseClauses()
				accept(RCURL)
				return FTree()
			}

			if (ascription()) {
				return FTree()
			}
		}
		FNon
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
	//			return FTree()
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
	//				return FTree()
	//			}
	//
	//			if(ascription()){
	//				return FTree()
	//			}
	//
	//			if (argumentExprs()) {
	//				accept(EQ)
	//				expr()
	//				return FTree()
	//			}
	//
	//			if(isToken(ID)){
	//				ident()
	//				accept(EQ)
	//				expr()
	//				return FTree()
	//			}
	//
	//			if(isToken(EQ)){
	//				next()
	//				expr()
	//				return FTree()
	//			}
	//
	//			return FTree()
	//		}

	def annotations(): FTree = {
		if (isToken(AT)) {
			while ( {
				next()
				simpleType()
				argumentExprs()
				isToken(AT)
			}) {}
			return FTree()
		}
		FNon
	}

	def ascription(): FTree = {
		if (isToken(COLON)) {
			if (isTokenPrefix(UNDERSCORE, ID)) {// STAR
				skip(2)
				return FTree()
			} else if (annotations() || infixType()) {
				return FTree()
			}
		}
		FNon
	}

	def args(): FTree = {
		var isExpr = false
		if (exprs()) {
			isExpr = true
			while (isToken(COMMA)) {
				next()
				exprs()
			}
		} 
			
		if (postfixExpr()) {
			if (isTokenOneOf(COLON, UNDERSCORE, ID)) {//STAR
				next()
			}
			return FTree()
		}
		isExpr
	}

	def bindings(): FTree = {
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
			return FTree()
		}
		FNon
	}

	def exprs(): FTree = {
		assrt(expr())
		while (isToken(COMMA)) {
			next()
			assrt(expr())
		}
		FTree()
	}

	def expr(): FTree = {
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

	def resultExprRest(): FTree = {
		accept(FAT_ARROW)
		block()
		FTree()
	}

	def resultExpr(): FTree = {
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
			return FNon
		}
		FTree()
	}

	def argumentExprs(): FTree = {
		if (blockExpr()) {
			return FTree()
		}
		if (isTokenOneOf(LPAREN, LCURL)) {
			val t = if (token.kind == LPAREN) RPAREN else RCURL
			next()
			args()
			accept(t)
			return FTree()
		}

		FNon
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

	def pattern3(): FTree = {
		assrt(simplePattern())
		while (isToken(ID)) {
			ident()
			assert(simplePattern())
		}
		FTree()
	}


	def patDef(): FTree = {
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
			return FNon
		}
		FTree()
	}

	def ids(): FTree = {
		if (isToken(ID)) {
			ident()
			while (isToken(COMMA)) {
				next()
				ident()
			}
		} else {
			return FNon
		}
		FTree()
	}

	def varDef(): FTree = {
		if (patDef()) {
		} else if (ids()) {
			accept(COLON)
			assrt(_type())
			accept(EQ)
			accept(UNDERSCORE)
		} else {
			return FNon
		}
		FTree()
	}

	def patVarDef(): FTree = {
		if (isToken(VAL)) {
			next()
			return patDef()
		} else if (isToken(VAR)) {
			next()
			return varDef()
		}
		FNon
	}

	def earlyDefs(): Unit = {
		accept(LCURL)
		modifiers()
		patVarDef()
		accept(RCURL)
		accept(WITH)
	}

	def selfInvocation(): FTree = {
		if (isToken(THIS)) {
			next()
			argumentExprs()
			return FTree()
		}
		FNon
	}

	def constrBlock(): FTree = {
		if (isToken(LCURL)) {
			selfInvocation()
			while (blockStat()) {}
			accept(RCURL)
			return FTree()
		}
		FNon
	}

	def constrExpr(): FTree = {
		if (selfInvocation()) {
		} else if (constrBlock()) {
		} else {
			return FNon
		}
		FTree()
	}

	def defDcl(): FTree = {
		if (isToken(VAL)) {
			if (valDcl() || patVarDef()) {
				return FTree()
			}
		} else if (isToken(VAR)) {
			if (varDcl() || patVarDef()) {
				return FTree()
			}
		} else if (isToken(DEF)) {
			if (funDclDef()) {
				return FTree()
			}
		} else if (isToken(TYPE)) {
			if (typeDclDef()) {
				return FTree()
			}
		}
		FNon
	}

	def typeDclDef(): FTree = {
		if (isToken(TYPE)) {
			next()
			ident()
			typeParamClause()
			if (isToken(EQ)) {
				next()
				assrt(_type())
				return FTree()
			}
			if (isToken(LOWER_BOUND)) {
				next()
				assrt(_type())
			}
			if (isToken(UPPER_BOUND)) {
				next()
				assrt(_type())
			}
			return FTree()
		}
		FNon
	}

	def varDcl(): FTree = {
		if (isToken(VAR) && isTokenLaOneOf(1, ID)) {
			ids()
			accept(COLON)
			assrt(_type())
			return FTree()
		}
		FNon
	}

	def valDcl(): FTree = {
		if (isToken(VAL) && isTokenLaOneOf(1, ID)) {
			ids()
			accept(COLON)
			assrt(_type())
			return FTree()
		}
		FNon
	}

	def funTypeParamClause(): FTree = {
		if (isToken(LBRACKET)) {
			next()
			typeParam()
			while (isToken(COMMA)) {
				next()
				typeParam()
			}
			accept(RBRACKET)
			return FTree()
		}
		FNon
	}

	def param(): FTree = {
		if (isTokenOneOf(ID)) {
			ident()
			if (isToken(COLON)) {
				next()
				paramType()
			} else if (isToken(EQ)) {
				next()
				expr()
			}
			return FTree()
		}
		FNon
	}

	def params(): FTree = {
		if (param()) {
			while (isToken(COMMA)) {
				next()
				param()
			}
			return FTree()
		}
		FNon
	}

	def paramClausesRest(): FTree = {
		if (isToken(LPAREN) && isTokenLaOneOf(1, IMPLICIT)) {
			skip(2)
			params()
			accept(RPAREN)
			return FTree()
		}
		FNon
	}

	def paramClause(): FTree = {
		if (isToken(LPAREN)) {
			next()
			params()
			accept(RPAREN)
			return FTree()
		}
		FNon
	}

	def paramClauses(): FTree = {
		if (paramClausesRest()) {
			return FTree()
		}
		else if (paramClause()) {
			paramClausesRest()
			return FTree()
		}
		FNon
	}

	def funSig(): FTree = {
		if (isToken(ID)) {
			ident()
			funTypeParamClause()
			paramClauses()
			return FTree()
		}
		FNon
	}

	def funDclDef(): FTree = {
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
				return FTree()
			} else if (isToken(THIS)) {
				next()
				paramClause()
				paramClauses()
				if (isToken(EQ)) {
					next()
					constrExpr()
				} else if (constrBlock()) {
				}
				return FTree()
			}
		}
		FNon
	}

	def dcl(): FTree = {
		if (isToken(VAL)) {
			return valDcl()
		} else if (isToken(VAR)) {
			return varDcl()
		} else if (isToken(DEF)) {
			return funDclDef()
		} else if (isToken(TYPE)) {
			return typeDclDef()
		}
		FNon
	}

	def _def(): FTree = {
		if (patVarDef()) {
			return FTree()
		}

		if (funDclDef()) {
			return FTree()
		}

		if (typeDclDef()) {
			return FTree()
		}

		if (tmplDef()) {
			return FTree()
		}
		FNon
	}

	def block(): FTree = {
		assrt(blockStat())
		while (blockStat()) {}
		resultExpr()
		FTree()
	}

	def modifiers(): FTree = {
		var isModifier: FTree = FNon
		while (modifier()) {
			isModifier = true
		}
		isModifier
	}

	def modifier(): FTree = {
		if (localModifier() || accessModifier()) {
			return FTree()
		}
		if (isToken(OVERRIDE)) {
			next()
			return FTree()
		}
		FNon
	}

	def localModifier(): FTree = {
		if (isTokenOneOf(ABSTRACT, FINAL, SEALED, IMPLICIT, LAZY)) {
			next()
			return FTree()
		}
		FNon
	}

	def accessModifier(): FTree = {
		if (isTokenOneOf(PRIVATE, PROTECTED)) {
			next()
			accessQualifier()
			return FTree()
		}
		FNon
	}

	def blockStat(): FTree = {
		if (_import()) {
			return FTree()
		}

		if (localModifier()) {
			tmplDef()
			return FTree()
		}

		if (isTokenOneOf(IMPLICIT, LAZY)) {
			next()
			_def()
			return FTree()
		}

		if (_def()) {
			return FTree()
		}

		if (expr1()) {
			return FTree()
		}
		FNon
	}

	def templateStat(): FTree = {
		if (_import()) {
		} else if (modifiers()) {
			return defDcl()
		}
		if (defDcl()) {
		} else if (expr()) {
		} else {
			return FNon
		}
		FTree()
	}

	def literal(): FTree = {
		if (isToken(ID) && isTokenLaOneOf(1, INTLITERAL, FLOATLITERAL)) {
			skip(2)
			return FTree()
		} else if (isTokenOneOf(INTLITERAL, FLOATLITERAL, STRINGLITERAL, CHARLITERAL, BOOLEANLITERAL, NULL)) {
			next()
			return FTree()
		}
		FNon
	}

	def selfType(): FTree = {
		if (isToken(ID)) {
			if (isToken(COLON)) {
				next()
				assrt(_type())
			}
		} else if (isToken(THIS)) {
			accept(COLON)
			assrt(_type())
		} else {
			return FNon
		}
		accept(FAT_ARROW)
		FTree()
	}

	def classParamClause(): FTree = {
		if (isToken(LPAREN)) {
			next()
			classParams()
			accept(RPAREN)
			return FTree()
		}
		FNon
	}

	def classParams(): FTree = {
		if (classParam()) {
			while (isToken(COMMA)) {
				next()
				classParam()
			}
			return FTree()
		}
		FNon
	}

	def classParamClausesRest(): FTree = {
		if (isTokenPrefix(LPAREN, IMPLICIT)) {
			skip(2)
			classParams()
			return FTree()
		}
		FNon
	}

	def classParamClauses(): FTree = {
		if (classParamClausesRest()) {
			return FTree()
		}
		if (classParamClause()) {
			while (classParamClause()) {}
			classParamClausesRest()
			return FTree()
		}
		FNon
	}

	def classTemplate(): FTree = {
		//earlyDefs?
		classParents()
		templateBody()
		FTree()
	}

	def templateBody(): FTree = {
		if (isToken(LCURL)) {
			next()
			selfType()
			while (!isToken(RCURL)) {
				templateStat() //+
			}
			accept(RCURL)
			return FTree()
		}
		FNon
	}

	def classTemplateOpt(): FTree = {
		//Not completely correct according to the grammar
		if (isToken(EXTENDS)) {
			next()
			//earlyDefs?
			classParents()
			templateBody()
			return FTree()
		} else if (templateBody()) {
			return FTree()
		}
		FNon
	}

	def traitTemplateOpt(): FTree = {
		if (isToken(EXTENDS)) {
			next()
			classParents()
		} 
		templateBody()
	}

	def classDef(isCase: Boolean): FTree = {
		if (isToken(CLASS)) {
			next()
			ident()
			typeParamClause()
			accessModifier()
			classParamClauses()
			classTemplateOpt()
			return FClassDef()
		}
		FNon
	}

	def objectDef(isCase: Boolean): FTree = {
		if (isToken(OBJECT)) {
			next()
			ident()
			classTemplateOpt()
			return FObjectDef()
		}
		FNon
	}

	def traitDef(): FTree = {
		if (isToken(TRAIT)) {
			next()
			ident()
			typeParamClause()
			traitTemplateOpt()
			return FTraitDef()
		}
		FNon
	}

	def accessQualifier(): FTree = {
		if (isToken(LBRACKET)) {
			next()
			acceptOneOf(ID, THIS)
			accept(RBRACKET)
			return FAccessQualifier()
		}
		FNon
	}

	def tmplDef(): FTree = {
		var isCase = false
		if (isToken(CASE)) {
			if (!isTokenLaOneOf(1, CLASS, OBJECT)) {
				return FNon
			}
			isCase = true
			next()
		}

		var t = FNon
		if ((t != traitDef())!= FNon) {
			return t
		}

		if ((t=objectDef(isCase))!= FNon) {
			return t
		}

		if ((t=classDef(isCase))!= FNon) {
			return t
		}

		FNon
	}

	def topStatement(): FTree = {
		val t = _import()
		if (t != FNon) {
			return t
		}
		modifiers()
		tmplDef()
	}

	def topStatements(): ArrayBuffer[FTree] = {
		val defs = ArrayBuffer[FTree]()
		while (token.kind != EOF) {
			defs.append(topStatement())
		}
		defs
	}

	def compilationUnit(): Unit = {
		val defs = ArrayBuffer[FTree]()
		while ({
			val t = _package()
			t != FNon
		}) {
			defs.append(t)
		}
		defs.appendAll(topStatements())
	}
}

