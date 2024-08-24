package com.fdk.compiler.parser

import com.fdk.compiler.parser.FParser.assrt
import com.fdk.compiler.parser.FToken.FTokenKind
import com.fdk.compiler.parser.FToken.FTokenKind.*
import com.fdk.compiler.tree._
import scala.collection.mutable.ArrayBuffer

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
		throw new IllegalArgumentException(s"Syntax error at $pos: $key ${kinds.mkString(" or ")}")
	}

	def ident(): FTree = {
		val prev = token
		if (isToken(UNDERSCORE)) {
			return underscore()
		}
		accept(ID)
		FIdent(prev.name)
	}

	def underscore(): FTree = {
		accept(UNDERSCORE)
		FIdent("_", true)
	}

	def qualId(): FTree = {
		if (isToken(ID)) {
			val defs = ArrayBuffer[FTree]()
			defs.append(ident())
			while (isToken(DOT)) {
				next()
				defs.append(ident())
			}
			return FQualId(defs.toList)
		}
		FNon
	}

	def _package(): FTree = {
		if (isToken(PACKAGE)) {
			next()
			val qs = qualId()
			if (qs == FNon) {
				reportSyntaxError(token.pos, "expected", ID)
			}
			return FPackage(qs)
		}
		FNon
	}

	def _import(): FTree = {
		if (isToken(IMPORT)) {
			next()
			val defs = ArrayBuffer[FTree]()
			var ie = importExpr()
			while (ie != FNon) {
				defs.append(ie)
				if (isToken(COMMA)) {
					next()
					ie = importExpr()
				} else {
					ie = FNon
				}
			}
			if (defs.isEmpty) {
				reportSyntaxError(token.pos, "expected", ID, UNDERSCORE)
			}
			return FImport(defs.toList)
		}
		FNon
	}

	def importExpr(): FTree = {
		val sid = stableId()
		if (sid != FNon) {
			val defs = ArrayBuffer[FTree]()
			defs.append(sid)
			if (isToken(DOT)) {
				next()
				if (isToken(UNDERSCORE)) {
					defs.append(underscore())
				} else if (isToken(ID)) {
					defs.append(ident())
				} else {
					val t = importSelectors()
					if (t != FNon) {
						defs.append(t)
					}
				}
			}
			return FImportExpr(defs.toList)
		}
		FNon
	}

	def importSelectors(): FTree = {
		if (isToken(LCURL)) {
			next()
			if (isToken(UNDERSCORE)) {
				next()
				accept(RCURL)
				return underscore()
			}
			val defs = ArrayBuffer[FTree]()
			var is = importSelector()
			while (is != FNon) {
				defs.append(is)
				if (isToken(COMMA)) {
					next()
					is = importSelector()
				} else {
					is = FNon
				}
			}
			if (defs.isEmpty) {
				reportSyntaxError(token.pos, "expected", ID, UNDERSCORE)
			}
			accept(RCURL)
			return FImportSelectors(defs.toList)
		}
		FNon
	}

	def importSelector(): FTree = {
		if (isToken(ID)) {
			val defs = ArrayBuffer[FTree]()
			defs.append(ident())
			if (isToken(FAT_ARROW)) {
				next()
				if (isToken(UNDERSCORE)) {
					defs.append(underscore())
				} else if (isToken(ID)) {
					defs.append(ident())
				} else {
					reportSyntaxError(token.pos, "expected", ID, UNDERSCORE)
				}
			}
			return FImportSelector(defs.toList)
		}
		FNon
	}

	def annotType(): FTree = {
		val t = simpleType()
		if (t.isInstanceOf[FSimpleType]) {
			val st = t.asInstanceOf[FSimpleType]
			st.ann = annotations()
		}
		t
	}

	def refineStat(): FTree = {
		var t: FTree = dcl()
		if (t != FNon) {
			return FRefineStat()
		} else if (isToken(TYPE)) {
			next()
			t = typeDef()
			if (t != FNon) {
				return FRefineStat()
			}
		}
		FNon
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
			return FSimpleType()
		}
		FNon
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
		var t = FNon
		if ((t = functionArgTypes()) != FNon) {
			if (isToken(FAT_ARROW)) {
				next()
				val t = _type()
			}
			return FType()
		}
		FNon
	}

	def typeArgs(): FTree = {
		if (isToken(LBRACKET)) {
			next()
			val t = types()
			accept(RBRACKET)
			return FTypeArgs()
		}
		FNon
	}

	def functionArgTypes(): FTree = {
		var t = FNon
		if ((t = infixType()) != FNon) {
			return FFunctionArgTypes()
		}
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
			return FFunctionArgTypes()
		}
		FNon
	}

	def paramType(): FTree = {
		var t = FNon
		if (isToken(FAT_ARROW)) {
			next()
			t = _type()
		} else {
			val t = _type()
			if (isToken(ID)) ident() //STAR
		}
		FParamType(t)
	}

	def classParam(): FTree = {
		val mods = modifiers()
		if (isTokenOneOf(VAR, VAL) && isTokenLaOneOf(1, ID)) {
			val varVal = token.kind
			next()
			val id = ident()
			accept(COLON)
			val pt = paramType()
			var exp = FNon
			if (token.kind == EQ) {
				next()
				exp = expr()
			}
			return FClassParam(mods, varVal, id, pt, exp)
		}
		FNon
	}

	def typeParamClause(): FTree = {
		if (isToken(LBRACKET)) {
			next()
			val defs = ArrayBuffer[FTree]()
			defs.append(variantTypeParam())
			while (token.kind == COMMA) {
				next()
				defs.append(variantTypeParam())
			}
			accept(RBRACKET)
			return FTypeParamClause(defs.toList)
		}
		FNon
	}

	def typeParam(): FTree = {
		if (isTokenOneOf(ID, UNDERSCORE)) {
			val tp = FTypeParam(ident())
			tp.tpc = typeParamClause()
			if (isToken(LOWER_BOUND)) {
				next()
				tp.lowerB = _type()
			}
			if (isToken(UPPER_BOUND)) {
				next()
				tp.upperB = _type()
			}
			if (isToken(LESS_PERCENT)) {
				next()
				tp.ctxB = _type()
			}
			if (isToken(COLON)) {
				next() //Context bound
				tp.parType = _type()
			}
			return tp
		}
		FNon
	}

	def types(): FTree = {
		val defs = ArrayBuffer[FTree]()
		defs.append(_type())
		while (token.kind == COMMA) {
			next()
			defs.append(_type())
		}
		FTypes(defs)
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

	def variantTypeParam(): FTree = {
		var plusMinus: FIdent = null
		if (isToken(ID)) { //OneOf(PLUS, SUB))
			plusMinus = ident()
		}
		val tp = typeParam()
		if (tp != FNon) {
			return FVariantTypeParam(plusMinus, tp)
		}
		FNon
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
			while (isToken(ID)) { //PIPE
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
		if (literal() || stableId()) {
			simpleExpr1Rest()
			return FTree()
		}
		if (isToken(UNDERSCORE)) {
			next()
			simpleExpr1Rest()
			return FTree()
		}
		if (isToken(LPAREN)) {
			next()
			if (!isToken(RPAREN)) {
				exprs()
			}
			accept(RPAREN)
			simpleExpr1Rest()
			return FTree()
		}
		if (simpleExpr()) {
			if (isToken(DOT)) {
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
		if (isTokenPrefix(UNDERSCORE, DOT)) {
			skip(2)
			ident()
			simpleExpr1Rest()
			return FTree()
		}
		if (isTokenPrefix(UNDERSCORE, LBRACKET)) {
			next()
			typeArgs()
			simpleExpr1Rest()
			return FTree()
		}
		if (isToken(DOT)) {
			next()
			ident()
			simpleExpr1Rest()
			return FTree()
		}
		if (isToken(LBRACKET)) {
			typeArgs()
			simpleExpr1Rest()
			return FTree()
		}
		if (argumentExprs()) {
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
		if (simpleExpr1Rest2()) {
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
		if (typeArgs()) {
			return FTree()
		}
		FNon
	}

	def prefixExpr(): FTree = {
		if (isToken(ID)) { //SUB, PLUS, BANG, TILDE
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
		if (isToken(ID)) { //SUB, PLUS, TILDE, BANG
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
			if (isToken(UNDERSCORE)) {
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
			if (isTokenPrefix(UNDERSCORE, ID)) { // STAR
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
			if (isTokenOneOf(COLON, UNDERSCORE, ID)) { //STAR
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
		var t = FNon
		if ((t = pattern2()) != FNon) {
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
		}
		FNon
	}

	def ids(): FTree = {
		if (isToken(ID)) {
			val defs = ArrayBuffer[FTree]()
			defs.append(ident())
			while (isToken(COMMA)) {
				next()
				defs.append(ident())
			}
			return FIds(defs.toList)
		}
		FNon
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

	def typeDcl(): FTree = {
		if (isToken(ID)) {
			val id = ident()
			val tpc = typeParamClause()
			var lowerB = FNon
			if (isToken(LOWER_BOUND)) {
				next()
				lowerB = _type()
			}
			var upperB = FNon
			if (isToken(UPPER_BOUND)) {
				next()
				upperB = _type()
			}
			return FTypeDcl(id, tpc, lowerB, upperB)
		}
		FNon
	}

	def typeDef(): FTree = {
		if (isToken(ID)) {
			val id = ident()
			val tpc = typeParamClause()
			accept(EQ)
			val t = _type()
			return FTypeDef(id, tpc, t)
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


	def valDcl(): FTree = {
		val is = ids()
		if (is != FNon) {
			accept(EQ)
			val t = _type()
			return FValDcl(is, t)
		}
		FNon
	}

	def varDcl(): FTree = {
		val is = ids()
		if (is != FNon) {
			accept(EQ)
			val t = _type()
			return FVarDcl(is, t)
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
			next()
			return valDcl()
		}

		if (isToken(VAR)) {
			next()
			return varDcl()
		}

		if (isToken(DEF)) {
			next()
			return funDcl()
		}

		if (isToken(TYPE)) {
			next()
			return typeDcl()
		}
		FNon
	}

	def funDcl(): FTree = {
		val f = funSig()
		if (f != FNon) {
			var t = FNon
			if (isToken(EQ)) {
				accept()
				t = _type()
			}
			return FFunDcl(f, t)
		}
		FNon
	}

	def funDef(): FTree = {
		if (isToken(THIS)) {
			next()
			val pc = paramClause()
			val pcs = paramClauses()
			if (isToken(EQ)) {
				val ce = constrExpr()
				return FFunDef(pc, pcs, ce)
			}
			val cb = constrBlock()
			return FFunDef(pc, pcs, cb)
		}

		val fs = funSig()
		if (fs) {
			if (isToken(LCURL)) {
				val b = block()
				accept(RCURL)
				return FFunDef(fs, b)
			}

			var ty = FNon
			if (isToken(COLON)) {
				next()
				ty = _type()
			}

			accept(EQ)
			val ex = expr()
			return FFunDef(fs, ty, ex)
		}

		FNon
	}

	def _def(): FTree = {
		var t = FNon
		if ((t = patVarDef()) != FNon) {
			return t
		}

		if (isToken(DEF)) {
			next()
			funDef()
		}

		//		if ((t=funDclDef())!=FNon) {
		//			return t
		//		}

		if ((t = typeDclDef()) != FNon) {
			return t
		}

		if ((t = tmplDef()) != FNon) {
			return t
		}
		FNon
	}

	def block(): FTree = {
		val defs = ArrayBuffer[FTree]()
		var b = blockStat()
		while (b != FNon) {
			defs.append(b)
			b = blockStat()
		}
		if (defs.size > 0) {
			val re = resultExpr()
			return FBlock(defs.toList, re)
		}
		FNon
	}

	def modifiers(): FTree = {
		val ms = 0
		var m = modifier()
		while (m != 0) {
			ms |= m
			m = modifier()
		}
		if (ms > 0) {
			return FModifiers(ms)
		}
		FNon
	}

	def modifier(): Int = {
		var m = localModifier()
		if(m == 0){
			m = accessModifier()
		}
		if(m == 0 && isToken(OVERRIDE)){
			next()
			m = BFlags.Modifiers.OVERRIDE
		}
		m
	}

	def localModifier(): Int = {
		val m = token.kind match {
			case ABSTRACT => BFlags.Modifiers.ABSTRACT
			case FINAL => BFlags.Modifiers.FINAL
			case SEALED => BFlags.Modifiers.SEALED
			case IMPLICIT => BFlags.Modifiers.IMPLICIT
			case LAZY => BFlags.Modifiers.LAZY
			case _ => 0
		}
		if(m > 0)	next()
		m
	}

	def accessModifier(): Int = {
		val m = token.kind match {
			case PRIVATE => BFlags.Modifiers.PRIVATE 
			case PROTECTED => BFlags.Modifiers.PROTECTED
			case _ => 0
		}
		if(m > 0)	{
			next()
			m |= accessQualifier()
		}
		m
	}

	def blockStat(): FTree = {
		var t = _import()
		if (t != FNon) {
			return t
		}

		if (localModifier()) {
			return tmplDef()
		}

		if (isTokenOneOf(IMPLICIT, LAZY)) {
			next()
			return _def()
		}

		t = _def()
		if (t != FNon) {
			return t
		}

		expr1()
	}

	def templateStat(): FTree = {
		var t: FTree = FNon
		if ((t = _import()) != FNon) {
			return t
		}
		if ((t = modifiers()) != FNon) {
			return defDcl()
		}
		if ((t = defDcl()) != FNon) {
			return t
		}
		if ((t = expr()) != FNon) {
			return t
		}
		FNon
	}

	def literal(): FTree = {
		if (isToken(ID) && isTokenLaOneOf(1, INTLITERAL, FLOATLITERAL)) {
			skip(2)
			return FLiteral()
		} else if (isTokenOneOf(INTLITERAL, FLOATLITERAL, STRINGLITERAL, CHARLITERAL, BOOLEANLITERAL, NULL)) {
			next()
			return FLiteral()
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
			return FClassParamClause()
		}
		FNon
	}

	def classParams(): FTree = {
		val t = classParam()
		if (t != FNon) {
			val defs = ArrayBuffer[FTree]()
			defs.append(t)
			while (isToken(COMMA)) {
				next()
				defs.append(classParam())
			}
			return FClassParams(defs.toList)
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
			return FClassParamClauses()
		}
		if (classParamClause()) {
			while (classParamClause()) {}
			classParamClausesRest()
			return FClassParamClauses()
		}
		FNon
	}



	def templateBody(): FTree = {
		if (isToken(LCURL)) {
			next()
			selfType()
			while (!isToken(RCURL)) {
				templateStat() //+
			}
			accept(RCURL)
			return FTemplateBody()
		}
		FNon
	}

	def earlyDef(): FTree = {
		val ms = modifiers()
		val pvd = patVarDef()
		if (pvd != FNon) {
			return FEarlyDef(ms, pvd)
		}
		FNon
	}
	
	def earlyDefs(): FTree = {
		if(isToken(LCURL)){
			next()
			val defs = ArrayBuffer[FTree]()
			while(!isToken(RCURL)){
				defs.append(earlyDef())
			}
			accept(RCURL)
			accept(WITH)
			FEarlyDefs(defs.toList)
		}
		FNon
	}

	def classTemplate(): FTree = {
		val ed = earlyDefs()
		classParents()
		templateBody()
	}


	def classTemplateOpt(): FTree = {
		var ct = FNon
		if (isToken(EXTENDS)) {
			next()
			ct = classTemplate()
			if(ct == FNon){
				ct = templateBody()
			}
		} else {
			ct = templateBody()
		}  
		ct
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
			val id = ident()
			val tpc = typeParamClause()
			val am = accessModifier()
			val tpcs = classParamClauses()
			val ct = classTemplateOpt()
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

	def accessQualifier(): Int = {
		if (isToken(LBRACKET)) {
			next()
			val aq = token.kind match {
				case ID => BFlags.Modifiers.ACCSSQUAL_ID
				case THIS => BFlags.Modifiers.ACCSSQUAL_THIS
				case _ => 0
			}
			if(aq > 0) next()
			accept(RBRACKET)
			return aq
		}
		0
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

		var t: FTree = FNon
		if ((t != traitDef()) != FNon) {
			return t
		}

		if ((t = objectDef(isCase)) != FNon) {
			return t
		}

		if ((t = classDef(isCase)) != FNon) {
			return t
		}

		FNon
	}

	def topStatement(): FTree = {
		val i = _import()
		if (i != FNon) {
			return i
		}
		val mfs = modifiers()
		val td = tmplDef()
		FTopStmt(mfs, td)
	}

	def topStatements(): ArrayBuffer[FTree] = {
		val defs = ArrayBuffer[FTree]()
		while (token.kind != EOF) {
			defs.append(topStatement())
		}
		defs
	}

	def compilationUnit(): FCompilationUnit = {
		val pkgs = ArrayBuffer[FTree]()
		var p = _package()
		while (p != FNon) {
			pkgs.append(p)
			p = _package()
		}
		val top = topStatements()
		FCompilationUnit(pkgs.toList, top.toList)
	}
}

